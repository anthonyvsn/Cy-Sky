package cysky

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.actors.{TowerControlActor, ClockActor, ScheduleManagerActor}
import cysky.models.ScheduleGeneratorAlgorithm
import cysky.petri.PetriScheduleVerifier
import java.time.{LocalDate, LocalTime}
import scala.concurrent.Await
import scala.concurrent.duration._

// ═══════════════════════════════════════════════════════════════
// Main — simulation d'une journée aéroport CySky (mode dual)
//
// Deux simulations tournent en parallèle avec le même planning :
//   - gauche  : Mode Libre    (ScheduleManager sans vérif Pétri)
//   - droite  : Mode Contrôle (ScheduleManager avec vérif Pétri)
//
// Calibrage temporel :
//   tickInterval = 112 ms  (temps réel entre deux ticks)
//   simStep      = 1 min   (temps simulé avancé par tick)
//   → journée 06:00 → 23:59 (1 079 ticks) ≈ 2 minutes réelles
// ═══════════════════════════════════════════════════════════════
object Main extends App {

  // ── Paramètres ───────────────────────────────────────────────
  val RUNWAY_COUNT  = 2
  val GARAGE_COUNT  = 5
  val MAX_AIRPLANES = 8
  val SCHEDULE_SEED = 42L
  val SIM_START     = LocalTime.of(6, 0)
  val SIM_END       = LocalTime.of(23, 59)
  val SIM_DATE      = LocalDate.now()

  val TICK_INTERVAL: FiniteDuration     = 112.millis
  val SIM_STEP:      java.time.Duration = java.time.Duration.ofMinutes(1)

  // ── Génération du planning (partagé entre les deux sims) ─────
  val schedule = ScheduleGeneratorAlgorithm.generate(
    terminalId   = "T1",
    runwayCount  = RUNWAY_COUNT,
    maxAirplanes = MAX_AIRPLANES,
    seed         = SCHEDULE_SEED,
    startTime    = SIM_START,
    endTime      = SIM_END
  )

  SimState.setSchedule(schedule)
  val totalFlights = schedule.values.flatten.size / 2
  println(s"Planning généré : $totalFlights vols sur ${schedule.size} piste(s)")
  println(s"Durée simulation réelle estimée : ~${SIM_START.until(SIM_END, java.time.temporal.ChronoUnit.MINUTES) * TICK_INTERVAL.toMillis / 1000} secondes")
  println("─" * 60)

  // ── Vérification Pétri du schedule ───────────────────────────
  println("Vérification réseau de Pétri...")
  val verifResult = PetriScheduleVerifier.verify(schedule, RUNWAY_COUNT, GARAGE_COUNT)
  println(verifResult.report)
  if (!verifResult.valid) {
    println("⚠ Schedule invalide — correction nécessaire avant de lancer la simulation.")
    System.exit(1)
  }
  println("─" * 60)

  // ── Démarrage du serveur HTTP ─────────────────────────────────
  DashboardServer.start()
  println("Dashboard disponible → http://localhost:8080")
  println("En attente du clic Start dans le navigateur...")
  println("─" * 60)

  // ── Attente du clic Start ─────────────────────────────────────
  SimState.startLatch.await()
  println("Simulations démarrées !")
  println("─" * 60)

  // ── Messages du guardian ──────────────────────────────────────
  sealed trait GuardianMsg
  final case class ClockDone(side: String) extends GuardianMsg

  // ── Guardian ─────────────────────────────────────────────────
  val rootBehavior: Behavior[GuardianMsg] = Behaviors.setup { ctx =>

    val simStart = SIM_DATE.atTime(SIM_START)
    val simEnd   = SIM_DATE.atTime(SIM_END)

    // 1. Tour Libre (gauche) — ScheduleManager sans vérification Pétri
    val towerLibre = ctx.spawn(
      TowerControlActor(
        runwayCount = RUNWAY_COUNT,
        garageCount = GARAGE_COUNT,
        schedule    = schedule,
        simDate     = SIM_DATE,
        slot        = SimState.libre,
        mode        = ScheduleManagerActor.Libre
      ),
      "tower-libre"
    )
    val clockLibre = ctx.spawn(
      ClockActor(
        towerRef     = towerLibre,
        startTime    = simStart,
        tickInterval = TICK_INTERVAL,
        simStep      = SIM_STEP,
        endTime      = simEnd
      ),
      "clock-libre"
    )

    // 2. Tour Contrôle (droite) — ScheduleManager avec vérification Pétri
    val towerControle = ctx.spawn(
      TowerControlActor(
        runwayCount = RUNWAY_COUNT,
        garageCount = GARAGE_COUNT,
        schedule    = schedule,
        simDate     = SIM_DATE,
        slot        = SimState.controle,
        mode        = ScheduleManagerActor.Controle
      ),
      "tower-controle"
    )
    val clockControle = ctx.spawn(
      ClockActor(
        towerRef     = towerControle,
        startTime    = simStart,
        tickInterval = TICK_INTERVAL,
        simStep      = SIM_STEP,
        endTime      = simEnd
      ),
      "clock-controle"
    )

    // Observer les deux horloges — chacune envoie un GuardianMsg distinct
    ctx.watchWith(clockLibre,    ClockDone("libre"))
    ctx.watchWith(clockControle, ClockDone("controle"))

    def awaitDone(remaining: Int): Behavior[GuardianMsg] =
      Behaviors.receiveMessage { case ClockDone(side) =>
        println(s"Simulation $side terminée.")
        if (side == "libre")    SimState.libre.markFinished()
        else                    SimState.controle.markFinished()
        if (remaining - 1 <= 0) {
          println("─" * 60)
          println("Toutes les simulations terminées — arrêt du système")
          ctx.system.terminate()
          Behaviors.stopped
        } else awaitDone(remaining - 1)
      }

    awaitDone(2)
  }

  // ── Démarrage et attente de fin propre ────────────────────────
  val system = ActorSystem[GuardianMsg](rootBehavior, "cysky-aerosim")

  Await.result(system.whenTerminated, 10.minutes)
}
