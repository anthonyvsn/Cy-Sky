package cysky

import akka.Done
import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.actors.{TowerControlActor, ClockActor}
import cysky.models.ScheduleGeneratorAlgorithm
import java.time.{LocalDate, LocalTime}
import scala.concurrent.Await
import scala.concurrent.duration._

// ═══════════════════════════════════════════════════════════════
// Main — simulation d'une journée aéroport CySky
//
// Calibrage temporel :
//   tickInterval = 112 ms  (temps réel entre deux ticks)
//   simStep      = 1 min   (temps simulé avancé par tick)
//   → journée 06:00 → 23:59 (1 079 ticks)
//     ≈ 1 079 × 112 ms ≈ 121 secondes ≈ 2 minutes réelles
//
// Démarrage :
//   1. DashboardServer démarre sur :8080
//   2. Main bloque sur startLatch jusqu'au clic Start dans le navigateur
//   3. Le système d'acteurs est lancé
//
// Terminaison :
//   Le ClockActor s'arrête quand simTime ≥ endTime.
//   Le guardian observe le clock via watchWith(Done)
//   et appelle ctx.system.terminate() — arrêt propre garanti.
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

  // ── Génération du planning ────────────────────────────────────
  val schedule = ScheduleGeneratorAlgorithm.generate(
    terminalId   = "T1",
    runwayCount  = RUNWAY_COUNT,
    maxAirplanes = MAX_AIRPLANES,
    seed         = SCHEDULE_SEED,
    startTime    = SIM_START,
    endTime      = SIM_END
  )

  val totalFlights = schedule.values.flatten.size / 2
  println(s"Planning généré : $totalFlights vols sur ${schedule.size} piste(s)")
  println(s"Durée simulation réelle estimée : ~${SIM_START.until(SIM_END, java.time.temporal.ChronoUnit.MINUTES) * TICK_INTERVAL.toMillis / 1000} secondes")
  println("─" * 60)

  // ── Démarrage du serveur HTTP ─────────────────────────────────
  DashboardServer.start()
  println("Dashboard disponible → http://localhost:8080")
  println("En attente du clic Start dans le navigateur...")
  println("─" * 60)

  // ── Attente du clic Start ─────────────────────────────────────
  SimState.startLatch.await()
  println("Simulation démarrée !")
  println("─" * 60)

  // ── Guardian ─────────────────────────────────────────────────
  val rootBehavior: Behavior[Done] = Behaviors.setup[Done] { ctx =>

    // 1. Tour de contrôle (spawne pistes et garages en interne)
    val towerRef = ctx.spawn(
      TowerControlActor(
        runwayCount = RUNWAY_COUNT,
        garageCount = GARAGE_COUNT,
        schedule    = schedule,
        simDate     = SIM_DATE
      ),
      "tower-control"
    )

    // 2. Horloge simulée
    val simStart = SIM_DATE.atTime(SIM_START)
    val simEnd   = SIM_DATE.atTime(SIM_END)
    val clockRef = ctx.spawn(
      ClockActor(
        towerRef     = towerRef,
        startTime    = simStart,
        tickInterval = TICK_INTERVAL,
        simStep      = SIM_STEP,
        endTime      = simEnd
      ),
      "clock"
    )

    // 3. Observer le clock : quand il s'arrête → Done → terminate
    ctx.watchWith(clockRef, Done)

    Behaviors.receiveMessage { _ =>
      println("─" * 60)
      println("Simulation terminée — arrêt du système")
      SimState.markFinished()
      ctx.system.terminate()
      Behaviors.stopped
    }
  }

  // ── Démarrage et attente de fin propre ────────────────────────
  val system = ActorSystem[Done](rootBehavior, "cysky-aerosim")

  Await.result(system.whenTerminated, 10.minutes)
}
