package cysky

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.actors.{TowerControlActor, ClockActor, ScheduleManagerActor}
import java.time.LocalDate
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

// ═══════════════════════════════════════════════════════════════
// Main — simulation d'une journée aéroport CySky (mode dual)
//
// Deux simulations tournent en parallèle avec le même planning :
//   - gauche  : Mode Libre    (ScheduleManager sans vérif Pétri)
//   - droite  : Mode Contrôle (ScheduleManager avec vérif Pétri)
//
// Calibrage temporel :
//   tickInterval = 112 ms  (temps réel entre deux ticks, ajustable via UI)
//   simStep      = 1 min   (temps simulé avancé par tick)
//   → journée 06:00 → 23:59 (1 079 ticks) ≈ 2 minutes réelles
// ═══════════════════════════════════════════════════════════════
object Main extends App {

  val TICK_INTERVAL: FiniteDuration     = 112.millis
  val SIM_STEP:      java.time.Duration = java.time.Duration.ofMinutes(1)
  val SIM_DATE                          = LocalDate.now()

  // ── Démarrage du serveur HTTP (avant la configuration) ───────
  DashboardServer.start()
  println("Dashboard disponible → http://localhost:8080")
  println("En attente de la configuration dans le navigateur...")
  println("─" * 60)

  // ── Attente de la configuration utilisateur ──────────────────
  SimState.configLatch.await()
  val cfg = SimState.getConfig.get
  println(s"Configuration : ${cfg.runwayCount} piste(s), ${cfg.garageCount} garage(s), ${cfg.maxAirplanes} avions max, graine=${cfg.seed}")
  val totalFlights = SimState.scheduleSnapshot.values.flatten.size / 2
  val simDurSec    = (cfg.startHour until cfg.endHour + 1).size * 60 * TICK_INTERVAL.toMillis / 1000
  println(s"Planning généré : $totalFlights vols — durée réelle estimée : ~${(cfg.endHour * 60 + cfg.endMinute - cfg.startHour * 60 - cfg.startMinute) * TICK_INTERVAL.toMillis / 1000} s")
  println("─" * 60)

  // ── Attente du clic Start ─────────────────────────────────────
  println("En attente du clic Start dans le navigateur...")
  SimState.startLatch.await()
  println("Simulations démarrées !")
  println("─" * 60)

  val schedule = SimState.scheduleSnapshot
  val simStart  = SIM_DATE.atTime(cfg.startHour, cfg.startMinute)
  val simEnd    = SIM_DATE.atTime(cfg.endHour, cfg.endMinute)

  // ── Messages du guardian ──────────────────────────────────────
  sealed trait GuardianMsg
  final case class ClockDone(side: String) extends GuardianMsg

  // ── Guardian ─────────────────────────────────────────────────
  val rootBehavior: Behavior[GuardianMsg] = Behaviors.setup { ctx =>

    // 1. Tour Libre (gauche) — ScheduleManager sans vérification Pétri
    val towerLibre = ctx.spawn(
      TowerControlActor(
        runwayCount = cfg.runwayCount,
        garageCount = cfg.garageCount,
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
        runwayCount = cfg.runwayCount,
        garageCount = cfg.garageCount,
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
  Await.result(system.whenTerminated, Duration.Inf)
}
