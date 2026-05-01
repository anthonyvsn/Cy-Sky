package cysky

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.actors.{TowerControlActor, ClockActor, ScheduleManagerActor}
import java.time.LocalDate
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._



//Main du projet 
object Main extends App {

  val TICK_INTERVAL: FiniteDuration     = 112.millis // 1 tick = 1 minutes simulé 
  val SIM_STEP:      java.time.Duration = java.time.Duration.ofMinutes(1)
  val SIM_DATE                          = LocalDate.now()

  DashboardServer.start() // démarage du serveur http
  println("Dashboard disponible → http://localhost:8080")
  println("En attente de la configuration dans le navigateur...")
  println("─" * 60)

  // Configuration User ( nbr de pistes, de garage, avions max ect ...)
  SimState.configLatch.await()
  val cfg = SimState.getConfig.get
  println(s"Configuration : ${cfg.runwayCount} piste(s), ${cfg.garageCount} garage(s), ${cfg.maxAirplanes} avions max, graine=${cfg.seed}")
  val totalFlights = SimState.scheduleSnapshot.values.flatten.size / 2
  val simDurSec    = (cfg.startHour until cfg.endHour + 1).size * 60 * TICK_INTERVAL.toMillis / 1000
  println(s"Planning généré : $totalFlights vols — durée réelle estimée : ~${(cfg.endHour * 60 + cfg.endMinute - cfg.startHour * 60 - cfg.startMinute) * TICK_INTERVAL.toMillis / 1000} s")
  println("─" * 60)

  println("En attente du clic Start dans le navigateur...")
  SimState.startLatch.await()
  println("Simulations démarrées !")
  println("─" * 60)

  val schedule = SimState.scheduleSnapshot
  val simStart  = SIM_DATE.atTime(cfg.startHour, cfg.startMinute)
  val simEnd    = SIM_DATE.atTime(cfg.endHour, cfg.endMinute)

  sealed trait GuardianMsg
  final case class ClockDone(side: String) extends GuardianMsg
  val rootBehavior: Behavior[GuardianMsg] = Behaviors.setup { ctx =>

    // 1. Tour de Contrôle en mode libre(gauche) 
    // ScheduleManager sans vérification Pétri
    val towerLibre = ctx.spawn(
      TowerControlActor(
        runwayCount = cfg.runwayCount,
        garageCount = cfg.garageCount,
        schedule    = schedule,
        simDate     = SIM_DATE,
        slot        = SimState.libre,
        mode        = ScheduleManagerActor.Libre // mode Libre sans verif de Petri
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

    // 2. Tour de Contrôle en mode controle (droite) 
    //ScheduleManager avec vérification Pétri
    val towerControle = ctx.spawn(
      TowerControlActor(
        runwayCount = cfg.runwayCount,
        garageCount = cfg.garageCount,
        schedule    = schedule,
        simDate     = SIM_DATE,
        slot        = SimState.controle,
        mode        = ScheduleManagerActor.Controle // mode control avec vérification de petri
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

    // methode qui attend la fin des 2 simulation
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

  val system = ActorSystem[GuardianMsg](rootBehavior, "cysky-aerosim")
  Await.result(system.whenTerminated, Duration.Inf)
}
