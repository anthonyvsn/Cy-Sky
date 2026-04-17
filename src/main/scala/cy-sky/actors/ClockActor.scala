package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.protocol.ControlTowerCommand
import cysky.protocol.ControlTowerCommand.Tick
import java.time.LocalDateTime
import scala.concurrent.duration.FiniteDuration

object ClockActor {

  sealed trait ClockCommand
  private case object TimerFired extends ClockCommand
  case object Stop               extends ClockCommand

  private case object TickKey

  /**
   * @param towerRef     Acteur ControlTower qui reçoit les ticks
   * @param startTime    Heure de départ de la simulation
   * @param tickInterval Intervalle réel entre deux ticks (ex: 50.millis)
   * @param simStep      Pas de temps simulé par tick (ex: 1 minute)
   * @param endTime      Heure de fin — le clock s'arrête proprement quand
   *                     le temps simulé dépasse cette valeur
   */
  def apply(
    towerRef:     ActorRef[ControlTowerCommand],
    startTime:    LocalDateTime,
    tickInterval: FiniteDuration,
    simStep:      java.time.Duration = java.time.Duration.ofMinutes(1),
    endTime:      LocalDateTime
  ): Behavior[ClockCommand] =
    Behaviors.withTimers { timers =>
      timers.startTimerWithFixedDelay(TickKey, TimerFired, tickInterval)
      running(towerRef, startTime, simStep, endTime)
    }

  private def running(
    towerRef: ActorRef[ControlTowerCommand],
    now:      LocalDateTime,
    simStep:  java.time.Duration,
    endTime:  LocalDateTime
  ): Behavior[ClockCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case TimerFired =>
          val next = now.plus(simStep)
          if (!next.isBefore(endTime)) {
            // Dernier tick puis arrêt propre
            towerRef ! Tick(next)
            ctx.log.info(s"[Clock] Fin de journée simulée — $next")
            Behaviors.stopped
          } else {
            towerRef ! Tick(next)
            running(towerRef, next, simStep, endTime)
          }

        case Stop =>
          ctx.log.info("[Clock] Arrêt forcé de l'horloge")
          Behaviors.stopped
      }
    }
}
