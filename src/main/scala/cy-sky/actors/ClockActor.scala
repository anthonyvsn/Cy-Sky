package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, TimerScheduler}
import cysky.SimState
import cysky.protocol.ControlTowerCommand
import cysky.protocol.ControlTowerCommand.Tick
import java.time.LocalDateTime
import scala.concurrent.duration._

object ClockActor {

  sealed trait ClockCommand
  private case object TimerFired extends ClockCommand
  case object Stop               extends ClockCommand

  private case object TickKey

  def apply(
    towerRef:     ActorRef[ControlTowerCommand],
    startTime:    LocalDateTime,
    tickInterval: FiniteDuration,
    simStep:      java.time.Duration = java.time.Duration.ofMinutes(1),
    endTime:      LocalDateTime
  ): Behavior[ClockCommand] =
    Behaviors.withTimers { timers =>
      timers.startSingleTimer(TickKey, TimerFired, tickInterval)
      running(towerRef, startTime, simStep, endTime, timers)
    }

  private def running(
    towerRef: ActorRef[ControlTowerCommand],
    now:      LocalDateTime,
    simStep:  java.time.Duration,
    endTime:  LocalDateTime,
    timers:   TimerScheduler[ClockCommand]
  ): Behavior[ClockCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case TimerFired =>
          val next = now.plus(simStep)
          if (!next.isBefore(endTime)) {
            towerRef ! Tick(next)
            ctx.log.info(s"[Clock] Fin de journée simulée — $next")
            Behaviors.stopped
          } else {
            towerRef ! Tick(next)
            val delay = SimState.getTickInterval.millis
            timers.startSingleTimer(TickKey, TimerFired, delay)
            running(towerRef, next, simStep, endTime, timers)
          }

        case Stop =>
          ctx.log.info("[Clock] Arrêt forcé de l'horloge")
          timers.cancelAll()
          Behaviors.stopped
      }
    }
}
