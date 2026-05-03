package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, TimerScheduler}
import cysky.SimState
import cysky.protocol.ControlTowerCommand
import cysky.protocol.ControlTowerCommand.Tick
import java.time.LocalDateTime
import scala.concurrent.duration._

/***
 * Classe de l'horloge de la simulation.
 * 
 * A noter : Cette classe est un singleton (1 seule instance possible).
 *           C'est comme "static class" en java.
 */
object ClockActor {

  sealed trait ClockCommand
    // sealed : permet au compilateur de connaitre toutes les sous-classes (qui doivent etre dans le meme fichier source).
    //  -> utile dans l'usage de "match case" au sein des méthodes car
    //     le compilateur exige que tous les cas soient couverts (ici : TimerFired et Stop).
  private case object TimerFired extends ClockCommand   // Cas 1 : horloge continue de tourner
  case object Stop               extends ClockCommand   // Cas 2 : fin de l'horloge

  private case object TickKey

  /**
   * Lance l'horloge de la simulation.
   * Méthode principale de [[ClockActor]].
   *
   * @param towerRef     référence de l'acteur [[TowerControlActor]] qui reçoit les ticks (destinataire des messages)
   * @param startTime    heure de départ de la simulation.
   * @param tickInterval intervalle réel entre deux ticks (ex: 50.millis).
   * @param simStep      le pas du temps simulé par tick (ex: 1 minute).
   * @param endTime      heure de fin de la simulation (le clock s'arrête proprement quand le temps simulé dépasse cette valeur).
   *
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[ClockCommand]].
   */
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

  /***
   * Gère l'horloge pendant la simulation
   * @param towerRef      référence de l'acteur [[TowerControlActor]] qui reçoit les ticks (destinataire des messages)
   * @param now           heure actuelle dans la simulation.
   * @param simStep       le pas du temps simulé par tick (ex: 1 minute).
   * @param endTime       heure de fin de la simulation (le clock s'arrête proprement quand le temps simulé dépasse cette valeur).
   * 
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[ClockCommand]].
   */
  private def running(
    towerRef: ActorRef[ControlTowerCommand],
    now:      LocalDateTime,
    simStep:  java.time.Duration,
    endTime:  LocalDateTime,
    timers:   TimerScheduler[ClockCommand]
  ): Behavior[ClockCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        // Cas 1 : horloge continue
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
        // Cas 2 : horloge s'arrete
        case Stop =>
          ctx.log.info("[Clock] Arrêt forcé de l'horloge")
          timers.cancelAll()
          Behaviors.stopped
      }
    }
}
