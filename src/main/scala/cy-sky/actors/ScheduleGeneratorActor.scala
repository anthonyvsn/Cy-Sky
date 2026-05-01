package cysky.actors.scheduleGenerator

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import cysky.models.{ ScheduleGeneratorAlgorithm, ScheduleGeneratorProtocol }
import ScheduleGeneratorProtocol._

/***
 * Classe de génération de l'emploi du temps de la journée (Akka Typed).
 * 
 * A noter : Cette classe est un singleton (1 seule instance possible).
 *           C'est comme "static class" en java.
 */
object ScheduleGeneratorActor {
  /***
   * Envoie l'emploi du temps généré.
   * 
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit au protocole [[GenerateSchedule]].
   */
  def apply(): Behavior[GenerateSchedule] = Behaviors.receive { (_, msg) =>
    val schedule = ScheduleGeneratorAlgorithm.generate(
      msg.terminalId,
      msg.runwayCount,
      msg.maxAirplanes,
      msg.seed,
      msg.startTime,
      msg.endTime
    )
    msg.replyTo ! ScheduleGenerated(msg.terminalId, schedule)
    Behaviors.stopped
  }

}