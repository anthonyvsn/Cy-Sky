package cysky.actors.scheduleGenerator

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import cysky.models.{ ScheduleGeneratorAlgorithm, ScheduleGeneratorProtocol }
import ScheduleGeneratorProtocol._

object ScheduleGeneratorActor {

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