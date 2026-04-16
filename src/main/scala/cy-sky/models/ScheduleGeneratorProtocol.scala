package cysky.models

import akka.actor.typed.ActorRef
import cysky.models.AircraftFlight
import java.time.LocalTime

object ScheduleGeneratorProtocol {

  final case class GenerateSchedule(
    terminalId  : String,
    runwayCount : Int,
    maxAirplanes: Int,
    seed        : Long,
    startTime   : LocalTime,
    endTime     : LocalTime,
    replyTo     : ActorRef[ScheduleGenerated]
  )

  final case class ScheduleGenerated(
    terminalId: String,
    schedule  : Map[String, List[AircraftFlight]]
  )

}