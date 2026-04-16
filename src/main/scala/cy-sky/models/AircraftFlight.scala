package cysky.models

import java.time.LocalTime

/** Vol individuel (arrivée OU départ) — utilisé par le système d'acteurs. */
final case class AircraftFlight(
  flightId     : String,
  airplaneId   : String,
  runway       : String,
  scheduledTime: LocalTime,
  destination  : String,
  kind         : FlightKind
) {
  override def toString: String =
    s"AircraftFlight[$flightId | $airplaneId | $kind | $scheduledTime on $runway -> $destination]"
}

object AircraftFlight {
  implicit val orderByTime: Ordering[AircraftFlight] =
    Ordering.by(_.scheduledTime)
}
