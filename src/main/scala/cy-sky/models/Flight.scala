package cysky.models

import java.time.LocalTime

final case class Flight(
  flightId     : String,
  airplaneId   : String,
  runway       : String,
  scheduledTime: LocalTime,
  destination  : String,
  kind         : FlightKind
) {
  override def toString: String =
    s"Flight[$flightId | $airplaneId | $kind | $scheduledTime on $runway -> $destination]"
}

object Flight {
  implicit val orderByTime: Ordering[Flight] =
    Ordering.by(_.scheduledTime)
}