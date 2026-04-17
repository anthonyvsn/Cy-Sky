package cysky.models

import java.time.LocalTime

/** Vol complet (arrivée + départ) — utilisé par le générateur de planning et le front. */
final case class Flight(
  flightId       : String,
  airplaneId     : String,
  arrivalTime    : LocalTime,
  arrivalRunway  : String,
  departureRunway: String,
  departureTime  : LocalTime,
  destination    : String,
  boardingTime   : LocalTime,
  gateId         : String
)

object Flight {
  implicit val orderByArrival: Ordering[Flight] =
    Ordering.by(_.arrivalTime)
}