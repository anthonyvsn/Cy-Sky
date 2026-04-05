package cysky.models

sealed trait FlightKind
case object Arrival   extends FlightKind
case object Departure extends FlightKind