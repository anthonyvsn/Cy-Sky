package cysky.models

sealed trait Operation {
  def durationMinutes: Int
  def realDurationMs(accelerationFactor: Int): Long =
    (durationMinutes * 60 * 1000L) / accelerationFactor
}

case object Landing extends Operation {
  val durationMinutes: Int = 10
}

case object Takeoff extends Operation {
  val durationMinutes: Int = 5
}

case object EmergencyLanding extends Operation {
  val durationMinutes: Int = 10
}

case object TaxiToGarage extends Operation {
  val durationMinutes: Int = 8
}

case object TaxiToRunway extends Operation {
  val durationMinutes: Int = 8
}

case object Boarding extends Operation {
  val durationMinutes: Int = 45
}

case object Deboarding extends Operation {
  val durationMinutes: Int = 30
}

case object Refueling extends Operation {
  val durationMinutes: Int = 20
}

case object Maintenance extends Operation {
  val durationMinutes: Int = 60
}

case object GroundTurnaround extends Operation {
  val durationMinutes: Int =
    Deboarding.durationMinutes + Refueling.durationMinutes + Boarding.durationMinutes
}

object Operation {
  val allOperation: List[Operation] = List(
    Landing, Takeoff, EmergencyLanding,
    TaxiToGarage, TaxiToRunway,
    Boarding, Deboarding, Refueling, Maintenance, GroundTurnaround
  )

  def totalDurationMinutes(ops: List[Operation]): Int =
    ops.foldLeft(0)(_ + _.durationMinutes)

  def totalDurationMs(ops: List[Operation], accelerationFactor: Int): Long =
    ops.foldLeft(0L)(_ + _.realDurationMs(accelerationFactor))

  val fullCycle: List[Operation] =
    List(Landing, TaxiToGarage, GroundTurnaround, TaxiToRunway, Takeoff)

  val fullCycleDurationMinutes: Int = totalDurationMinutes(fullCycle)
}
