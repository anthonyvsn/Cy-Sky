package cysky.models

/**
 * Représente une opération aéroportuaire avec sa durée simulée associée.
 *
 * Sealed trait = toutes les variantes sont connues à la compilation.
 * Le compilateur te prévient si tu oublies un cas dans un pattern match.
 */
sealed trait Operation {
  /** Durée simulée de l'opération en minutes. */
  def durationMinutes: Int

  /** Durée simulée convertie en millisecondes réelles selon le facteur d'accélération. */
  def realDurationMs(accelerationFactor: Int): Long =
    (durationMinutes * 60 * 1000L) / accelerationFactor
}

// ─── Opérations de piste ────────────────────────────────────────────────────

case object Landing extends Operation {
  val durationMinutes: Int = 10 // approche finale + toucher + freinage
}

case object Takeoff extends Operation {
  val durationMinutes: Int = 5   // alignement + accélération + rotation
}

case object EmergencyLanding extends Operation {
  val durationMinutes: Int = 10
}

case object TaxiToGarage extends Operation {
  val durationMinutes: Int = 8   // piste → garage (libère la piste immédiatement)
}

case object TaxiToRunway extends Operation {
  val durationMinutes: Int = 8   // garage → piste avant décollage
}

// ─── Opérations en garage ───────────────────────────────────────────────────

case object Boarding extends Operation {
  val durationMinutes: Int = 45  // embarquement passagers
}

case object Deboarding extends Operation {
  val durationMinutes: Int = 30  // débarquement passagers
}

case object Refueling extends Operation {
  val durationMinutes: Int = 20  // ravitaillement carburant
}

case object Maintenance extends Operation {
  val durationMinutes: Int = 60  // maintenance légère entre deux vols
}

/**
 * Durée totale au sol = débarquement + ravitaillement + embarquement (en parallèle partiel).
 * On modélise ça comme une opération composite.
 */
case object GroundTurnaround extends Operation {
  // débarquement (30) + ravitaillement overlappé + embarquement (45)
  // en pratique ~60 min pour un turnaround standard
  val durationMinutes: Int =
    Deboarding.durationMinutes + Refueling.durationMinutes + Boarding.durationMinutes
}

// ─── Companion object ────────────────────────────────────────────────────────

object Operation {

  /** Toutes les opérations disponibles — utile pour les stats et le ScheduleGenerator. */
  val allOperation: List[Operation] = List(
    Landing, Takeoff, EmergencyLanding,
    TaxiToGarage, TaxiToRunway,
    Boarding, Deboarding, Refueling, Maintenance, GroundTurnaround
  )

  /**
   * Calcule la durée réelle en ms pour une liste d'opérations séquentielles.
   * Utile pour estimer le temps total d'un cycle complet d'un avion.
   *
   * Exemple :
   *   Operation.totalDurationMs(List(Landing, TaxiToGarage, GroundTurnaround, TaxiToRunway, Takeoff), factor = 1000)
   */
  def totalDurationMinutes(ops: List[Operation]): Int =
    ops.foldLeft(0)(_ + _.durationMinutes)

  def totalDurationMs(ops: List[Operation], accelerationFactor: Int): Long =
    ops.foldLeft(0L)(_ + _.realDurationMs(accelerationFactor))

  /** Cycle complet d'un avion du touchdown au décollage. */
  val fullCycle: List[Operation] =
    List(Landing, TaxiToGarage, GroundTurnaround, TaxiToRunway, Takeoff)

  val fullCycleDurationMinutes: Int = totalDurationMinutes(fullCycle)
  // → 10 + 4 + 95 + 4 + 6 = 119 min ≈ 2h par avion
}