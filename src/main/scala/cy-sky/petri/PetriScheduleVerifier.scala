package cysky.petri

import cysky.models.{AircraftFlight, Arrival, Departure, Landing, TaxiToGarage, TaxiToRunway, Takeoff}
import java.time.LocalTime

// ═══════════════════════════════════════════════════════════════
// PetriScheduleVerifier — vérifie la correction d'un schedule
// via un réseau de Pétri formalisé.
//
// Modèle Pétri de l'aéroport :
//   Places ressources :
//     - "rwy_{id}" : 1 jeton = piste libre (0 = occupée)
//     - "gates"    : nGarages jetons (capacité totale des gates)
//
//   Transitions (4 par avion, dans l'ordre chronologique) :
//     1. land_start_k  : consomme 1 jeton "rwy_{arrRwy}"
//     2. land_end_k    : rend   1 jeton "rwy_{arrRwy}",
//                        consomme 1 jeton "gates"
//     3. taxi_out_k    : rend   1 jeton "gates",
//                        consomme 1 jeton "rwy_{depRwy}"
//     4. takeoff_end_k : rend   1 jeton "rwy_{depRwy}"
//
//   Deadlock = une transition requise est désactivée (marquage 0)
//              quand elle devrait franchir.
// ═══════════════════════════════════════════════════════════════
object PetriScheduleVerifier {

  // ─── Types de résultats ────────────────────────────────────────

  sealed trait Issue {
    def atMin: Int
    def msg:   String
    def timeStr: String = f"${atMin / 60}%02d:${atMin % 60}%02d"
  }

  /** phase = "LAND" (atterrissage) ou "TAXI" (départ / taxi-out) */
  final case class RunwayConflict(atMin: Int, runway: String, flightId: String, phase: String = "LAND") extends Issue {
    val msg = s"[CONFLIT PISTE] $timeStr — piste $runway occupée pour $flightId (phase: $phase)"
  }

  final case class GateOverflow(atMin: Int, needed: Int, capacity: Int, flightId: String) extends Issue {
    val msg = s"[DÉBORDEMENT GATES] $timeStr — $needed avions au sol, capacité $capacity (vol $flightId)"
  }

  final case class Deadlock(atMin: Int, desc: String) extends Issue {
    val msg = s"[DEADLOCK] $timeStr — $desc"
  }

  final case class VerificationResult(
    valid   : Boolean,
    issues  : List[Issue],
    petriNet: PetriModule
  ) {
    def report: String = {
      val header = if (valid) "✓ Schedule valide — aucun deadlock détecté."
                   else       s"✗ ${issues.size} problème(s) détecté(s) :"
      val lines  = issues.map("  " + _.msg)
      (header :: lines).mkString("\n")
    }
  }

  // ─── API publique ──────────────────────────────────────────────

  def verify(
    schedule    : Map[String, List[AircraftFlight]],
    runwayCount : Int,
    garageCount : Int
  ): VerificationResult = {

    val net    = buildPetriNet(runwayCount, garageCount)
    val pairs  = extractPairs(schedule)
    val events = buildEvents(pairs).sortBy(_.atMin)

    // Marquage initial : chaque piste libre + gates dispo
    val runwayIds = (1 to runwayCount).map(i => s"RWY_$i").toList
    var marking: Map[String, Int] =
      runwayIds.map(_ -> 1).toMap + ("gates" -> garageCount)

    var issues = List.empty[Issue]

    for (e <- events) {
      e.kind match {

        // 1. Atterrissage commence → consomme 1 jeton piste
        case "LAND_START" =>
          val rwy = e.runway
          if (marking.getOrElse(rwy, 0) < 1)
            issues = RunwayConflict(e.atMin, rwy, e.flightId, "LAND") :: issues
          else
            marking = marking.updated(rwy, marking(rwy) - 1)

        // 2. Atterrissage terminé → rend piste, consomme 1 gate
        case "LAND_END" =>
          val rwy   = e.runway
          val gates = marking.getOrElse("gates", 0)
          marking = marking.updated(rwy, marking.getOrElse(rwy, 0) + 1)
          if (gates < 1)
            issues = GateOverflow(e.atMin, garageCount - gates + 1, garageCount, e.flightId) :: issues
          else
            marking = marking.updated("gates", gates - 1)

        // 3. Taxi vers piste → rend 1 gate, consomme 1 jeton piste départ
        case "TAXI_OUT" =>
          val rwy = e.runway
          marking = marking.updated("gates", marking.getOrElse("gates", 0) + 1)
          if (marking.getOrElse(rwy, 0) < 1)
            issues = RunwayConflict(e.atMin, rwy, e.flightId, "TAXI") :: issues
          else
            marking = marking.updated(rwy, marking(rwy) - 1)

        // 4. Décollage terminé → rend 1 jeton piste
        case "TAKEOFF_END" =>
          val rwy = e.runway
          marking = marking.updated(rwy, marking.getOrElse(rwy, 0) + 1)

        case _ =>
      }

      // Vérification invariant : aucun marquage négatif
      marking.foreach { case (place, tokens) =>
        if (tokens < 0)
          issues = Deadlock(e.atMin, s"Marquage négatif sur '$place' ($tokens) après ${e.kind} de ${e.flightId}") :: issues
      }
    }

    // Vérification finale : toutes les ressources doivent être revenues
    val finalRunwaysOk = runwayIds.forall(r => marking.getOrElse(r, 0) == 1)
    val finalGatesOk   = marking.getOrElse("gates", 0) == garageCount
    if (!finalRunwaysOk || !finalGatesOk)
      issues = Deadlock(24 * 60, s"Ressources non libérées en fin de journée : $marking") :: issues

    VerificationResult(issues.isEmpty, issues.reverse, net)
  }

  // ─── Construction du réseau de Pétri ──────────────────────────

  def buildPetriNet(runwayCount: Int, garageCount: Int): PetriModule = {
    // Module de base : une piste (2 places : libre / occupée, 2 transitions : use/free)
    def runwayModule(id: String): PetriModule = PetriModule(
      places      = Vector(s"${id}_free", s"${id}_busy"),
      transitions = Vector(s"use_$id", s"free_$id"),
      pre         = Vector(
        Vector(1, 0),  // ${id}_free  : use consomme 1
        Vector(0, 1)   // ${id}_busy  : free consomme 1
      ),
      post        = Vector(
        Vector(0, 1),  // ${id}_free  : free produit 1
        Vector(1, 0)   // ${id}_busy  : use produit 1
      ),
      marking     = Vector(1, 0)  // libre au départ
    )

    // Composer les modules piste en bloc diagonal
    val runways = (1 to runwayCount)
      .map(i => runwayModule(s"RWY_$i"))
      .reduce(PetriComposer.blockDiag)

    // Ajouter la place "gates" (pool de jetons = capacité)
    PetriComposer.addPlace(runways, "gates", garageCount)
  }

  // ─── Helpers internes ─────────────────────────────────────────

  private case class SimEvent(
    atMin   : Int,
    flightId: String,
    kind    : String,   // LAND_START | LAND_END | TAXI_OUT | TAKEOFF_END
    runway  : String
  )

  private def toMin(t: LocalTime): Int = t.getHour * 60 + t.getMinute

  private def extractPairs(
    schedule: Map[String, List[AircraftFlight]]
  ): List[(AircraftFlight, AircraftFlight)] = {
    val allFlights = schedule.values.flatten.toList
    allFlights
      .filter(_.kind == Arrival)
      .flatMap { arr =>
        allFlights
          .find(f => f.kind == Departure && f.airplaneId == arr.airplaneId)
          .map(dep => (arr, dep))
      }
  }

  private def buildEvents(pairs: List[(AircraftFlight, AircraftFlight)]): List[SimEvent] =
    pairs.flatMap { case (arr, dep) =>
      val arrMin = toMin(arr.scheduledTime)
      val depMin = toMin(dep.scheduledTime)
      List(
        // 1. L'avion commence à atterrir → piste arrêtée
        SimEvent(arrMin,
                 arr.flightId, "LAND_START", arr.runway),
        // 2. Atterrissage terminé → piste libérée, gate consommé
        SimEvent(arrMin + Landing.durationMinutes,
                 arr.flightId, "LAND_END",   arr.runway),
        // 3. Départ du gate → gate libéré, piste de décollage consommée
        SimEvent(depMin - Takeoff.durationMinutes,
                 dep.flightId, "TAXI_OUT",   dep.runway),
        // 4. Décollage terminé → piste libérée
        SimEvent(depMin + Takeoff.durationMinutes,
                 dep.flightId, "TAKEOFF_END", dep.runway)
      )
    }
}
