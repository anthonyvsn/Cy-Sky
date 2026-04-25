package cysky.models

import java.time.LocalTime
import scala.util.Random

object ScheduleGeneratorAlgorithm {

  private case class Slot(startMin: Int, endMin: Int)

  def generate(
    terminalId  : String,
    runwayCount : Int,
    maxAirplanes: Int,
    seed        : Long,
    startTime   : LocalTime,
    endTime     : LocalTime
  ): Map[String, List[AircraftFlight]] = {

    val rng          = new Random(seed)
    val startMin     = startTime.getHour * 60 + startTime.getMinute
    val endMin       = endTime.getHour   * 60 + endTime.getMinute
    val runwayIds    = (1 to runwayCount).map(i => s"RWY_$i").toList
    val landingMin   = Landing.durationMinutes
    val groundMin    = GroundTurnaround.durationMinutes + TaxiToGarage.durationMinutes + TaxiToRunway.durationMinutes
    val takeoffMin   = Takeoff.durationMinutes
    val cycleMin     = landingMin + groundMin + takeoffMin

    def minutesToTime(m: Int): LocalTime =
      LocalTime.of((m / 60) % 24, m % 60)

    def overlaps(a: Slot, b: Slot): Boolean =
      a.startMin < b.endMin && b.startMin < a.endMin

    def airplanesOnGroundAt(t: Int, placed: List[(AircraftFlight, AircraftFlight)]): Int =
      placed.count { case (arr, dep) =>
        arr.scheduledTime.getHour * 60 + arr.scheduledTime.getMinute <= t &&
        dep.scheduledTime.getHour * 60 + dep.scheduledTime.getMinute > t
      }

    // Génère des candidats avec un peu de bruit aléatoire sur l'heure (+/- 0-9 min)
    def candidates: LazyList[Int] = LazyList.unfold(startMin) { cur =>
      val jitter = rng.nextInt(10)
      val next   = cur + cycleMin + jitter
      Option.when(next <= endMin)(cur + jitter, next)
    }

    // Pour chaque piste, on place les vols en vérifiant les 2 contraintes
    // runwaySlots : slots occupés par piste
    // placedPairs : toutes les paires (arrivée, départ) placées toutes pistes confondues
    val (_, allPairs) = runwayIds.foldLeft(
      (Map.empty[String, List[Slot]], List.empty[(String, AircraftFlight, AircraftFlight)])
    ) { case ((runwaySlots, placedPairs), runwayId) =>

      val slotsForThisRunway = runwaySlots.getOrElse(runwayId, List.empty)

      val newPairs = candidates.foldLeft(List.empty[(AircraftFlight, AircraftFlight)]) { case (acc, arrMin) =>
        if (acc.size >= maxAirplanes) acc
        else {
          val depMin       = arrMin + landingMin + groundMin
          val arrSlot      = Slot(arrMin,          arrMin + landingMin)
          val depSlot      = Slot(depMin,           depMin + takeoffMin)
          val allSlots     = slotsForThisRunway ++ acc.flatMap { case (a, d) =>
            val am = a.scheduledTime.getHour * 60 + a.scheduledTime.getMinute
            val dm = d.scheduledTime.getHour * 60 + d.scheduledTime.getMinute
            List(Slot(am, am + landingMin), Slot(dm, dm + takeoffMin))
          }

          val runwayFree   = !allSlots.exists(s => overlaps(s, arrSlot) || overlaps(s, depSlot))
          val allPlaced    = placedPairs.map { case (_, a, d) => (a, d) } ++ acc
          val capOk        = (arrMin until depMin).forall { t =>
            airplanesOnGroundAt(t, allPlaced ++ acc) < maxAirplanes
          }

          if (runwayFree && capOk) {
            val idx        = acc.size + 1
            val airplaneId = s"PLANE_${runwayId}_$idx"
            val dest       = randomDestination(rng)

            val arrival = AircraftFlight(
              flightId        = s"${terminalId}_${runwayId}_ARR$idx",
              airplaneId      = airplaneId,
              runway          = runwayId,
              scheduledTime   = minutesToTime(arrMin),
              destination     = dest,
              kind            = Arrival
            )
            val departure = AircraftFlight(
              flightId        = s"${terminalId}_${runwayId}_DEP$idx",
              airplaneId      = airplaneId,
              runway          = runwayId,
              scheduledTime   = minutesToTime(depMin),
              destination     = dest,
              kind            = Departure
            )
            acc :+ (arrival, departure)
          } else acc
        }
      }

      val newSlots = newPairs.flatMap { case (a, d) =>
        val am = a.scheduledTime.getHour * 60 + a.scheduledTime.getMinute
        val dm = d.scheduledTime.getHour * 60 + d.scheduledTime.getMinute
        List(Slot(am, am + landingMin), Slot(dm, dm + takeoffMin))
      }

      (
        runwaySlots.updated(runwayId, slotsForThisRunway ++ newSlots),
        placedPairs ++ newPairs.map { case (a, d) => (runwayId, a, d) }
      )
    }

    allPairs.groupMap(_._1) { case (_, a, d) => List(a, d) }
      .map { case (rwy, pairs) => rwy -> pairs.flatten[AircraftFlight] }
  }

  private val destinations = Vector(
    "CDG", "JFK", "LHR", "AMS", "FRA", "MAD", "FCO", "IST",
    "DXB", "SIN", "NRT", "LAX", "ORD", "MUC", "BCN"
  )

  private def randomDestination(rng: Random): String =
    destinations(rng.nextInt(destinations.length))
}