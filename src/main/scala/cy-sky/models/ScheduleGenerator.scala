package cysky.models

import java.time.LocalTime
import scala.util.Random

// ─── Data types ───────────────────────────────────────────────────────────────

final case class Schedule(
  flights     : List[Flight],
  byGarage    : Map[String, List[Flight]],
  byRunway    : Map[String, List[Flight]],
  runwayCount : Int,
  garageCount : Int,
  dayStart    : LocalTime,
  dayEnd      : LocalTime
)

sealed trait ScheduleValidation
final case class ScheduleValid(
  message     : String,
  garageRates : Map[String, Double],
  runwayRates : Map[String, Double],
  avgRate     : Double
) extends ScheduleValidation {
  override def toString: String =
    s"ScheduleValid($message, avgGarage=${String.format(java.util.Locale.US, "%.1f", avgRate * 100)}%)"
}
final case class ScheduleInvalid(reason: String) extends ScheduleValidation

final case class GlobalEvent(
  operation: Operation,
  time     : LocalTime,
  runway   : String,
  gate     : String,
  flight   : Flight
)

// ─── Generator ────────────────────────────────────────────────────────────────

class ScheduleGenerator private (
  runways              : Int,
  garages              : Int,
  start                : LocalTime,
  end                  : LocalTime,
  acceleration         : Int,
  maxGroundStayMinutes : Int,
  minAvgGarageOccupancy: Double,
  seed                 : Option[Long]
) {

  private val rng: Random = seed.fold(new Random)(s => new Random(s))

  private val airlineCodes = Vector(
    "AF","BA","LH","KL","IB","AZ","TK","EK","SQ","NH",
    "CX","DL","UA","AA","AC","QR","EY","FR","VY","SK"
  )
  private def randomRegId(): String = {
    val alpha = "ABCDEFGHJKLMNPQRSTUVWXYZ"
    s"${alpha(rng.nextInt(alpha.length))}${10 + rng.nextInt(90)}${alpha(rng.nextInt(alpha.length))}${10 + rng.nextInt(90)}"
  }

  private val destinations = Vector(
    "CDG", "JFK", "LHR", "AMS", "FRA", "MAD", "FCO", "IST",
    "DXB", "SIN", "NRT", "LAX", "ORD", "MUC", "BCN", "ATL",
    "PEK", "HKG", "DFW", "SYD"
  )

  private def randomDest(): String = destinations(rng.nextInt(destinations.length))
  private def toTime(m: Int): LocalTime = LocalTime.of((m / 60) % 24, m % 60)
  private def toMin(t: LocalTime): Int  = t.getHour * 60 + t.getMinute

  def generate(): Schedule = {
    val startMin = toMin(start)
    val endMin   = toMin(end)

    val minCycle =
      Landing.durationMinutes + TaxiToGarage.durationMinutes +
      Boarding.durationMinutes + TaxiToRunway.durationMinutes + Takeoff.durationMinutes

    val runwayIds = (1 to runways).map(i => s"RWY_$i").toVector
    val gateIds   = (1 to garages).map(j => s"GATE_$j").toVector

    // Génère les vols d'une piste, thread l'état (flightC, gateC) entre appels.
    // Retourne (vols générés, flightC final, gateC final).
    @scala.annotation.tailrec
    def loopRunway(
      currentMin: Int,
      flightC:    Int,
      gateC:      Int,
      runwayId:   String,
      acc:        List[Flight]
    ): (List[Flight], Int, Int) =
      if (currentMin + minCycle > endMin) (acc, flightC, gateC)
      else {
        val idx      = flightC + 1
        val arrMin   = currentMin
        val boardMin = arrMin + Landing.durationMinutes + TaxiToGarage.durationMinutes
        val baseStay = TaxiToGarage.durationMinutes + Boarding.durationMinutes + TaxiToRunway.durationMinutes
        val maxExtra = Math.min(maxGroundStayMinutes - baseStay, endMin - arrMin - minCycle).max(0)
        val extra    = if (maxExtra > 0) rng.nextInt(maxExtra) else 0
        val depMin   = boardMin + Boarding.durationMinutes + TaxiToRunway.durationMinutes + extra
        val jitter   = rng.nextInt(10)
        val nextMin  = depMin + Takeoff.durationMinutes + jitter

        if (depMin + Takeoff.durationMinutes <= endMin) {
          val gate   = gateIds(gateC % garages)
          val depRwy = runwayIds(rng.nextInt(runways))
          val flight = Flight(
            flightId        = s"${airlineCodes((idx - 1) % airlineCodes.length)}${f"$idx%03d"}",
            airplaneId      = randomRegId(),
            arrivalTime     = toTime(arrMin),
            arrivalRunway   = runwayId,
            departureRunway = depRwy,
            departureTime   = toTime(depMin),
            destination     = randomDest(),
            boardingTime    = toTime(boardMin),
            gateId          = gate
          )
          loopRunway(nextMin, idx, gateC + 1, runwayId, acc :+ flight)
        } else {
          loopRunway(nextMin, idx, gateC, runwayId, acc)
        }
      }

    // Fold sur les pistes pour thread l'état (gateC, flightC, accVols) entre pistes
    val (_, _, allFlights) = runwayIds.foldLeft((0, 0, List.empty[Flight])) {
      case ((gateC, flightC, acc), runwayId) =>
        val (flights, newFlightC, newGateC) = loopRunway(startMin, flightC, gateC, runwayId, Nil)
        (newGateC, newFlightC, acc ++ flights)
    }

    val sorted   = allFlights.sortBy(f => toMin(f.arrivalTime))
    val byGarage = sorted.groupBy(_.gateId)
    val byRunway = sorted.groupBy(_.arrivalRunway)

    Schedule(
      flights     = sorted,
      byGarage    = byGarage,
      byRunway    = byRunway,
      runwayCount = runways,
      garageCount = garages,
      dayStart    = start,
      dayEnd      = end
    )
  }

  def validate(s: Schedule): ScheduleValidation = {
    val startMin = toMin(start)
    val endMin   = toMin(end)
    val dayMin   = endMin - startMin

    if (dayMin <= 0) return ScheduleInvalid("Plage horaire invalide")

    val allGateIds   = (1 to garages).map(j => s"GATE_$j").toList
    val allRunwayIds = (1 to runways).map(i => s"RWY_$i").toList

    val garageRates: Map[String, Double] = allGateIds.map { gid =>
      val intervals = s.byGarage.getOrElse(gid, Nil).flatMap { f =>
        val inMin  = toMin(f.arrivalTime)   + Landing.durationMinutes + TaxiToGarage.durationMinutes
        val outMin = toMin(f.departureTime) - TaxiToRunway.durationMinutes
        if (outMin > inMin) Some((inMin.max(startMin), outMin.min(endMin))) else None
      }.sortBy(_._1)
      gid -> (unionCoverage(intervals).toDouble / dayMin)
    }.toMap

    val runwayRates: Map[String, Double] = allRunwayIds.map { rid =>
      val arrs = s.byRunway.getOrElse(rid, Nil).map { f =>
        val a = toMin(f.arrivalTime)
        (a.max(startMin), (a + Landing.durationMinutes).min(endMin))
      }
      val deps = s.flights.filter(_.departureRunway == rid).map { f =>
        val d = toMin(f.departureTime) - Takeoff.durationMinutes
        (d.max(startMin), toMin(f.departureTime).min(endMin))
      }
      val all = (arrs ++ deps).filter { case (a, b) => b > a }.sortBy(_._1)
      rid -> (unionCoverage(all).toDouble / dayMin)
    }.toMap

    val avgRate = if (garageRates.nonEmpty) garageRates.values.sum / garageRates.size else 0.0
    val pct     = String.format(java.util.Locale.US, "%.0f", avgRate * 100)

    ScheduleValid(
      s"${s.flights.size} vols — occupation garage moy. ${pct}%",
      garageRates,
      runwayRates,
      avgRate
    )
  }

  def globalSchedule(s: Schedule): List[GlobalEvent] = {
    val landings = s.flights.map(f => GlobalEvent(Landing, f.arrivalTime,   f.arrivalRunway,   f.gateId, f))
    val takeoffs = s.flights.map(f => GlobalEvent(Takeoff, f.departureTime, f.departureRunway, f.gateId, f))
    (landings ++ takeoffs).sortBy(e => toMin(e.time))
  }

  private def unionCoverage(sorted: List[(Int, Int)]): Int =
    sorted.foldLeft((0, Int.MinValue)) { case ((total, lastEnd), (a, b)) =>
      if (a >= lastEnd) (total + b - a, b)
      else              (total + Math.max(0, b - lastEnd), Math.max(lastEnd, b))
    }._1
}

object ScheduleGenerator {
  def default(
    runways              : Int,
    garages              : Int,
    start                : LocalTime,
    end                  : LocalTime,
    acceleration         : Int          = 1000,
    maxGroundStayMinutes : Int          = 360,
    minAvgGarageOccupancy: Double       = 0.60,
    seed                 : Option[Long] = None
  ): ScheduleGenerator =
    new ScheduleGenerator(runways, garages, start, end,
      acceleration, maxGroundStayMinutes, minAvgGarageOccupancy, seed)
}
