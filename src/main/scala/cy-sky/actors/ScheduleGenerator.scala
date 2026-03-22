package cysky.actors

import cysky.models._
import java.time.LocalTime
import scala.annotation.tailrec
import scala.util.Random

// ─── Schedule ────────────────────────────────────────────────────────────────

final case class Schedule(
  flights     : List[Flight],
  byGarage    : Map[String, List[Flight]],
  byRunway    : Map[String, List[Flight]],
  runwayCount : Int,
  garageCount : Int,
  dayStart    : LocalTime,
  dayEnd      : LocalTime
) {
  val maxCapacity: Int = runwayCount + garageCount
  val sortedByArrival: List[Flight] = flights.sorted(Flight.orderByArrival)
}

// ─── Résultat de validation ───────────────────────────────────────────────────

sealed trait ValidationResult

final case class ScheduleValid(
  totalFlights  : Int,
  garageRates   : Map[String, Double],
  runwayRates   : Map[String, Double],
  avgGarageRate : Double
) extends ValidationResult {
  override def toString: String = {
    val gr = garageRates.toList.sortBy(_._1).map { case (g, r) => s"$g=${(r*100).toInt}%" }.mkString(", ")
    val rr = runwayRates.toList.sortBy(_._1).map { case (r, v) => s"$r=${(v*100).toInt}%" }.mkString(", ")
    s"✓ Schedule valide | $totalFlights vols\n" +
    s"  Garages : $gr | Moy: ${(avgGarageRate*100).toInt}%\n" +
    s"  Pistes  : $rr (informatif)"
  }
}

final case class ScheduleInvalid(violations: List[String]) extends ValidationResult {
  override def toString: String =
    s"✗ Schedule invalide :\n" + violations.map(v => s"  - $v").mkString("\n")
}

// ─── AirportEvent ─────────────────────────────────────────────────────────────

final case class AirportEvent(
  time      : LocalTime,
  flight    : Flight,
  operation : Operation,
  runway    : String,
  gate      : String
)

// ─── ScheduleGenerator ───────────────────────────────────────────────────────

final class ScheduleGenerator(
  val runwayCount           : Int,
  val garageCount           : Int,
  val dayStart              : LocalTime,
  val dayEnd                : LocalTime,
  val accelerationFactor    : Int,
  val maxGroundStayMinutes  : Int    = 360,
  val minAvgGarageOccupancy : Double = 0.60,
  val seed                  : Option[Long] = None
) {

  // ─── Constantes ────────────────────────────────────────────────────────────

  private val dayDurationMinutes: Int = {
    val s = dayStart.toSecondOfDay / 60
    val e = dayEnd.toSecondOfDay   / 60
    if (e > s) e - s else (24 * 60 - s) + e
  }

  private val maxCapacity    : Int = runwayCount + garageCount
  private val arrToGarage    : Int = Landing.durationMinutes + TaxiToGarage.durationMinutes   // 18
  private val garageToTakeoff: Int = TaxiToRunway.durationMinutes + Takeoff.durationMinutes   // 13
  private val minStay        : Int = GroundTurnaround.durationMinutes                          // 95
  private val minFullCycle   : Int = arrToGarage + minStay + garageToTakeoff                   // 126
  private val shiftWindow    : Int = 60

  // Séjour cible 2h, max 3h — garantit ~6 rotations par garage/jour
  private val targetStay      : Int = 120
  private val stayVariance    : Int = 15
  private val effectiveMaxStay: Int = 180

  private val runwayIds   : Vector[String] = (1 to runwayCount).map(i => s"RWY_$i").toVector
  private val garageIds   : Vector[String] = (1 to garageCount).map(i => s"GATE_$i").toVector
  private val destinations: Vector[String] =
    Vector("CDG", "JFK", "LHR", "AMS", "FRA", "MAD", "BCN", "FCO", "DXB", "IST")

  private def toTime(offset: Int): LocalTime = dayStart.plusMinutes(offset.toLong)

  private def toOffset(t: LocalTime): Int = {
    val tMin = t.toSecondOfDay / 60
    val sMin = dayStart.toSecondOfDay / 60
    if (tMin >= sMin) tMin - sMin else (24 * 60 - sMin) + tMin
  }

  // ─── Point d'entrée ────────────────────────────────────────────────────────

  def generate(): Schedule = {
    val rng = new Random(seed.getOrElse(System.currentTimeMillis()))

    // Phase 1 : garage-first + assignation pistes sans conflit
    val phase1 = generateAllFlights(rng)

    // Phase 2 : corriger les chevauchements garage résiduels
    val phase2 = fixGarageOverlaps(phase1, rng)

    // Phase 3 : remplir si occupation < 60%
    val phase3 = fillGarages(phase2, rng)

    // Phase finale : supprimer les vols encore en conflit
    val clean = removeConflicts(phase3)
    buildSchedule(clean)
  }

  def globalSchedule(s: Schedule): List[AirportEvent] =
    s.flights.flatMap { f => List(
      AirportEvent(f.arrivalTime,   f, Landing, f.arrivalRunway,   f.gateId),
      AirportEvent(f.departureTime, f, Takeoff,  f.departureRunway, f.gateId)
    )}.sortBy(e => toOffset(e.time))

  def validate(s: Schedule): ValidationResult = {
    val v = checkGarageOverlaps(s) ++ checkRunwayConflicts(s) ++
            checkNoDepartureAfterEnd(s) ++ checkCapacityViolations(s)
    if (v.isEmpty) {
      val gr = computeGarageRates(s)
      val rr = computeRunwayRates(s)
      ScheduleValid(s.flights.size, gr, rr, gr.values.sum / gr.size)
    } else ScheduleInvalid(v)
  }

  // ─── Phase 1 : génération garage-first ────────────────────────────────────

  private def generateAllFlights(rng: Random): List[Flight] = {
    // Générer les vols par garage (séquence sans chevauchement par construction)
    val raw = garageIds.flatMap(gid => generateGarageSchedule(gid, rng)).toList
    // Assigner les pistes sans conflit
    assignRunways(raw)
  }

  private def generateGarageSchedule(gateId: String, rng: Random): List[Flight] = {
    @tailrec
    def loop(arrOffset: Int, idx: Int, acc: List[Flight]): List[Flight] = {
      val stay            = randomStay(arrOffset, rng)
      val gateLeaveOffset = arrOffset + arrToGarage + stay   // avion quitte la gate
      val depOffset       = gateLeaveOffset + garageToTakeoff // décollage terminé
      if (depOffset > dayDurationMinutes) acc.reverse
      else {
        val boardingOffset = (gateLeaveOffset - Boarding.durationMinutes).max(0)
        val f = Flight(
          flightId        = s"${gateId}_F${String.format("%03d", idx)}",
          airplaneId      = s"PLANE_${gateId}_${String.format("%03d", idx)}",
          arrivalTime     = toTime(arrOffset),
          arrivalRunway   = "TBD",
          departureRunway = "TBD",
          departureTime   = toTime(depOffset),
          destination     = pickRandom(destinations, rng),
          boardingTime    = toTime(boardingOffset),
          gateId          = gateId
        )
        // Suivant arrive quand la gate est libre
        loop(gateLeaveOffset, idx + 1, f :: acc)
      }
    }
    loop(0, 1, Nil)
  }

  // ─── Assignation pistes sans conflit ──────────────────────────────────────

  /**
   * UN seul freeAt partagé pour toutes les opérations (ARR + DEP).
   * Toutes les opérations sont triées chronologiquement.
   * En cas d'égalité de disponibilité, on alterne les pistes (round-robin)
   * pour équilibrer la charge entre RWY_1 et RWY_2.
   *
   * Résultat garanti sans conflit par construction.
   */
  private def assignRunways(flights: List[Flight]): List[Flight] = {
    // Pool unique partagé ARR + DEP — une piste est une ressource physique unique.
    // Opérations triées chronologiquement. Round-robin en cas d'égalité.
    // Le départ garde son heure ORIGINALE (pas de propagation du shift arrivée).
    val freeAt = Array.fill(runwayCount)(0)
    var rr     = 0

    def pick(wanted: Int, dur: Int): (Int, Int) = {
      val earliest   = freeAt.map(f => math.max(f, wanted)).min
      val candidates = freeAt.zipWithIndex.filter { case (f,_) => math.max(f, wanted) == earliest }
      val (_, ri)    = candidates(rr % candidates.length)
      rr += 1
      val start      = math.max(freeAt(ri), wanted)
      freeAt(ri)     = start + dur
      (ri, start)
    }

    sealed trait K
    case object A extends K; case object D extends K
    case class Op(offset: Int, dur: Int, fid: String, k: K)

    // Trier toutes les ops chronologiquement
    // Pour les DEP : on utilise l'heure ORIGINALE (pas décalée par l'arrivée)
    val ops = (
      flights.map(f => Op(toOffset(f.arrivalTime),   Landing.durationMinutes, f.flightId, A)) ++
      flights.map(f => Op(toOffset(f.departureTime), Takeoff.durationMinutes,  f.flightId, D))
    ).sortBy(o => (o.offset, o.k match { case D => 0; case _ => 1 }))
    // Les DEP ont priorité sur les ARR au même offset (avion qui décolle d'abord)

    val ar = scala.collection.mutable.Map[String,(Int,Int)]()
    val dr = scala.collection.mutable.Map[String,(Int,Int)]()
    ops.foreach {
      case Op(off, dur, fid, A) => ar(fid) = pick(off, dur)
      case Op(off, dur, fid, D) => dr(fid) = pick(off, dur)
    }

    flights.flatMap { f =>
      val (arrRi, actualArr) = ar.getOrElse(f.flightId, (0, toOffset(f.arrivalTime)))
      val (depRi, actualDep) = dr.getOrElse(f.flightId, (0, toOffset(f.departureTime)))
      if (actualDep > dayDurationMinutes + 10) None
      else {
        val cappedDep   = actualDep.min(dayDurationMinutes)
        val depShift    = cappedDep - toOffset(f.departureTime)
        val newBoarding = (toOffset(f.boardingTime) + depShift).max(0)
        Some(f.copy(
          arrivalTime     = toTime(actualArr),
          arrivalRunway   = runwayIds(arrRi),
          departureTime   = toTime(cappedDep),
          departureRunway = runwayIds(depRi),
          boardingTime    = toTime(newBoarding)
        ))
      }
    }
  }

  // ─── Phase 2 : corriger les chevauchements garage ─────────────────────────

  private def fixGarageOverlaps(flights: List[Flight], rng: Random): List[Flight] = {
    @tailrec
    def fixGate(mine: List[Flight], others: List[Flight]): List[Flight] = {
      val sorted = mine.sortBy(f => toOffset(f.arrivalTime))
      // Gate libre quand l'avion part en taxi = depOffset - garageToTakeoff
      val conflict = sorted.sliding(2).collectFirst {
        case List(f1, f2)
          if (toOffset(f1.departureTime) - garageToTakeoff) > toOffset(f2.arrivalTime) =>
          (f1, f2)
      }
      conflict match {
        case None => sorted ++ others
        case Some((f1, f2)) =>
          val neededStart = toOffset(f1.departureTime) - garageToTakeoff
          val delta       = neededStart - toOffset(f2.arrivalTime)
          if (delta <= shiftWindow && neededStart + minFullCycle <= dayDurationMinutes) {
            val stay    = (toOffset(f2.departureTime) - toOffset(f2.arrivalTime) - arrToGarage - garageToTakeoff).max(minStay)
            val newDep  = neededStart + arrToGarage + stay + garageToTakeoff
            if (newDep > dayDurationMinutes) {
              fixGate(sorted.filterNot(_.flightId == f2.flightId), others)
            } else {
              val newBoarding = (newDep - Boarding.durationMinutes - TaxiToRunway.durationMinutes).max(0)
              val f2Fixed = f2.copy(
                arrivalTime   = toTime(neededStart),
                departureTime = toTime(newDep),
                boardingTime  = toTime(newBoarding)
              )
              fixGate(sorted.map(f => if (f.flightId == f2.flightId) f2Fixed else f), others)
            }
          } else {
            fixGate(sorted.filterNot(_.flightId == f2.flightId), others)
          }
      }
    }

    garageIds.foldLeft(flights) { (current, gid) =>
      val (mine, others) = current.partition(_.gateId == gid)
      fixGate(mine, others)
    }
  }

  // ─── Phase 3 : remplir les garages sous-utilisés ─────────────────────────

  private def fillGarages(flights: List[Flight], rng: Random): List[Flight] = {
    val buf     = scala.collection.mutable.ListBuffer[Flight]() ++= flights
    var fillIdx = 1

    garageIds.foreach { gid =>
      var continue = true
      while (continue) {
        val gFlights = buf.filter(_.gateId == gid).toList.sortBy(f => toOffset(f.arrivalTime))
        if (garageOccRate(gFlights) >= minAvgGarageOccupancy) {
          continue = false
        } else {
          findFreeSlots(gFlights).find { case (s, e) => e - s >= minFullCycle } match {
            case None => continue = false
            case Some((slotStart, _)) =>
              val stay      = randomStay(slotStart, rng)
              val depOffset = slotStart + arrToGarage + stay + garageToTakeoff
              if (depOffset <= dayDurationMinutes) {
                val boarding = (depOffset - Boarding.durationMinutes - TaxiToRunway.durationMinutes).max(0)
                val newF = Flight(
                  flightId        = s"${gid}_FILL_${String.format("%03d", fillIdx)}",
                  airplaneId      = s"PLANE_${gid}_FILL_${String.format("%03d", fillIdx)}",
                  arrivalTime     = toTime(slotStart),
                  arrivalRunway   = "TBD",
                  departureRunway = "TBD",
                  departureTime   = toTime(depOffset),
                  destination     = pickRandom(destinations, rng),
                  boardingTime    = toTime(boarding),
                  gateId          = gid
                )
                buf += newF
                fillIdx += 1
              } else continue = false
          }
        }
      }
    }

    // Assigner les pistes sur tous les vols (existants + FILL)
    // mais restaurer les pistes originales des vols existants
    val existing     = buf.filterNot(_.flightId.contains("FILL")).toList
    val all          = buf.toList
    val allWithRwy   = assignRunways(all)
    // Remettre les pistes originales des vols existants, garder celles des FILL
    val existingRwy  = existing.map(f => f.flightId -> (f.arrivalRunway, f.departureRunway)).toMap
    allWithRwy.map { f =>
      if (f.flightId.contains("FILL")) f
      else existingRwy.get(f.flightId).map { case (arr, dep) =>
        f.copy(arrivalRunway = arr, departureRunway = dep)
      }.getOrElse(f)
    }
  }

  // ─── Helpers ──────────────────────────────────────────────────────────────

  private def findFreeSlots(gFlights: List[Flight]): List[(Int, Int)] = {
    val occupied = gFlights.sortBy(f => toOffset(f.arrivalTime))
      .map(f => (toOffset(f.arrivalTime), toOffset(f.departureTime)))
    val pts = (0 +: occupied.flatMap { case (s, e) => List(s, e) } :+ dayDurationMinutes).sorted.distinct
    pts.sliding(2).flatMap {
      case List(a, b) =>
        val busy = occupied.exists { case (s, e) => a >= s && b <= e }
        if (!busy && b - a >= minFullCycle) Some((a, b)) else None
      case _ => None
    }.toList
  }

  private def garageOccRate(gFlights: List[Flight]): Double = {
    val occ = gFlights.map { f =>
      val in  = toOffset(f.arrivalTime)  + arrToGarage
      val out = toOffset(f.departureTime) - TaxiToRunway.durationMinutes
      (out - in).toDouble.max(0)
    }.sum
    occ / dayDurationMinutes.toDouble
  }

  private def randomStay(arrOffset: Int, rng: Random): Int = {
    val maxPossible = (dayDurationMinutes - arrOffset - arrToGarage - garageToTakeoff)
                        .min(effectiveMaxStay)
    val lo    = (targetStay - stayVariance).max(minStay)
    val hi    = (targetStay + stayVariance).min(maxPossible)
    val range = (hi - lo).max(0)
    if (range == 0) lo else lo + rng.nextInt(range + 1)
  }

  private def pickRandom[A](v: Vector[A], rng: Random): A = v(rng.nextInt(v.size))

  // ─── Phase finale : suppression des conflits résiduels ───────────────────

  /**
   * Supprime les vols responsables de conflits de piste ou chevauchements gate.
   * Itère jusqu'à ce qu'il n'y ait plus aucun conflit.
   */
  @tailrec
  private def removeConflicts(flights: List[Flight]): List[Flight] = {
    // Trouver les flightIds en conflit de piste
    val runwayConflicts: Set[String] = runwayIds.flatMap { rid =>
      val ops = (
        flights.filter(_.arrivalRunway   == rid).map(f => (toOffset(f.arrivalTime),   Landing.durationMinutes, f.flightId)) ++
        flights.filter(_.departureRunway == rid).map(f => (toOffset(f.departureTime), Takeoff.durationMinutes,  f.flightId))
      ).sortBy(_._1)
      ops.sliding(2).flatMap {
        case Seq((s1, d1, id1), (s2, _, id2)) if s2 < s1 + d1 =>
          // Supprimer le 2ème (celui qui arrive en conflit)
          Some(id2)
        case _ => None
      }
    }.toSet

    // Trouver les flightIds en chevauchement de gate
    val gateConflicts: Set[String] = garageIds.flatMap { gid =>
      val fs = flights.filter(_.gateId == gid).sortBy(f => toOffset(f.arrivalTime))
      fs.sliding(2).flatMap {
        case List(f1, f2) if gateLeave(f1) > toOffset(f2.arrivalTime) =>
          Some(f2.flightId) // supprimer le 2ème
        case _ => None
      }
    }.toSet

    val toRemove = runwayConflicts ++ gateConflicts

    if (toRemove.isEmpty) flights
    else {
      println(s"  [CLEAN] Suppression de ${toRemove.size} vol(s) en conflit : ${toRemove.mkString(", ")}")
      removeConflicts(flights.filterNot(f => toRemove.contains(f.flightId)))
    }
  }

  // ─── Construction du Schedule ─────────────────────────────────────────────

  private def buildSchedule(flights: List[Flight]): Schedule = {
    val byGarage = flights
      .groupBy(_.gateId)
      .map { case (k, vs) => k -> vs.sortBy(f => toOffset(f.arrivalTime)) }

    val byRunway = runwayIds.map { rid =>
      val ops = (
        flights.filter(_.arrivalRunway   == rid).map(f => toOffset(f.arrivalTime)   -> f) ++
        flights.filter(_.departureRunway == rid).map(f => toOffset(f.departureTime) -> f)
      ).sortBy(_._1).map(_._2)
      rid -> ops
    }.toMap

    Schedule(flights, byGarage, byRunway, runwayCount, garageCount, dayStart, dayEnd)
  }

  // ─── Validation ───────────────────────────────────────────────────────────

  private def gateLeave(f: Flight): Int =
    toOffset(f.departureTime) - garageToTakeoff

  private def checkGarageOverlaps(s: Schedule): List[String] =
    garageIds.toList.flatMap { gid =>
      val fs = s.flights.filter(_.gateId == gid).sortBy(f => toOffset(f.arrivalTime))
      fs.sliding(2).flatMap {
        case List(f1, f2) if gateLeave(f1) > toOffset(f2.arrivalTime) =>
          Some(s"Chevauchement $gid : ${f1.flightId} libère ${toTime(gateLeave(f1))}, ${f2.flightId} arrive ${f2.arrivalTime}")
        case _ => None
      }.toList
    }

  private def checkRunwayConflicts(s: Schedule): List[String] =
    runwayIds.toList.flatMap { rid =>
      val ops = (
        s.flights.filter(_.arrivalRunway   == rid).map(f => (toOffset(f.arrivalTime),   Landing.durationMinutes, f.flightId)) ++
        s.flights.filter(_.departureRunway == rid).map(f => (toOffset(f.departureTime), Takeoff.durationMinutes,  f.flightId))
      ).sortBy(_._1)
      ops.sliding(2).flatMap {
        case Seq((s1, d1, id1), (s2, _, id2)) if s2 < s1 + d1 =>
          Some(s"Conflit piste $rid : $id1 et $id2 à ${toTime(s1)}")
        case _ => None
      }.toList
    }

  private def checkAvgGarageOccupancy(s: Schedule): List[String] = {
    val avg = computeGarageRates(s).values.sum / garageCount
    if (avg < minAvgGarageOccupancy)
      List(s"Occupation ${(avg*100).toInt}% < minimum ${(minAvgGarageOccupancy*100).toInt}%")
    else Nil
  }

  private def checkNoDepartureAfterEnd(s: Schedule): List[String] =
    s.flights.flatMap { f =>
      if (toOffset(f.departureTime) > dayDurationMinutes)
        Some(s"${f.flightId} dépasse dayEnd : ${f.departureTime}")
      else Nil
    }

  private def checkCapacityViolations(s: Schedule): List[String] =
    (0 until dayDurationMinutes by 5).toList.flatMap { offset =>
      val count = s.flights.count { f =>
        val a = toOffset(f.arrivalTime); val d = toOffset(f.departureTime)
        offset >= a && offset < d
      }
      if (count > s.maxCapacity)
        Some(s"Capacité dépassée à ${toTime(offset)} : $count avions (max $maxCapacity)")
      else None
    }

  private def computeGarageRates(s: Schedule): Map[String, Double] =
    garageIds.map(gid => gid -> garageOccRate(s.flights.filter(_.gateId == gid))).toMap

  private def computeRunwayRates(s: Schedule): Map[String, Double] =
    runwayIds.map { rid =>
      val l = s.flights.count(_.arrivalRunway   == rid) * Landing.durationMinutes
      val t = s.flights.count(_.departureRunway == rid) * Takeoff.durationMinutes
      rid -> (l + t) / dayDurationMinutes.toDouble
    }.toMap
}

object ScheduleGenerator {
  def default(
    runways               : Int,
    garages               : Int,
    start                 : LocalTime    = LocalTime.of(6, 0),
    end                   : LocalTime    = LocalTime.of(23, 59),
    acceleration          : Int          = 1000,
    maxGroundStayMinutes  : Int          = 360,
    minAvgGarageOccupancy : Double       = 0.60,
    seed                  : Option[Long] = None
  ): ScheduleGenerator =
    new ScheduleGenerator(runways, garages, start, end, acceleration,
                          maxGroundStayMinutes, minAvgGarageOccupancy, seed)
}