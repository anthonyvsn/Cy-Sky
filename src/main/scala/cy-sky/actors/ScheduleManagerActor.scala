package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.models.{AircraftFlight, Arrival, Departure, Landing, Takeoff}
import cysky.petri.PetriScheduleVerifier
import cysky.petri.PetriScheduleVerifier.{VerificationResult, RunwayConflict, GateOverflow, Deadlock}
import cysky.protocol.{ControlTowerCommand, ScheduleManagerCommand}
import cysky.protocol.ControlTowerCommand.{FlightAddedByManager, FlightCancelledByManager, BoomRunway, BoomTaxi, BoomGarage}
import cysky.SimState
import java.time.{LocalDateTime, LocalTime}
import scala.util.Random

// ═══════════════════════════════════════════════════════════════
// ScheduleManagerActor
//
// Mode Libre :
//   Ajoute le vol directement sans replanification.
//   Conflit Pétri → BOOM immédiat.
//
// Mode Contrôle :
//   Pour chaque piste :
//     1. Tenter le placement sans délai → vérifie Pétri.
//     2. En cas de conflit, identifie le(s) vol(s) bloquants
//        (arrivée OU départ qui occupe la ressource) dont
//        l'heure est encore dans le futur (> simTime).
//     3. Teste des décalages par paliers (+15 … +120 min) sur
//        ces vols, re-vérifie chaque fois avec le Pétri.
//     4. Si aucune piste ni aucun délai ne sont viables → annulation.
// ═══════════════════════════════════════════════════════════════
object ScheduleManagerActor {

  sealed trait Mode
  case object Libre    extends Mode
  case object Controle extends Mode

  // ── Paliers de décalage testés (minutes) ─────────────────────
  private val DelaySteps = List(15, 30, 45, 60, 90, 120)

  def apply(
    towerRef    : ActorRef[ControlTowerCommand],
    schedule    : Map[String, List[AircraftFlight]],
    runwayIds   : List[String],
    runwayCount : Int,
    garageCount : Int,
    mode        : Mode = Libre
  ): Behavior[ScheduleManagerCommand] =
    running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, counter = 0, rng = new Random())

  private def running(
    towerRef    : ActorRef[ControlTowerCommand],
    schedule    : Map[String, List[AircraftFlight]],
    runwayIds   : List[String],
    runwayCount : Int,
    garageCount : Int,
    mode        : Mode,
    counter     : Int,
    rng         : Random
  ): Behavior[ScheduleManagerCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case ScheduleManagerCommand.PrepareNewFlight(triggeringEventId, simTime, arrivalHour, arrivalMinute) =>
          val newCounter    = counter + 1
          val arrivalTime   = LocalTime.of(arrivalHour, arrivalMinute)
          val departureTime = arrivalTime.plusHours(1)

          if (arrivalTime.isAfter(LocalTime.of(22, 59))) {
            ctx.log.info(s"[ScheduleManager] Événement $triggeringEventId ignoré — heure cible trop tardive")
            running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng)
          } else {
            val airplaneId = s"SM_PLANE_$newCounter"
            val dest       = randomDestination(rng)

            // Squelettes — la piste sera affectée dans findValidPlacement
            val baseArrival = AircraftFlight(
              flightId      = s"SM_ARR_$newCounter",
              airplaneId    = airplaneId,
              runway        = "",
              scheduledTime = arrivalTime,
              destination   = dest,
              kind          = Arrival
            )
            val baseDep = AircraftFlight(
              flightId      = s"SM_DEP_$newCounter",
              airplaneId    = airplaneId,
              runway        = "",
              scheduledTime = departureTime,
              destination   = dest,
              kind          = Departure
            )

            mode match {

              // ─── Mode Libre : vérification Pétri sans replanification
              //     Conflit → BOOM immédiat (pas de tentative de décalage)
              case Libre =>
                val runway    = runwayIds(rng.nextInt(runwayIds.size))
                val arr       = baseArrival.copy(runway = runway)
                val dep       = baseDep.copy(runway = runway)
                val candidate = addToSchedule(schedule, runway, arr, dep)
                val result    = PetriScheduleVerifier.verify(candidate, runwayCount, garageCount)

                if (result.valid) {
                  ctx.log.info(
                    s"[SM/Libre ✓] SM_ARR_$newCounter/$airplaneId → $runway" +
                    s" arrivée ${fmt(arrivalTime)}, départ ${fmt(departureTime)}"
                  )
                  SimState.setSchedule(candidate)
                  towerRef ! FlightAddedByManager(candidate)
                  running(towerRef, candidate, runwayIds, runwayCount, garageCount, mode, newCounter, rng)
                } else {
                  ctx.log.warn(s"[SM/Libre ✗] Conflit pour SM_ARR_$newCounter → BOOM (mode libre, pas de replanification)")
                  result.issues.foreach(i => ctx.log.warn(s"  ${i.msg}"))
                  sendBoom(towerRef, airplaneId, schedule, result, arr, dep)
                  running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng)
                }

              // ─── Mode Contrôle : placement intelligent ───────────
              case Controle =>
                findValidPlacement(
                  schedule, runwayIds, airplaneId, newCounter,
                  baseArrival, baseDep,
                  runwayCount, garageCount,
                  simTime.toLocalTime
                ) match {

                  case Right((newSchedule, logMsg)) =>
                    ctx.log.info(s"[SM/Controle ✓] $logMsg")
                    SimState.setSchedule(newSchedule)
                    towerRef ! FlightAddedByManager(newSchedule)
                    running(towerRef, newSchedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng)

                  case Left((_, lastRunway)) =>
                    // Toutes les tentatives ARR+DEP ont échoué.
                    // Essayer de placer l'atterrissage seul (DEP annulé — gate occupé).
                    ctx.log.warn(s"[SM/Controle ✗] Aucun placement ARR+DEP viable pour SM_ARR_$newCounter")
                    val arrOnlyResult =
                      runwayIds.view.flatMap { rwy =>
                        val a = baseArrival.copy(runway = rwy)
                        val d = baseDep.copy(runway = rwy)
                        val candidate = addArrivalOnlyToSchedule(schedule, rwy, a)
                        val res = PetriScheduleVerifier.verify(candidate, runwayCount, garageCount)
                        Option.when(res.valid)((candidate, rwy, a, d))
                      }.headOption

                    arrOnlyResult match {
                      case Some((newSched, rwy, arr, dep)) =>
                        ctx.log.warn(s"[SM/Controle ~] ARR seul placé sur $rwy — DEP annulé (gate occupé)")
                        SimState.setSchedule(newSched)
                        towerRef ! FlightCancelledByManager(List(dep), newSched)
                        running(towerRef, newSched, runwayIds, runwayCount, garageCount, mode, newCounter, rng)
                      case None =>
                        ctx.log.warn(s"[SM/Controle ✗] Impossible de placer l'ARR — vol SM_ARR_$newCounter totalement annulé")
                        val arr = baseArrival.copy(runway = lastRunway)
                        val dep = baseDep.copy(runway = lastRunway)
                        towerRef ! FlightCancelledByManager(List(arr, dep), schedule)
                        running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng)
                    }
                }
            }
          }
      }
    }

  // ═══════════════════════════════════════════════════════════════
  // findValidPlacement
  //
  // Parcourt chaque piste :
  //   1. Tente sans délai.
  //   2. Si conflit, identifie les vols bloquants futurs (arrivée
  //      OU départ qui occupent la piste/gate) et teste des
  //      décalages croissants.
  //
  // Retourne :
  //   Right((schedule, logMsg))        placement trouvé
  //   Left((lastResult, lastRunway))   toutes pistes épuisées
  // ═══════════════════════════════════════════════════════════════
  private def findValidPlacement(
    schedule    : Map[String, List[AircraftFlight]],
    runwayIds   : List[String],
    airplaneId  : String,
    counter     : Int,
    baseArrival : AircraftFlight,
    baseDep     : AircraftFlight,
    runwayCount : Int,
    garageCount : Int,
    simTime     : LocalTime
  ): Either[(VerificationResult, String), (Map[String, List[AircraftFlight]], String)] = {

    val found: Option[(Map[String, List[AircraftFlight]], String)] =
      runwayIds.view.flatMap { runway =>
        val arr        = baseArrival.copy(runway = runway)
        val dep        = baseDep.copy(runway = runway)
        val candidate0 = addToSchedule(schedule, runway, arr, dep)
        val result0    = PetriScheduleVerifier.verify(candidate0, runwayCount, garageCount)

        if (result0.valid)
          Some((candidate0,
            s"SM_ARR_$counter/$airplaneId → $runway (sans délai)" +
            s" arrivée ${fmt(arr.scheduledTime)}, départ ${fmt(dep.scheduledTime)}"))
        else {
          val blockingIds = extractBlockingIds(candidate0, result0, airplaneId, simTime)
          if (blockingIds.isEmpty) None
          else {
            val who = blockingIds.mkString(", ")
            DelaySteps.view.flatMap { deltaMin =>
              val rescheduled = blockingIds.foldLeft(schedule)(delayFlight(_, _, deltaMin, simTime))
              val candidate   = addToSchedule(rescheduled, runway, arr, dep)
              val result      = PetriScheduleVerifier.verify(candidate, runwayCount, garageCount)
              Option.when(result.valid)((candidate,
                s"SM_ARR_$counter/$airplaneId → $runway (+${deltaMin}min sur $who)" +
                s" arrivée ${fmt(arr.scheduledTime)}, départ ${fmt(dep.scheduledTime)}"))
            }.headOption
          }
        }
      }.headOption

    found match {
      case Some(result) => Right(result)
      case None         =>
        val lastRunway = runwayIds.last
        val lastResult = PetriScheduleVerifier.verify(
          addToSchedule(schedule, lastRunway,
            baseArrival.copy(runway = lastRunway), baseDep.copy(runway = lastRunway)),
          runwayCount, garageCount
        )
        Left((lastResult, lastRunway))
    }
  }

  // ═══════════════════════════════════════════════════════════════
  // extractBlockingIds
  //
  // Identifie les airplaneIds qui bloquent la ressource au moment
  // du conflit Pétri.
  //
  // IMPORTANT : un conflit de type "LAND" peut être causé par un
  // ATTERRISSAGE en cours OU par un DÉCOLLAGE en cours (qui occupe
  // la même piste). Les deux cas sont traités.
  //
  // Seuls les vols dont l'heure est encore dans le futur (>simTime)
  // sont retournés — on ne peut pas décaler un vol déjà passé.
  // ═══════════════════════════════════════════════════════════════
  private def extractBlockingIds(
    candidate : Map[String, List[AircraftFlight]],
    result    : VerificationResult,
    newId     : String,
    simTime   : LocalTime
  ): List[String] =
    result.issues.flatMap {

      case RunwayConflict(atMin, rwy, _, "LAND") =>
        val flights = candidate.getOrElse(rwy, Nil).filter(_.airplaneId != newId)

        val byLanding: Option[String] = flights
          .filter(f => f.kind == Arrival && f.scheduledTime.isAfter(simTime))
          .find { f =>
            val s = toMin(f.scheduledTime)
            s <= atMin && atMin < s + Landing.durationMinutes
          }
          .map(_.airplaneId)

        val byTakeoff: Option[String] = flights
          .filter(f => f.kind == Departure && f.scheduledTime.isAfter(simTime))
          .find { f =>
            val t = toMin(f.scheduledTime)
            (t - Takeoff.durationMinutes) <= atMin && atMin < (t + Takeoff.durationMinutes)
          }
          .map(_.airplaneId)

        byLanding.orElse(byTakeoff)

      case RunwayConflict(atMin, rwy, _, "TAXI") =>
        val flights = candidate.getOrElse(rwy, Nil).filter(_.airplaneId != newId)

        flights.find { f =>
          val t = toMin(f.scheduledTime)
          f.kind match {
            case Arrival   => t <= atMin && atMin < t + Landing.durationMinutes
            case Departure =>
              val ts = t - Takeoff.durationMinutes
              ts <= atMin && atMin < t + Takeoff.durationMinutes
          }
        }
        .filter(_.scheduledTime.isAfter(simTime))
        .map(_.airplaneId)

      case GateOverflow(atMin, _, _, _) =>
        val allFlights = candidate.values.flatten.toList
        allFlights
          .filter(f => f.kind == Arrival && f.airplaneId != newId)
          .flatMap { arr =>
            allFlights
              .find(d => d.kind == Departure
                      && d.airplaneId == arr.airplaneId
                      && d.scheduledTime.isAfter(simTime))
              .filter { dep =>
                val landEnd = toMin(arr.scheduledTime) + Landing.durationMinutes
                val taxiOut = toMin(dep.scheduledTime) - Takeoff.durationMinutes
                landEnd <= atMin && atMin <= taxiOut
              }
              .map(_ => arr.airplaneId)
          }

      case Deadlock(_, _)             => None
      case RunwayConflict(_, _, _, _) => None
    }.distinct.filterNot(_ == newId)

  // ═══════════════════════════════════════════════════════════════
  // delayFlight
  //
  // Décale les horaires d'un avion dans le schedule.
  // Seuls les vols dont scheduledTime > simTime sont modifiés.
  // ═══════════════════════════════════════════════════════════════
  private def delayFlight(
    schedule  : Map[String, List[AircraftFlight]],
    airplaneId: String,
    deltaMin  : Int,
    simTime   : LocalTime
  ): Map[String, List[AircraftFlight]] =
    schedule.view.mapValues { flights =>
      flights.map { f =>
        if (f.airplaneId == airplaneId && f.scheduledTime.isAfter(simTime))
          f.copy(scheduledTime = f.scheduledTime.plusMinutes(deltaMin))
        else f
      }
    }.toMap

  // ═══════════════════════════════════════════════════════════════
  // sendBoom — détermine le type de BOOM et le transmet à la tour.
  // ═══════════════════════════════════════════════════════════════
  private def sendBoom(
    towerRef   : ActorRef[ControlTowerCommand],
    airplaneId : String,
    schedule   : Map[String, List[AircraftFlight]],
    result     : VerificationResult,
    arrival    : AircraftFlight,
    departure  : AircraftFlight
  ): Unit =
    result.issues.headOption match {

      case Some(RunwayConflict(atMin, rwy, _, "LAND")) =>
        val other = schedule.getOrElse(rwy, Nil)
          .filter(f => f.airplaneId != airplaneId)
          .find { f =>
            val t = toMin(f.scheduledTime)
            f.kind match {
              case Arrival   => t <= atMin && atMin <= t + Landing.durationMinutes
              case Departure =>
                (t - Takeoff.durationMinutes) <= atMin && atMin < (t + Takeoff.durationMinutes)
            }
          }
          .map(_.airplaneId)
        val boomPlanes = (airplaneId :: other.toList).distinct
        towerRef ! BoomRunway(rwy, boomPlanes,
          List(arrival.copy(runway = rwy), departure.copy(runway = rwy)))

      case Some(RunwayConflict(atMin, rwy, _, "TAXI")) =>
        val other = schedule.getOrElse(rwy, Nil)
          .filter(f => f.airplaneId != airplaneId)
          .find { f =>
            val t = toMin(f.scheduledTime)
            f.kind match {
              case Arrival   => t <= atMin && atMin <= t + Landing.durationMinutes
              case Departure =>
                val taxiStart = t - Takeoff.durationMinutes
                taxiStart <= atMin && atMin <= t + Takeoff.durationMinutes
            }
          }
          .map(_.airplaneId)
        val boomPlanes = (airplaneId :: other.toList).distinct
        towerRef ! BoomTaxi(rwy, boomPlanes,
          List(arrival.copy(runway = rwy), departure.copy(runway = rwy)))

      case Some(GateOverflow(_, _, _, _)) | Some(Deadlock(_, _)) | _ =>
        val existing = schedule.values.flatten.toList
          .filter(_.kind == Arrival).map(_.airplaneId).distinct
        val boomPlanes = (airplaneId :: existing).distinct
        towerRef ! BoomGarage(boomPlanes, List(arrival, departure))
    }

  // ── Helpers ───────────────────────────────────────────────────

  private def addToSchedule(
    schedule: Map[String, List[AircraftFlight]],
    runway  : String,
    arr     : AircraftFlight,
    dep     : AircraftFlight
  ): Map[String, List[AircraftFlight]] =
    schedule.updatedWith(runway) {
      case Some(fs) => Some(fs :+ arr :+ dep)
      case None     => Some(List(arr, dep))
    }

  private def addArrivalOnlyToSchedule(
    schedule: Map[String, List[AircraftFlight]],
    runway  : String,
    arr     : AircraftFlight
  ): Map[String, List[AircraftFlight]] =
    schedule.updatedWith(runway) {
      case Some(fs) => Some(fs :+ arr)
      case None     => Some(List(arr))
    }

  private def toMin(t: LocalTime): Int = t.getHour * 60 + t.getMinute

  private val HHmm = java.time.format.DateTimeFormatter.ofPattern("HH:mm")
  private def fmt(t: LocalTime): String = t.format(HHmm)

  private val destinations = Vector(
    "CDG", "JFK", "LHR", "AMS", "FRA", "MAD", "FCO", "IST",
    "DXB", "SIN", "NRT", "LAX", "ORD", "MUC", "BCN"
  )
  private def randomDestination(rng: Random): String =
    destinations(rng.nextInt(destinations.length))
}
