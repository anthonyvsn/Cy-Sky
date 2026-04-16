package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.models.{AircraftFlight, Arrival, Departure, Landing, Takeoff}
import cysky.petri.PetriScheduleVerifier
import cysky.petri.PetriScheduleVerifier.{RunwayConflict, GateOverflow, Deadlock}
import cysky.protocol.{ControlTowerCommand, ScheduleManagerCommand}
import cysky.protocol.ControlTowerCommand.{FlightAddedByManager, BoomRunway, BoomTaxi, BoomGarage}
import cysky.SimState
import java.time.{LocalTime}
import scala.util.Random

// ═══════════════════════════════════════════════════════════════
// ScheduleManagerActor
//
// Mode Controle :
//   Vérifie le schedule candidat contre le réseau de Pétri.
//   En cas de conflit, identifie les 2 avions précis en cause
//   et envoie la commande BOOM appropriée à la TowerControl.
//
//   • RunwayConflict(LAND) → BoomRunway(runway, [smPlane, existingPlane])
//   • RunwayConflict(TAXI) → BoomTaxi  (runway, [smPlane, existingPlane])
//   • GateOverflow/Deadlock→ BoomGarage([smPlane, ...active planes])
// ═══════════════════════════════════════════════════════════════
object ScheduleManagerActor {

  sealed trait Mode
  case object Libre    extends Mode
  case object Controle extends Mode

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

        case ScheduleManagerCommand.PrepareNewFlight(triggeringEventId, _, arrivalHour, arrivalMinute) =>
          val newCounter    = counter + 1
          val runway        = runwayIds(rng.nextInt(runwayIds.size))
          val arrivalTime   = LocalTime.of(arrivalHour, arrivalMinute)
          val departureTime = arrivalTime.plusHours(1)

          if (arrivalTime.isAfter(LocalTime.of(22, 59))) {
            ctx.log.info(s"[ScheduleManager] Événement $triggeringEventId ignoré — heure cible trop tardive")
            running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng)
          } else {
            val airplaneId = s"SM_PLANE_$newCounter"
            val dest       = randomDestination(rng)

            val arrival = AircraftFlight(
              flightId      = s"SM_ARR_$newCounter",
              airplaneId    = airplaneId,
              runway        = runway,
              scheduledTime = arrivalTime,
              destination   = dest,
              kind          = Arrival
            )
            val departure = AircraftFlight(
              flightId      = s"SM_DEP_$newCounter",
              airplaneId    = airplaneId,
              runway        = runway,
              scheduledTime = departureTime,
              destination   = dest,
              kind          = Departure
            )

            val candidate: Map[String, List[AircraftFlight]] =
              schedule.updatedWith(runway) {
                case Some(fs) => Some(fs :+ arrival :+ departure)
                case None     => Some(List(arrival, departure))
              }

            mode match {

              case Libre =>
                ctx.log.info(
                  s"[ScheduleManager] [LIBRE] SM_ARR_$newCounter/$airplaneId sur $runway" +
                  s" — arrivée ${f(arrivalTime)}, départ ${f(departureTime)}"
                )
                SimState.setSchedule(candidate)
                towerRef ! FlightAddedByManager(candidate)
                running(towerRef, candidate, runwayIds, runwayCount, garageCount, mode, newCounter, rng)

              case Controle =>
                val result = PetriScheduleVerifier.verify(candidate, runwayCount, garageCount)

                if (result.valid) {
                  ctx.log.info(
                    s"[ScheduleManager] [CONTROLE ✓] SM_ARR_$newCounter/$airplaneId sur $runway" +
                    s" — arrivée ${f(arrivalTime)}, départ ${f(departureTime)}"
                  )
                  SimState.setSchedule(candidate)
                  towerRef ! FlightAddedByManager(candidate)
                  running(towerRef, candidate, runwayIds, runwayCount, garageCount, mode, newCounter, rng)

                } else {
                  ctx.log.warn(s"[ScheduleManager] [CONTROLE ✗] Conflit pour SM_ARR_$newCounter — BOOM")
                  result.issues.foreach(i => ctx.log.warn(s"  ${i.msg}"))

                  result.issues.headOption match {

                    case Some(RunwayConflict(atMin, rwy, _, "LAND")) =>
                      // Trouver l'avion existant dont la fenêtre d'atterrissage couvre atMin
                      val existing = findLandingConflict(schedule, rwy, atMin)
                      val boomPlanes = (airplaneId :: existing.toList).distinct
                      ctx.log.warn(s"[ScheduleManager] 💥 BOOM PISTE $rwy — ${boomPlanes.mkString(", ")}")
                      towerRef ! BoomRunway(rwy, boomPlanes)

                    case Some(RunwayConflict(atMin, rwy, _, "TAXI")) =>
                      // Trouver l'avion existant dont la fenêtre de taxi couvre atMin
                      val existing = findTaxiConflict(schedule, rwy, atMin)
                      val boomPlanes = (airplaneId :: existing.toList).distinct
                      ctx.log.warn(s"[ScheduleManager] 💥 BOOM TAXI $rwy — ${boomPlanes.mkString(", ")}")
                      towerRef ! BoomTaxi(rwy, boomPlanes)

                    case Some(GateOverflow(_, _, _, _)) | Some(Deadlock(_, _)) | _ =>
                      val existing = schedule.values.flatten.toList
                        .filter(_.kind == Arrival).map(_.airplaneId).distinct
                      val boomPlanes = (airplaneId :: existing).distinct
                      ctx.log.warn(s"[ScheduleManager] 💥 BOOM GARAGE")
                      towerRef ! BoomGarage(boomPlanes)
                  }

                  // Schedule inchangé — vol SM rejeté
                  running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng)
                }
            }
          }
      }
    }

  // ── Helpers de détection du conflit ──────────────────────────

  /** Trouve l'avion existant sur `runway` dont la fenêtre d'atterrissage
   *  (LAND_START..LAND_END) contient `atMin`. */
  private def findLandingConflict(
    schedule: Map[String, List[AircraftFlight]],
    runway:   String,
    atMin:    Int
  ): Option[String] =
    schedule.getOrElse(runway, Nil)
      .filter(_.kind == Arrival)
      .find { f =>
        val start = toMin(f.scheduledTime)
        start <= atMin && atMin < start + Landing.durationMinutes
      }
      .map(_.airplaneId)

  /** Trouve l'avion existant sur `runway` dont la fenêtre taxi+décollage
   *  (TAXI_OUT..TAKEOFF_END) contient `atMin`. */
  private def findTaxiConflict(
    schedule: Map[String, List[AircraftFlight]],
    runway:   String,
    atMin:    Int
  ): Option[String] =
    schedule.getOrElse(runway, Nil)
      .filter(_.kind == Departure)
      .find { f =>
        val depMin = toMin(f.scheduledTime)
        val taxiStart = depMin - Takeoff.durationMinutes
        taxiStart <= atMin && atMin < depMin + Takeoff.durationMinutes
      }
      .map(_.airplaneId)

  private def toMin(t: LocalTime): Int = t.getHour * 60 + t.getMinute

  private val HHmm = java.time.format.DateTimeFormatter.ofPattern("HH:mm")
  private def f(t: LocalTime): String = t.format(HHmm)

  private val destinations = Vector(
    "CDG", "JFK", "LHR", "AMS", "FRA", "MAD", "FCO", "IST",
    "DXB", "SIN", "NRT", "LAX", "ORD", "MUC", "BCN"
  )
  private def randomDestination(rng: Random): String =
    destinations(rng.nextInt(destinations.length))
}
