package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.model._
import cysky.protocol._
import cysky.protocol.GarageCommand._
import cysky.protocol.ControlTowerCommand.GarageFreed
import cysky.protocol.AirplaneCommand.ParkConfirmed
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// ═══════════════════════════════════════════════════════════════
// GarageActor — Akka Typed, style fonctionnel pur
//
// Deux états : Free / Occupied.
// Correspond aux places P_garage_free_j et P_garage_occupied_j
// dans le réseau de Pétri.
// L'invariant |P_garage_free| + |P_garage_occupied| = M
// est maintenu par la ControlTower, pas par cet acteur.
// ═══════════════════════════════════════════════════════════════
object GarageActor {

  private val HHmm = DateTimeFormatter.ofPattern("HH:mm")
  private def fmt(t: LocalDateTime): String = t.format(HHmm)

  // ─────────────────────────────────────────────
  // État interne immuable
  // ─────────────────────────────────────────────
  final case class GarageData(
    garageId:             String,
    state:                GarageState,
    occupiedBy:           Option[String],
    towerRef:             ActorRef[ControlTowerCommand],
    occupiedSince:        Option[LocalDateTime],
    groundDurationMinutes: Long,
    simTime:              LocalDateTime = LocalDateTime.MIN
  ) {
    def withState(s: GarageState):        GarageData = copy(state = s)
    def withOccupied(id: Option[String]): GarageData = copy(occupiedBy = id)

    /**
     * Temps restant avant que l'avion puisse décoller.
     * Utilisé par le ScheduleManager pour identifier les garages
     * libérables en avance (Plan B du scénario urgence).
     */
    def remainingGroundTime(now: LocalDateTime): Long =
      occupiedSince.map { since =>
        val elapsed = java.time.Duration.between(since, now).toMinutes
        (groundDurationMinutes - elapsed).max(0L)
      }.getOrElse(0L)
  }

  // ─────────────────────────────────────────────
  // Point d'entrée
  // ─────────────────────────────────────────────
  def apply(
    garageId: String,
    towerRef: ActorRef[ControlTowerCommand]
  ): Behavior[GarageCommand] =
    Behaviors.setup { ctx =>
      ctx.log.info(s"[Garage $garageId] Initialisé — état: Free")
      val data = GarageData(
        garageId              = garageId,
        state                 = GarageState.Free,
        occupiedBy            = None,
        towerRef              = towerRef,
        occupiedSince         = None,
        groundDurationMinutes = 0L
      )
      free(data)
    }

  // ─────────────────────────────────────────────
  // État : Free — garage disponible
  // P_garage_free_j contient 1 jeton
  // Transition T_park_j franchissable
  // ─────────────────────────────────────────────
  private def free(data: GarageData): Behavior[GarageCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case ParkRequest(airplaneId, airplaneRef) =>
          ctx.log.info(s"[Garage ${data.garageId} ${fmt(data.simTime)}] $airplaneId stationné")
          val next = data
            .withState(GarageState.Occupied)
            .withOccupied(Some(airplaneId))
            .copy(occupiedSince = Some(data.simTime))
          airplaneRef ! ParkConfirmed(data.garageId)
          occupied(next, airplaneRef)

        case LeaveGarage(airplaneId, _) =>
          ctx.log.warn(s"[Garage ${data.garageId}] LeaveGarage de $airplaneId sur garage libre — ignoré")
          free(data)

        case Tick(simTime) => free(data.copy(simTime = simTime))
      }
    }

  // ─────────────────────────────────────────────
  // État : Occupied — avion stationné
  // P_garage_occupied_j contient 1 jeton
  // Transition T_leave_j franchissable quand groundDuration écoulée
  // ─────────────────────────────────────────────
  private def occupied(
    data:       GarageData,
    airplaneRef: ActorRef[AirplaneCommand]
  ): Behavior[GarageCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case Tick(simTime) =>
          occupied(data.copy(simTime = simTime), airplaneRef)

        case LeaveGarage(airplaneId, towerRef) =>
          data.occupiedBy match {
            case Some(id) if id == airplaneId =>
              ctx.log.info(s"[Garage ${data.garageId} ${fmt(data.simTime)}] $airplaneId quitte le garage")
              val next = data
                .withState(GarageState.Free)
                .withOccupied(None)
                .copy(occupiedSince = None, groundDurationMinutes = 0L)
              // Signaler la libération à la ControlTower
              towerRef ! GarageFreed(data.garageId)
              free(next)

            case Some(otherId) =>
              ctx.log.warn(
                s"[Garage ${data.garageId}] LeaveGarage de $airplaneId " +
                s"mais occupé par $otherId — ignoré"
              )
              occupied(data, airplaneRef)

            case None =>
              ctx.log.warn(s"[Garage ${data.garageId}] LeaveGarage mais occupiedBy = None")
              occupied(data, airplaneRef)
          }

        case ParkRequest(newAirplaneId, _) =>
          // Garage occupé : ne devrait pas arriver si la ControlTower
          // vérifie via le PetriNetEngine avant d'envoyer ParkRequest
          ctx.log.error(
            s"[Garage ${data.garageId}] ParkRequest de $newAirplaneId " +
            s"alors qu'occupé par ${data.occupiedBy} — violation de capacité !"
          )
          occupied(data, airplaneRef)
      }
    }
}
