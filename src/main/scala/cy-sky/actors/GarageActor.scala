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


/***
 * Classe garage (Akka Typed).
 * Elle sert à stocker les avions à l'arret (embarquement, en attente, ...)
 * 
 * 2 etats possibles : Free ou Occupied.
 * Correspond aux places P_garage_free_j et P_garage_occupied_j dans le réseau de Pétri.
 * L'invariant |P_garage_free| + |P_garage_occupied| = M est maintenu par la ControlTower, pas par cet acteur.
 * 
 * A noter : Cette classe est un singleton (1 seule instance possible).
 *           C'est comme "static class" en java.
 */
object GarageActor {

  private val HHmm = DateTimeFormatter.ofPattern("HH:mm")
  /***
   * Renvoie l'horaire en String.
   * @return l'horaire en String HH:mm
   */
  private def fmt(t: LocalDateTime): String = t.format(HHmm)


  /***
   * Données liées au garage.
   * 
   * @param garageId      id du garage
   * @param state         état du garage (Free / Occupied)
   * @param occupiedBy    id de l'avion occupant le garage (valeur optionnelle, peut etgre présente ou absente)
   * @param towerRef      référence de l'acteur [[TowerControlActor]] destinataire des messages
   * @param occupiedSince horaire à partir duquel le garage est occupé (valeur optionnelle, peut etgre présente ou absente)
   * @param groundDurationMinutes temps passé par l'avion dans le garage
   * @param simTime       horaire actuelle de la simulation
   */
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
     * Utilisé par le ScheduleManager pour identifier les garages libérables en avance (plan B du scénario urgence).
     */
    def remainingGroundTime(now: LocalDateTime): Long =
      occupiedSince.map { since =>
        val elapsed = java.time.Duration.between(since, now).toMinutes
        (groundDurationMinutes - elapsed).max(0L)
      }.getOrElse(0L)
  }


  /***
   * Initialise le garage à l'état "Free" (libre).
   * Point d'entrée de [[GarageActor]].
   * 
   * @param garageId      id du garage
   * @param towerRef      référence de l'acteur [[TowerControlActor]] destinataire des messages
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[GarageCommand]].
   */
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
  /***
   * Le garage est disponible (état "Free").
   * On gère les cas suivants : avion qui rentre sur garage, avion qui quitte le garage (erreur).
   * 
   * @param garageId      id du garage
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[GarageCommand]].
   */
  private def free(data: GarageData): Behavior[GarageCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        // Avion qui rentre sur garage
        case ParkRequest(airplaneId, airplaneRef) =>
          ctx.log.info(s"[Garage ${data.garageId} ${fmt(data.simTime)}] $airplaneId stationné")
          val next = data
            .withState(GarageState.Occupied)
            .withOccupied(Some(airplaneId))
            .copy(occupiedSince = Some(data.simTime))
          airplaneRef ! ParkConfirmed(data.garageId)
          occupied(next, airplaneRef)

        // Avion quitte le garage (erreur)
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
  /***
   * Le garage contient un avion stationné (état "Occupied").
   * On gère les cas suivants : avion qui rentre sur garage, avion qui quitte le garage (erreur).
   * 
   * @param garageId      id du garage
   * @param airplaneRef   référence de l'acteur [[AirplaneCommand]] destinaaire des messages (?)
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[GarageCommand]].
   */
  private def occupied(
    data:       GarageData,
    airplaneRef: ActorRef[AirplaneCommand]
  ): Behavior[GarageCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        // Garage reste occupé (état inchangé)
        case Tick(simTime) =>
          occupied(data.copy(simTime = simTime), airplaneRef)

        // Avion quitte le garage
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
