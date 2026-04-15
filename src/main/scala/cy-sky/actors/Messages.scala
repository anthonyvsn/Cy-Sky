package cysky.protocol

import akka.actor.typed.ActorRef
import cysky.model._
import java.time.LocalDateTime

// ═══════════════════════════════════════════════════════════════
// PROTOCOLE AKKA TYPED — CySky
//
// Convention de nommage :
//   - Les traits sealed définissent le type de message d'un acteur
//   - Chaque case class/object est un message précis
//   - Les ActorRef dans les messages permettent le pattern ask / tell
// ═══════════════════════════════════════════════════════════════

// ───────────────────────────────────────────────────────────────
// Messages de l'AirplaneActor
// ───────────────────────────────────────────────────────────────
sealed trait AirplaneCommand

object AirplaneCommand {

  /** La ControlTower autorise l'atterrissage sur la piste donnée */
  final case class LandingAuthorized(runwayId: String)
    extends AirplaneCommand

  /** La ControlTower autorise le décollage depuis la piste donnée */
  final case class TakeoffAuthorized(runwayId: String)
    extends AirplaneCommand

  /** Le GarageActor confirme le stationnement */
  final case class ParkConfirmed(garageId: String)
    extends AirplaneCommand

  /** Rester en attente (file saturée ou piste non disponible) */
  case object HoldPosition extends AirplaneCommand

  /** Déroutement vers un autre aéroport — état terminal */
  case object Divert extends AirplaneCommand

  /**
   * La ControlTower indique à l'avion de rouler vers le garage assigné.
   * Déclenche la transition Landing → Taxiing dans l'AirplaneActor.
   */
  final case class TaxiToGarage(garageRef: ActorRef[GarageCommand])
    extends AirplaneCommand

  /** Tick de l'horloge Akka — avance le temps simulé */
  final case class Tick(simulatedTime: LocalDateTime)
    extends AirplaneCommand
}

// ───────────────────────────────────────────────────────────────
// Messages de la ControlTower (envoyés PAR l'avion)
// ───────────────────────────────────────────────────────────────
sealed trait ControlTowerCommand

object ControlTowerCommand {

  /** Demande d'atterrissage standard */
  final case class RequestLanding(
    airplaneId:   String,
    urgencyLevel: UrgencyLevel,
    replyTo:      ActorRef[AirplaneCommand]
  ) extends ControlTowerCommand

  /** Demande de décollage */
  final case class RequestTakeoff(
    airplaneId: String,
    replyTo:    ActorRef[AirplaneCommand]
  ) extends ControlTowerCommand

  /**
   * Atterrissage d'urgence — injecté par EventInjector.
   * Arc inhibiteur Pétri : bloque toutes les T_land standards
   * tant que ce message n'est pas traité.
   */
  final case class EmergencyLand(
    airplaneId: String,
    replyTo:    ActorRef[AirplaneCommand]
  ) extends ControlTowerCommand

  /** Une piste vient de se libérer — traiter la file d'attente */
  final case class RunwayFreed(runwayId: String)
    extends ControlTowerCommand

  /** Un garage vient de se libérer */
  final case class GarageFreed(garageId: String)
    extends ControlTowerCommand

  /** Le ScheduleManager envoie un nouveau plan optimisé */
  final case class RescheduleFlights(newPlan: List[FlightData])
    extends ControlTowerCommand

  /** Retarder un vol spécifique */
  final case class DelayFlight(flightId: String, delayMinutes: Long)
    extends ControlTowerCommand

  /** Annuler un vol */
  final case class CancelFlight(flightId: String)
    extends ControlTowerCommand

  /** Tick de l'horloge simulée — avance le temps de la simulation */
  final case class Tick(simulatedTime: LocalDateTime)
    extends ControlTowerCommand
}

// ───────────────────────────────────────────────────────────────
// Messages du RunwayActor
// ───────────────────────────────────────────────────────────────
sealed trait RunwayCommand

object RunwayCommand {

  /** Ordre d'atterrissage depuis la ControlTower */
  final case class LandRequest(
    airplaneId: String,
    replyTo:    ActorRef[ControlTowerCommand]
  ) extends RunwayCommand

  /** Ordre de décollage depuis la ControlTower */
  final case class TakeoffRequest(
    airplaneId: String,
    replyTo:    ActorRef[ControlTowerCommand]
  ) extends RunwayCommand

  /** Atterrissage d'urgence — priorité absolue */
  final case class EmergencyLandRequest(
    airplaneId: String,
    replyTo:    ActorRef[ControlTowerCommand]
  ) extends RunwayCommand

  /** Fermeture pour cause de météo ou incident */
  case object StormShutdown extends RunwayCommand

  /** Réouverture de la piste */
  case object Reopen extends RunwayCommand

  /** Tick horloge */
  final case class Tick(simulatedTime: LocalDateTime) extends RunwayCommand
}

// ───────────────────────────────────────────────────────────────
// Messages du GarageActor
// ───────────────────────────────────────────────────────────────
sealed trait GarageCommand

object GarageCommand {

  /** Demande de stationnement depuis la ControlTower */
  final case class ParkRequest(
    airplaneId: String,
    replyTo:    ActorRef[AirplaneCommand]
  ) extends GarageCommand

  /** L'avion demande à quitter le garage */
  final case class LeaveGarage(
    airplaneId: String,
    towerRef:   ActorRef[ControlTowerCommand]
  ) extends GarageCommand

  /** Tick horloge — vérifie si groundDuration est écoulée */
  final case class Tick(simulatedTime: LocalDateTime) extends GarageCommand
}

// ───────────────────────────────────────────────────────────────
// Messages du PetriNetEngine
// ───────────────────────────────────────────────────────────────
sealed trait PetriNetCommand

object PetriNetCommand {

  /** Mettre à jour le marquage après une opération autorisée */
  final case class UpdateState(
    consume: Map[String, Int],
    produce: Map[String, Int]
  ) extends PetriNetCommand

  /** Vérifier si au moins une transition est franchissable */
  final case class CheckDeadlock(
    replyTo: ActorRef[PetriNetReply]
  ) extends PetriNetCommand

  /**
   * Simuler un plan alternatif sans modifier le marquage courant.
   * Retourne le marquage résultant ou None si une propriété est violée.
   */
  final case class SimulatePlan(
    transitions: List[String],
    replyTo:     ActorRef[PetriNetReply]
  ) extends PetriNetCommand

  /** Vérifier une formule LTL sur le marquage courant */
  final case class CheckInvariant(
    formula: String,
    replyTo: ActorRef[PetriNetReply]
  ) extends PetriNetCommand
}

sealed trait PetriNetReply

object PetriNetReply {
  final case class DeadlockStatus(hasDeadlock: Boolean)        extends PetriNetReply
  final case class PlanVerdict(valid: Boolean, reason: String, score: Int) extends PetriNetReply
  final case class InvariantResult(holds: Boolean, witness: Option[String]) extends PetriNetReply
}
