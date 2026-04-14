package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import cysky.model._
import cysky.protocol._
import cysky.protocol.AirplaneCommand._
import cysky.protocol.ControlTowerCommand._
import java.time.LocalDateTime

// ═══════════════════════════════════════════════════════════════
// AirplaneActor — Akka Typed, style fonctionnel pur
//
// Principes FP appliqués :
//   - Aucune mutation : chaque état est une nouvelle instance
//   - Behavior[A] est une fonction : Command => Behavior[A]
//   - Les données sont immuables (case class)
//   - Le changement d'état = Behaviors.receive retournant un nouveau Behavior
// ═══════════════════════════════════════════════════════════════
object AirplaneActor {

  // ─────────────────────────────────────────────
  // État interne immuable de l'acteur
  // Chaque champ correspond à un attribut du diagramme UML
  // ─────────────────────────────────────────────
  final case class AirplaneData(
    airplaneId:       String,
    flightNumber:     String,
    urgencyLevel:     UrgencyLevel,
    state:            AirplaneState,
    assignedRunwayId: Option[String],
    assignedGarageId: Option[String],
    scheduledArrival: LocalDateTime,
    scheduledDepart:  LocalDateTime,
    towerRef:         ActorRef[ControlTowerCommand]
  ) {
    lazy val groundDurationMinutes: Long =
      java.time.Duration.between(scheduledArrival, scheduledDepart).toMinutes

    def priorityScore: Int = urgencyLevel.priorityScore

    // Transitions d'état pures — retournent une nouvelle AirplaneData
    def withState(s: AirplaneState):          AirplaneData = copy(state = s)
    def withRunway(id: Option[String]):       AirplaneData = copy(assignedRunwayId = id)
    def withGarage(id: Option[String]):       AirplaneData = copy(assignedGarageId = id)
  }

  // ─────────────────────────────────────────────
  // Point d'entrée : factory method
  // Crée l'acteur en état InFlight et envoie immédiatement
  // une demande d'atterrissage à la ControlTower
  // ─────────────────────────────────────────────
  def apply(flight: FlightData, towerRef: ActorRef[ControlTowerCommand]): Behavior[AirplaneCommand] =
    Behaviors.setup { ctx =>
      val data = AirplaneData(
        airplaneId       = flight.airplaneId,
        flightNumber     = flight.flightNumber,
        urgencyLevel     = flight.urgencyLevel,
        state            = AirplaneState.InFlight,
        assignedRunwayId = None,
        assignedGarageId = None,
        scheduledArrival = flight.scheduledArrival,
        scheduledDepart  = flight.scheduledDepart,
        towerRef         = towerRef
      )
      ctx.log.info(s"[${data.flightNumber}] En approche — urgence: ${data.urgencyLevel.label}")
      requestLanding(ctx, data)
      inFlight(data)
    }

  // ─────────────────────────────────────────────
  // État : InFlight — en approche, attend autorisation
  // ─────────────────────────────────────────────
  private def inFlight(data: AirplaneData): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {

      case LandingAuthorized(runwayId) =>
        val next = data.withState(AirplaneState.Landing).withRunway(Some(runwayId))
        data.towerRef ! LandRequest(data.airplaneId, ???) // RunwayActor ref injecté au runtime
        landing(next)

      case HoldPosition =>
        // Rester en InFlight, réessayer au prochain slot
        inFlight(data)

      case Divert =>
        data match {
          case d if d.urgencyLevel == UrgencyLevel.Emergency =>
            // Une urgence ne devrait jamais être déroutée — log critique
            Behaviors.stopped
          case _ =>
            Behaviors.stopped
        }

      case Tick(_) => inFlight(data) // pas de transition sur tick en InFlight
    }

  // ─────────────────────────────────────────────
  // État : Landing — atterrissage en cours
  // La transition se fait automatiquement après la durée simulée
  // ─────────────────────────────────────────────
  private def landing(data: AirplaneData): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {

      case Tick(_) =>
        // En pratique le RunwayActor envoie LandingComplete via la ControlTower
        // qui déclenche la transition vers Taxiing
        landing(data)

      case HoldPosition =>
        // Ne devrait pas arriver en Landing — ignoré
        landing(data)

      // Transition automatique déclenchée par le RunwayActor
      case LandingAuthorized(_) => landing(data) // déjà sur piste, ignoré

      case msg =>
        // Aucun autre message attendu pendant l'atterrissage
        Behaviors.unhandled
    }

  // ─────────────────────────────────────────────
  // État : Taxiing — quitte la piste vers le garage
  // La piste est déjà libérée (RunwayFreed envoyé)
  // ─────────────────────────────────────────────
  private def taxiing(data: AirplaneData, garageRef: ActorRef[GarageCommand]): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {

      case ParkConfirmed(garageId) =>
        val next = data
          .withState(AirplaneState.Parked)
          .withRunway(None)          // piste libérée
          .withGarage(Some(garageId))
        parked(next, garageRef)

      case Tick(_) => taxiing(data, garageRef)

      case _ => Behaviors.unhandled
    }

  // ─────────────────────────────────────────────
  // État : Parked — garé, embarquement / ravitaillement
  // Attend que groundDuration soit écoulée pour demander le décollage
  // ─────────────────────────────────────────────
  private def parked(data: AirplaneData, garageRef: ActorRef[GarageCommand]): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {

      case Tick(simTime) =>
        val elapsed = java.time.Duration.between(data.scheduledArrival, simTime).toMinutes
        if (elapsed >= data.groundDurationMinutes) {
          // Prêt à décoller : quitter le garage
          garageRef ! GarageCommand.LeaveGarage(data.airplaneId, data.towerRef)
          val next = data.withState(AirplaneState.TaxiOut).withGarage(None)
          taxiOut(next)
        } else {
          parked(data, garageRef)
        }

      case HoldPosition =>
        // Le ScheduleManager demande d'attendre (retard planifié)
        parked(data, garageRef)

      case Divert =>
        // Ne devrait pas arriver quand garé — log + stop
        Behaviors.stopped

      case _ => Behaviors.unhandled
    }

  // ─────────────────────────────────────────────
  // État : TaxiOut — rejoint la piste de décollage
  // ─────────────────────────────────────────────
  private def taxiOut(data: AirplaneData): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {

      case TakeoffAuthorized(runwayId) =>
        val next = data.withState(AirplaneState.Takeoff).withRunway(Some(runwayId))
        takeoff(next)

      case HoldPosition =>
        taxiOut(data)

      case Tick(_) =>
        // Envoyer la demande de décollage à chaque tick tant que pas autorisé
        data.towerRef ! RequestTakeoff(data.airplaneId, ???)
        taxiOut(data)

      case _ => Behaviors.unhandled
    }

  // ─────────────────────────────────────────────
  // État : Takeoff — décollage en cours
  // ─────────────────────────────────────────────
  private def takeoff(data: AirplaneData): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {
      case Tick(_) => takeoff(data)
      case _       => Behaviors.unhandled
    }

  // ─────────────────────────────────────────────
  // Helpers privés — actions sans effet de bord sur l'état
  // ─────────────────────────────────────────────
  private def requestLanding(
    ctx:  ActorContext[AirplaneCommand],
    data: AirplaneData
  ): Unit = {
    val msg = data.urgencyLevel match {
      case UrgencyLevel.Emergency =>
        EmergencyLand(data.airplaneId, ctx.self)
      case _ =>
        RequestLanding(data.airplaneId, data.urgencyLevel, ctx.self)
    }
    data.towerRef ! msg
  }
}
