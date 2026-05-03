package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import cysky.model._
import cysky.protocol._
import cysky.protocol.AirplaneCommand._
import cysky.protocol.ControlTowerCommand.{RequestLanding, RequestTakeoff, EmergencyLand}
import cysky.protocol.GarageCommand
import java.time.LocalDateTime

/***
 * AirplaneActor — Akka Typed, style fonctionnel pur
 *
 * Principes FP appliqués :
 *   - Aucune mutation : chaque état est une nouvelle instance
 *   - Behavior[A] est une fonction : Command => Behavior[A]
 *   - Les données sont immuables (case class)
 *   - Le changement d'état = Behaviors.receive retournant un nouveau Behavior
*/
object AirplaneActor {    // object : singleton (1 seule instance possible). C'est comme "static class" en java.
  
  /***
   * Contient les données liées à l'avion.
   * État interne immuable de l'acteur.
   * Chaque champ correspond à un attribut du diagramme UML.
   * 
   * @param airplaneId    est l'id de l'avion
   * @param flightNumber  est le numero de vol
   * @param urgencyLevel  est le niveau de priorité de l'avion pour aterrir
   * @param state         correspond à l'état de l'avion (en vol, sur piste, en garage,...)
   * @param assignedRunwayId  est la piste assignée à l'avion pour aterrir.
   * @param assignedGarageId  est le garage assigné à l'avion.
   * @param scheduledArrival  horaire d'arrivée
   * @param scheduledDepart   horaire de départ
   * @param towerRef          référence de l'acteur [[TowerControlActor]] destinataire des messages
   */
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

  /***
   * Crée l'acteur en état InFlight et envoie immédiatement une demande d'atterrissage à la ControlTower.
   * Méthode principale (factory method).
   * 
   * @param flight  contient les données du vol.
   * @param ControlTower contient les messages de la ControlTower.
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[AirplaneCommand]].
   */
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

  /***
   * L'état "InFlight" dit que l'avion est en approche.
   * Il attend l'autorisation de pouvoir aterrir.
   * 
   * @param data contient les données de l'avion.
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[AirplaneCommand]].
   */
  private def inFlight(data: AirplaneData): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {

      case LandingAuthorized(runwayId) =>
        val next = data.withState(AirplaneState.Landing).withRunway(Some(runwayId))
        landing(next)

      case HoldPosition =>
        // Rester en InFlight, réessayer au prochain slot
        inFlight(data)

      case Divert =>
        Behaviors.stopped

      case Tick(_) => inFlight(data) // pas de transition sur tick en InFlight

      case UpdateScheduledDepart(newDepart) => inFlight(data.copy(scheduledDepart = newDepart))
    }

  /***
   * L'état "Landing" dit que l'avion est en cours d'atterrissage.
   * La transition se fait automatiquement après la durée simulée.
   * 
   * @param data contient les données de l'avion.
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[AirplaneCommand]].
   */
  private def landing(data: AirplaneData): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {

      case Tick(_) =>
        landing(data)

      case TaxiToGarage(garageRef) =>
        // La ControlTower confirme la fin d'atterrissage et assigne un garage
        val next = data.withState(AirplaneState.Taxiing)
        taxiing(next, garageRef)

      case HoldPosition =>
        landing(data)

      case LandingAuthorized(_) => landing(data) // déjà sur piste, ignoré

      case UpdateScheduledDepart(newDepart) => landing(data.copy(scheduledDepart = newDepart))

      case msg =>
        Behaviors.unhandled
    }

  /***
   * L'état "Taxiing" dit que l'avion quitte la pîste vers le garage.
   * La piste est déjà libérée (RunwayFreed envoyé).
   * 
   * @param data contient les données de l'avion.
   * @param garageRef est utilisé pour communiquer des messages liés au garage à la [[ControlTower]].
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[AirplaneCommand]].
   */
  private def taxiing(data: AirplaneData, garageRef: ActorRef[GarageCommand]): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {

      case ParkConfirmed(garageId) =>
        val next = data
          .withState(AirplaneState.Parked)
          .withRunway(None)          // piste libérée
          .withGarage(Some(garageId))
        parked(next, garageRef)

      case Tick(_) => taxiing(data, garageRef)

      case UpdateScheduledDepart(newDepart) => taxiing(data.copy(scheduledDepart = newDepart), garageRef)

      case _ => Behaviors.unhandled
    }

  /***
   * L'état "Parked" dit que l'avion est garé (/embarquement/ravitaillement).
   * Attend que groundDuration soit écoulée pour demander le décollage
   * 
   * @param data contient les données de l'avion.
   * @param garageRef est utilisé pour communiquer des messages liés au garage à la [[ControlTower]]. (ex: avion quitte le garage)
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[AirplaneCommand]].
   */
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

      case UpdateScheduledDepart(newDepart) =>
        parked(data.copy(scheduledDepart = newDepart), garageRef)

      case _ => Behaviors.unhandled
    }

  /***
   * L'état "TaxiOut" dit que l'avion rejoint la piste pour décoller.
   * Attend que groundDuration soit écoulée pour demander le décollage
   * 
   * @param data contient les données de l'avion.
   * @param requestSent évite de renvoyer la demande à chaque Tick.
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[AirplaneCommand]].
   */
  private def taxiOut(data: AirplaneData, requestSent: Boolean = false): Behavior[AirplaneCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case TakeoffAuthorized(runwayId) =>
          val next = data.withState(AirplaneState.Takeoff).withRunway(Some(runwayId))
          takeoff(next)

        case HoldPosition =>
          // La tour refuse pour l'instant — réessayer au prochain tick
          taxiOut(data, requestSent = false)

        case Tick(_) =>
          if (!requestSent) {
            data.towerRef ! RequestTakeoff(data.airplaneId, ctx.self)
          }
          taxiOut(data, requestSent = true)

        case _ => Behaviors.unhandled
      }
    }

   /***
   * L'état "TakeOff" désigne le décollage en cours de l'avion.
   * 
   * @param data contient les données de l'avion.
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[AirplaneCommand]].
   */
  private def takeoff(data: AirplaneData): Behavior[AirplaneCommand] =
    Behaviors.receiveMessage {
      case Tick(_) => takeoff(data)
      case _       => Behaviors.unhandled
    }

  /***
   * Envoie un message à ControlTower pour effectuer une demande d'atterrissage avec niveau d'urgance approprié.
   * Helpers privés — actions sans effet de bord sur l'état.
   * 
   * @param ctx désigne le contexte de l'avion.
   * @param data contient les données de l'avion.
   * @return
   */
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
