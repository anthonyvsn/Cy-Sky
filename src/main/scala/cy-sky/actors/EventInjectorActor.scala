package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.SimState
import cysky.model.{EventType, InjectedEvent}
import cysky.protocol.{ControlTowerCommand, EventInjectorCommand}
import cysky.protocol.EventInjectorCommand.{AddEvent, Tick}
import cysky.protocol.ControlTowerCommand.InjectEmergencyArrival
import java.time.LocalDateTime

/***
 * Gestion des événements créés depuis le front.
 *
 * Rôle :
 *   Recoit les événements créés depuis le front (urgences, fermetures de piste, retards ...) et les déclenche 30 minutes avant l'heure
 *   simulée (permet à la TowerControl de réagir à l'avance).
 *
 * Logique de déclenchement :
 *   Sur chaque Tick(simTime), l'acteur cherche les événements dont (targetTime − 30 min) == simTime. À ce moment, il envoie le
 *   message approprié à la TowerControl et marque l'événement "triggered" dans SimState.
 *
 * Types d'événements gérés :
 *   EmergencyArrival    -> InjectEmergencyArrival  (TowerControl spawne l'avion)
 */
object EventInjectorActor {   // object : singleton (1 seule instance possible). C'est comme "static class" en java.

  /***
   * Méthode de lancement de [[EventInjectorActor]].
   * 
   * @param towerRef    référence de l'acteur [[TowerControlActor]] destinataire des messages
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[ControlTowerCommand]].
   */
  def apply(
    towerRef: ActorRef[ControlTowerCommand]
  ): Behavior[EventInjectorCommand] =
    running(towerRef, pendingEvents = List.empty)

  /***
   * Méthode principale de gestion des événement injectés.
   * 
   * @param towerRef        référence de l'acteur [[TowerControlActor]] destinataire des messages
   * @param pendingEvents   liste des événements injectés
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[EventInjectorCommand]].
   */
  private def running(
    towerRef:      ActorRef[ControlTowerCommand],
    pendingEvents: List[InjectedEvent]
  ): Behavior[EventInjectorCommand] =

    Behaviors.receive { (ctx, msg) =>
      msg match {

        // Nouvel événement reçu depuis le front
        case AddEvent(event) =>
          ctx.log.info(
            s"[EventInjector] Événement enregistré : " +
            s"${event.eventType.displayName} à ${event.targetTimeStr} " +
            s"(urgence: ${event.urgencyLevel.label})" +
            (if (event.note.nonEmpty) s" — « ${event.note} »" else "")
          )
          running(towerRef, event :: pendingEvents)

        // Tick de l'horloge simulée
        case Tick(simTime) =>
          // Chercher les événements dont le déclenchement (targetTime − 30 min) correspond au simTime courant.
          val toTrigger = pendingEvents.filter { e =>
            e.status == "pending" && isTriggerTime(e, simTime)
          }

          // Déclencher chaque événement matché
          toTrigger.foreach { e =>
            ctx.log.warn(
              s"[EventInjector] ⚡ Déclenchement 30 min avant ${e.targetTimeStr} : " +
              s"${e.eventType.displayName} (urgence: ${e.urgencyLevel.label})"
            )
            dispatchEvent(towerRef, e, simTime)
            SimState.updateEventStatus(e.id, "triggered")
          }

          // Mettre à jour l'état local des événements déclenchés
          val updatedEvents =
            if (toTrigger.isEmpty) pendingEvents
            else pendingEvents.map { e =>
              if (toTrigger.exists(_.id == e.id)) e.copy(status = "triggered") else e
            }

          running(towerRef, updatedEvents)
      }
    }

  /***
   * Vérifie si simTime = targetTime − 30 min
   * 
   * @param e         événement injecté
   * @param simTime   horaire de la simulation en cours
   * @return true si simTime = targetTime − 30 min
   */
  private def isTriggerTime(e: InjectedEvent, simTime: LocalDateTime): Boolean = {
    // Construire le datetime cible puis soustraire 30 min
    val targetDt  = simTime.toLocalDate.atTime(e.targetHour, e.targetMinute)
    val triggerDt = targetDt.minusMinutes(30)
    simTime.getHour   == triggerDt.getHour &&
    simTime.getMinute == triggerDt.getMinute
  }

  /***
   * Envoie un message a la TowerControl pour l'informer de l'action à effectuer en fonction du type de l'événement.
   * 
   * @param towerRef  référence de l'acteur [[TowerControlActor]] destinataire des messages
   * @param e         événement injecté
   * @param simTime   horaire de la simulation en cours
   * @return le type de l'événement.
   */
  private def dispatchEvent(
    towerRef: ActorRef[ControlTowerCommand],
    e:        InjectedEvent,
    simTime:  LocalDateTime
  ): Unit = e.eventType match {

    case EventType.EmergencyArrival =>
      val targetDt = simTime.toLocalDate.atTime(e.targetHour, e.targetMinute)
      towerRef ! InjectEmergencyArrival(e.urgencyLevel, targetDt)

    case EventType.GroundStrike =>
      towerRef ! cysky.protocol.ControlTowerCommand.DelayFlight(e.id, 120L)
  }
}
