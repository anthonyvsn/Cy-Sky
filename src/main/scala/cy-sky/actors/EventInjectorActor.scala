package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.SimState
import cysky.model.{EventType, InjectedEvent}
import cysky.protocol.{ControlTowerCommand, EventInjectorCommand}
import cysky.protocol.EventInjectorCommand.{AddEvent, Tick}
import cysky.protocol.ControlTowerCommand.{
  InjectEmergencyArrival, InjectRunwayClosure, DelayFlight, CancelFlight
}
import java.time.LocalDateTime

// ═══════════════════════════════════════════════════════════════
// EventInjectorActor — point d'entrée des contraintes dynamiques
//
// Rôle :
//   Reçoit les événements créés depuis le front (urgences, fermetures
//   de piste, retards…) et les déclenche 30 MINUTES AVANT l'heure
//   simulée cible, permettant à la TowerControl de réagir à l'avance.
//
// Logique de déclenchement :
//   Sur chaque Tick(simTime), l'acteur cherche les événements dont
//   (targetTime − 30 min) == simTime. À ce moment, il envoie le
//   message approprié à la TowerControl et marque l'événement
//   "triggered" dans SimState.
//
// Types d'événements gérés :
//   EmergencyArrival    → InjectEmergencyArrival (TowerControl spawne l'avion)
//   RunwayClosure       → InjectRunwayClosure     (TowerControl ferme piste aléatoire)
//   FlightDelay         → DelayFlight             (TowerControl retarde un vol aléatoire)
//   FlightCancellation  → CancelFlight            (TowerControl annule un vol aléatoire)
// ═══════════════════════════════════════════════════════════════
object EventInjectorActor {

  def apply(
    towerRef: ActorRef[ControlTowerCommand]
  ): Behavior[EventInjectorCommand] =
    running(towerRef, pendingEvents = List.empty)

  private def running(
    towerRef:      ActorRef[ControlTowerCommand],
    pendingEvents: List[InjectedEvent]
  ): Behavior[EventInjectorCommand] =

    Behaviors.receive { (ctx, msg) =>
      msg match {

        // ── Nouvel événement reçu depuis le front ─────────────────
        case AddEvent(event) =>
          ctx.log.info(
            s"[EventInjector] Événement enregistré : " +
            s"${event.eventType.displayName} à ${event.targetTimeStr} " +
            s"(urgence: ${event.urgencyLevel.label})" +
            (if (event.note.nonEmpty) s" — « ${event.note} »" else "")
          )
          running(towerRef, event :: pendingEvents)

        // ── Tick de l'horloge simulée ─────────────────────────────
        case Tick(simTime) =>
          // Chercher les événements dont le déclenchement (targetTime − 30 min)
          // correspond au simTime courant.
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

  // ─────────────────────────────────────────────
  // Vérifie si simTime correspond à (targetTime − 30 min)
  // ─────────────────────────────────────────────
  private def isTriggerTime(e: InjectedEvent, simTime: LocalDateTime): Boolean = {
    // Construire le datetime cible puis soustraire 30 min
    val targetDt  = simTime.toLocalDate.atTime(e.targetHour, e.targetMinute)
    val triggerDt = targetDt.minusMinutes(30)
    simTime.getHour   == triggerDt.getHour &&
    simTime.getMinute == triggerDt.getMinute
  }

  // ─────────────────────────────────────────────
  // Dispatcher — envoie le message adapté selon le type d'événement
  // ─────────────────────────────────────────────
  private def dispatchEvent(
    towerRef: ActorRef[ControlTowerCommand],
    e:        InjectedEvent,
    simTime:  LocalDateTime
  ): Unit = e.eventType match {

    case EventType.EmergencyArrival =>
      // L'avion doit arriver à l'heure cible (pas à l'heure du trigger)
      val targetDt = simTime.toLocalDate.atTime(e.targetHour, e.targetMinute)
      towerRef ! InjectEmergencyArrival(e.urgencyLevel, targetDt)

    case EventType.RunwayClosure =>
      // La TowerControl ferme une piste aléatoire disponible
      towerRef ! InjectRunwayClosure

    case EventType.FlightDelay =>
      // Retard de 30 min appliqué à un vol futur aléatoire
      // On utilise l'id de l'événement comme token (TowerControl choisit le vol)
      towerRef ! DelayFlight(e.id, 30L)

    case EventType.FlightCancellation =>
      // Annulation d'un vol futur aléatoire
      towerRef ! CancelFlight(e.id)
  }
}
