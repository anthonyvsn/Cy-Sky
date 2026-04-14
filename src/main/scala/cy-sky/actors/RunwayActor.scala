package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.model._
import cysky.protocol._
import cysky.protocol.RunwayCommand._
import cysky.protocol.ControlTowerCommand._
import java.time.LocalDateTime

// ═══════════════════════════════════════════════════════════════
// RunwayActor — Akka Typed, style fonctionnel pur
//
// Machine à états stricte : un seul avion par piste à la fois.
// Correspond aux places P_runway_* dans le réseau de Pétri.
// ═══════════════════════════════════════════════════════════════
object RunwayActor {

  // ─────────────────────────────────────────────
  // État interne immuable
  // ─────────────────────────────────────────────
  final case class RunwayData(
    runwayId:          String,
    state:             RunwayState,
    occupiedBy:        Option[String],      // airplaneId courant
    towerRef:          ActorRef[ControlTowerCommand],
    landingDurationMin: Int   = 4,
    takeoffDurationMin: Int   = 3,
    taxiDurationMin:    Int   = 2,
    totalUsageMinutes:  Long  = 0L,         // pour le taux d'occupation ≥ 80%
    occupiedSince:      Option[LocalDateTime] = None
  ) {
    def withState(s: RunwayState):          RunwayData = copy(state = s)
    def withOccupied(id: Option[String]):   RunwayData = copy(occupiedBy = id)
    def withUsage(mins: Long):              RunwayData = copy(totalUsageMinutes = totalUsageMinutes + mins)

    /** Taux d'occupation sur la journée simulée (1440 min = 24h) */
    def occupancyRate(totalSimMinutes: Long): Float =
      if (totalSimMinutes == 0L) 0f
      else totalUsageMinutes.toFloat / totalSimMinutes.toFloat
  }

  // ─────────────────────────────────────────────
  // Point d'entrée
  // ─────────────────────────────────────────────
  def apply(
    runwayId: String,
    towerRef: ActorRef[ControlTowerCommand]
  ): Behavior[RunwayCommand] =
    Behaviors.setup { ctx =>
      ctx.log.info(s"[Runway $runwayId] Initialisée — état: Free")
      val data = RunwayData(
        runwayId = runwayId,
        state    = RunwayState.Free,
        occupiedBy = None,
        towerRef   = towerRef
      )
      free(data)
    }

  // ─────────────────────────────────────────────
  // État : Free — piste disponible
  // P_runway_free_i contient 1 jeton
  // ─────────────────────────────────────────────
  private def free(data: RunwayData): Behavior[RunwayCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case LandRequest(airplaneId, _) =>
          ctx.log.info(s"[Runway ${data.runwayId}] Atterrissage de $airplaneId — début")
          val next = data
            .withState(RunwayState.Landing)
            .withOccupied(Some(airplaneId))
            .copy(occupiedSince = Some(LocalDateTime.now()))
          landing(next)

        case TakeoffRequest(airplaneId, _) =>
          ctx.log.info(s"[Runway ${data.runwayId}] Décollage de $airplaneId — début")
          val next = data
            .withState(RunwayState.TakeoffInProgress)
            .withOccupied(Some(airplaneId))
          takeoffInProgress(next)

        case EmergencyLandRequest(airplaneId, _) =>
          ctx.log.warn(s"[Runway ${data.runwayId}] URGENCE $airplaneId — atterrissage forcé")
          val next = data
            .withState(RunwayState.Landing)
            .withOccupied(Some(airplaneId))
          landing(next)

        case StormShutdown =>
          ctx.log.warn(s"[Runway ${data.runwayId}] Fermeture météo")
          blocked(data)

        case Tick(_) => free(data)
      }
    }

  // ─────────────────────────────────────────────
  // État : Landing — atterrissage en cours
  // P_runway_landing_i contient 1 jeton, P_runway_free_i = 0
  // ─────────────────────────────────────────────
  private def landing(data: RunwayData): Behavior[RunwayCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case Tick(simTime) =>
          // Vérifier si la durée d'atterrissage est écoulée
          val elapsed = data.occupiedSince.map { since =>
            java.time.Duration.between(since, simTime).toMinutes
          }.getOrElse(0L)

          if (elapsed >= data.landingDurationMin) {
            ctx.log.info(s"[Runway ${data.runwayId}] Atterrissage terminé — passage en taxi")
            // La piste est libérée immédiatement (avant la fin du taxi)
            val updated = data
              .withState(RunwayState.TaxiToGarage)
              .withUsage(data.landingDurationMin.toLong)
            data.towerRef ! RunwayFreed(data.runwayId)
            taxiToGarage(updated, simTime)
          } else {
            landing(data)
          }

        // Pendant Landing, tous les autres messages sont mis en file
        // par la ControlTower — on ne les reçoit pas ici
        case _ => Behaviors.unhandled
      }
    }

  // ─────────────────────────────────────────────
  // État : TaxiToGarage — transitoire
  // La piste est déjà libre (RunwayFreed envoyé),
  // l'avion roule vers le garage
  // ─────────────────────────────────────────────
  private def taxiToGarage(
    data:      RunwayData,
    taxiStart: LocalDateTime
  ): Behavior[RunwayCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case Tick(simTime) =>
          val elapsed = java.time.Duration.between(taxiStart, simTime).toMinutes
          if (elapsed >= data.taxiDurationMin) {
            ctx.log.info(s"[Runway ${data.runwayId}] Taxi terminé — retour Free")
            val next = data
              .withState(RunwayState.Free)
              .withOccupied(None)
              .copy(occupiedSince = None)
            free(next)
          } else {
            taxiToGarage(data, taxiStart)
          }

        // La piste est déjà libre : on peut accepter un nouveau vol
        // pendant que l'avion précédent finit son taxi
        case LandRequest(airplaneId, towerRef) =>
          ctx.log.info(s"[Runway ${data.runwayId}] Accepte $airplaneId pendant taxi sortant")
          val next = data
            .withState(RunwayState.Landing)
            .withOccupied(Some(airplaneId))
            .copy(occupiedSince = Some(LocalDateTime.now()))
          landing(next)

        case _ => taxiToGarage(data, taxiStart)
      }
    }

  // ─────────────────────────────────────────────
  // État : TakeoffInProgress — décollage en cours
  // ─────────────────────────────────────────────
  private def takeoffInProgress(data: RunwayData): Behavior[RunwayCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case Tick(simTime) =>
          val elapsed = data.occupiedSince.map { since =>
            java.time.Duration.between(since, simTime).toMinutes
          }.getOrElse(0L)

          if (elapsed >= data.takeoffDurationMin) {
            ctx.log.info(s"[Runway ${data.runwayId}] Décollage terminé — piste libre")
            val next = data
              .withState(RunwayState.Free)
              .withOccupied(None)
              .withUsage(data.takeoffDurationMin.toLong)
              .copy(occupiedSince = None)
            data.towerRef ! RunwayFreed(data.runwayId)
            free(next)
          } else {
            takeoffInProgress(data)
          }

        case _ => Behaviors.unhandled
      }
    }

  // ─────────────────────────────────────────────
  // État : Blocked — piste fermée (météo, incident)
  // Aucune transition franchissable depuis cet état
  // ─────────────────────────────────────────────
  private def blocked(data: RunwayData): Behavior[RunwayCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case Reopen =>
          ctx.log.info(s"[Runway ${data.runwayId}] Réouverture")
          free(data.withState(RunwayState.Free))

        case Tick(_) => blocked(data)

        case other =>
          ctx.log.warn(s"[Runway ${data.runwayId}] Bloquée — message ignoré: $other")
          blocked(data)
      }
    }
}
