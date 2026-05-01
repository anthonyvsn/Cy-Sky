package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.model._
import cysky.protocol._
import cysky.protocol.RunwayCommand._
import cysky.protocol.ControlTowerCommand.RunwayFreed
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/***
 * Classe des pistes de l'aéroport (Akka Typed).
 * 
 * Machine à états stricte : un seul avion par piste à la fois.
 * => Correspond aux places P_runway_* dans le réseau de Pétri.
 * 
 * A noter : Cette classe est un singleton (1 seule instance possible).
 *           C'est comme "static class" en java.
 */
object RunwayActor {

  // ─────────────────────────────────────────────
  // État interne immuable
  // ─────────────────────────────────────────────
  private val HHmm = DateTimeFormatter.ofPattern("HH:mm")
  private def fmt(t: LocalDateTime): String = t.format(HHmm)


  /***
   * Données de la piste.
   * 
   * @param runwayId          id
   * @param state             état de la piste (Free, Landing, TakeOffInProgress, ...)
   * @param occupiedBy        id de l'avion occupant la piste (valeur optionnelle, peut etre présente ou absente)
   * @param towerRef
   * @param landingDurationMin  temps d'atterrissage minimal
   * @param takeoffDurationMin  temps de décollage minimal
   * @param taxiDurationMin     temps minimal pour aller au garage
   * @param totalUsageMinutes
   * @param occupiedSince       moment à partir duquel la piste est occupée (valeur optionnelle, peut etre présente ou absente)
   * @param simTime             temps courant de la simulation
   */
  final case class RunwayData(
    runwayId:          String,
    state:             RunwayState,
    occupiedBy:        Option[String],      // airplaneId courant
    towerRef:          ActorRef[ControlTowerCommand],
    landingDurationMin: Int   = 4,
    takeoffDurationMin: Int   = 3,
    taxiDurationMin:    Int   = 2,
    totalUsageMinutes:  Long  = 0L,
    occupiedSince:      Option[LocalDateTime] = None,
    simTime:            LocalDateTime = LocalDateTime.MIN   // dernière heure simulée connue
  ) {
    def withState(s: RunwayState):          RunwayData = copy(state = s)
    def withOccupied(id: Option[String]):   RunwayData = copy(occupiedBy = id)
    def withUsage(mins: Long):              RunwayData = copy(totalUsageMinutes = totalUsageMinutes + mins)

    /** Taux d'occupation sur la journée simulée (1440 min = 24h) */
    def occupancyRate(totalSimMinutes: Long): Float =
      if (totalSimMinutes == 0L) 0f
      else totalUsageMinutes.toFloat / totalSimMinutes.toFloat
  }


  /***
   * Initialise la piste comme étant vide.
   * Etat initial en début de simulation.
   * 
   * @param runwayId id de la piste
   * @param towerRef
   * @return un [[akka.actor.typed.Behavior]] qui décrit comment l'acteur réagit aux messages de type [[RunwayCommand]].
   */
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
          ctx.log.info(s"[Runway ${data.runwayId} ${fmt(data.simTime)}] Atterrissage de $airplaneId — début")
          val next = data
            .withState(RunwayState.Landing)
            .withOccupied(Some(airplaneId))
            .copy(occupiedSince = Some(data.simTime))
          landing(next)

        case TakeoffRequest(airplaneId, _) =>
          ctx.log.info(s"[Runway ${data.runwayId} ${fmt(data.simTime)}] Décollage de $airplaneId — début")
          val next = data
            .withState(RunwayState.TakeoffInProgress)
            .withOccupied(Some(airplaneId))
            .copy(occupiedSince = Some(data.simTime))
          takeoffInProgress(next)

        case EmergencyLandRequest(airplaneId, _) =>
          ctx.log.warn(s"[Runway ${data.runwayId} ${fmt(data.simTime)}] URGENCE $airplaneId — atterrissage forcé")
          val next = data
            .withState(RunwayState.Landing)
            .withOccupied(Some(airplaneId))
            .copy(occupiedSince = Some(data.simTime))
          landing(next)

        case StormShutdown =>
          ctx.log.warn(s"[Runway ${data.runwayId} ${fmt(data.simTime)}] Fermeture météo")
          blocked(data)

        case Tick(simTime) => free(data.copy(simTime = simTime))
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
          val elapsed = data.occupiedSince.map { since =>
            java.time.Duration.between(since, simTime).toMinutes
          }.getOrElse(0L)

          if (elapsed >= data.landingDurationMin) {
            ctx.log.info(s"[Runway ${data.runwayId} ${fmt(simTime)}] Atterrissage de ${data.occupiedBy.getOrElse("?")} terminé — piste libérée")
            val updated = data
              .withState(RunwayState.TaxiToGarage)
              .withUsage(data.landingDurationMin.toLong)
              .copy(simTime = simTime)
            data.towerRef ! RunwayFreed(data.runwayId)
            taxiToGarage(updated, simTime)
          } else {
            landing(data.copy(simTime = simTime))
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
            ctx.log.info(s"[Runway ${data.runwayId} ${fmt(simTime)}] Taxi terminé — piste libre")
            val next = data
              .withState(RunwayState.Free)
              .withOccupied(None)
              .copy(occupiedSince = None, simTime = simTime)
            free(next)
          } else {
            taxiToGarage(data.copy(simTime = simTime), taxiStart)
          }

        case LandRequest(airplaneId, _) =>
          ctx.log.info(s"[Runway ${data.runwayId} ${fmt(data.simTime)}] Accepte $airplaneId pendant taxi sortant")
          val next = data
            .withState(RunwayState.Landing)
            .withOccupied(Some(airplaneId))
            .copy(occupiedSince = Some(data.simTime))
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
            ctx.log.info(s"[Runway ${data.runwayId} ${fmt(simTime)}] Décollage de ${data.occupiedBy.getOrElse("?")} terminé — piste libre")
            val next = data
              .withState(RunwayState.Free)
              .withOccupied(None)
              .withUsage(data.takeoffDurationMin.toLong)
              .copy(occupiedSince = None, simTime = simTime)
            data.towerRef ! RunwayFreed(data.runwayId)
            free(next)
          } else {
            takeoffInProgress(data.copy(simTime = simTime))
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
