package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import cysky.model._
import cysky.models.{AircraftFlight => Flight, Arrival, Departure}
import cysky.protocol._
import cysky.protocol.ControlTowerCommand._
import cysky.protocol.AirplaneCommand.{LandingAuthorized, TakeoffAuthorized, TaxiToGarage}
import cysky.protocol.GarageCommand.ParkRequest
import cysky.{SimState, SimSlot}
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

// ═══════════════════════════════════════════════════════════════
// TowerControlActor — Akka Typed, style fonctionnel pur
//
// Orchestre une journée de simulation à l'aéroport en suivant
// le planning généré par ScheduleGeneratorAlgorithm.
//
// Responsabilités :
//   - Spawner les AirplaneActors au bon moment (heure d'arrivée)
//   - Gérer les files d'atterrissage et de décollage par priorité
//   - Assigner pistes libres et garages disponibles
//   - Propager les Ticks à tous les acteurs enfants
// ═══════════════════════════════════════════════════════════════
object TowerControlActor {

  // ─────────────────────────────────────────────
  // Demandes en attente dans les files internes
  // ─────────────────────────────────────────────
  final case class PendingLanding(
    airplaneId: String,
    urgency:    UrgencyLevel,
    replyTo:    ActorRef[AirplaneCommand],
    emergency:  Boolean = false
  )

  final case class PendingTakeoff(
    airplaneId: String,
    replyTo:    ActorRef[AirplaneCommand]
  )

  // ─────────────────────────────────────────────
  // État interne immuable de la tour de contrôle
  //
  //   runways          : pistes disponibles (id → acteur)
  //   garages          : garages disponibles (id → acteur)
  //   airplanes        : avions actifs       (id → acteur)
  //   schedule         : emploi du temps     (runwayId → vols)
  //   landingQueue     : file d'atterrissage triée par priorité décroissante
  //   takeoffQueue     : file de décollage FIFO
  //   runwayOccupancy  : quelle piste est occupée par quel avion
  //   freeRunways      : pistes actuellement libres
  //   freeGarages      : garages actuellement libres
  //   launchedAirplanes: airplaneIds déjà instanciés (évite les doublons)
  //   simDate          : date du jour simulé
  // ─────────────────────────────────────────────
  final case class TowerData(
    runways:              Map[String, ActorRef[RunwayCommand]],
    garages:              Map[String, ActorRef[GarageCommand]],
    airplanes:            Map[String, ActorRef[AirplaneCommand]],
    schedule:             Map[String, List[Flight]],
    landingQueue:         List[PendingLanding],
    takeoffQueue:         List[PendingTakeoff],
    runwayOccupancy:      Map[String, String],   // runwayId -> airplaneId (atterrissages)
    takeoffOccupancy:     Map[String, String],   // runwayId -> airplaneId (décollages)
    freeRunways:          Set[String],
    freeGarages:          Set[String],
    launchedAirplanes:    Set[String],
    simDate:              LocalDate,
    simTime:              LocalDateTime,
    flightStates:         Map[String, String],   // airplaneId -> état lisible
    tickCount:            Int,                   // compteur de ticks pour throttle dashboard
    scheduleManagerRef:   ActorRef[ScheduleManagerCommand],
    notifiedEventIds:     Set[String],           // IDs d'InjectedEvent déjà transmis au ScheduleManager
    delayInfo:            Map[String, Int],       // flightId -> retard cumulé en minutes (Mode Contrôle)
    cancelledFlights:     List[Flight]            // vols annulés par le SM (Mode Contrôle, pour affichage)
  )

  private val HHmm = DateTimeFormatter.ofPattern("HH:mm")
  private def fmt(t: LocalDateTime): String = t.format(HHmm)

  // ─────────────────────────────────────────────
  // Point d'entrée
  //
  // La TowerControl spawne elle-même ses RunwayActors et GarageActors
  // (ctx.self disponible dans Behaviors.setup → évite la dépendance circulaire).
  //
  //   runwayCount : nombre de pistes à créer  (ids : RWY_1, RWY_2 …)
  //   garageCount : nombre de garages à créer (ids : GATE_1, GATE_2 …)
  //   schedule    : plan de vol généré par ScheduleGeneratorAlgorithm
  //   simDate     : date de la simulation (pour construire les LocalDateTime)
  // ─────────────────────────────────────────────
  def apply(
    runwayCount: Int,
    garageCount: Int,
    schedule:    Map[String, List[Flight]],
    simDate:     LocalDate,
    slot:        SimSlot,
    mode:        ScheduleManagerActor.Mode = ScheduleManagerActor.Controle
  ): Behavior[ControlTowerCommand] =
    Behaviors.setup { ctx =>
      // Spawn des pistes — ctx.self est l'ActorRef[ControlTowerCommand]
      val runways: Map[String, ActorRef[RunwayCommand]] =
        (1 to runwayCount).map { i =>
          val id = s"RWY_$i"
          id -> ctx.spawn(RunwayActor(id, ctx.self), s"runway-$id-${slot.name}")
        }.toMap

      // Spawn des garages
      val garages: Map[String, ActorRef[GarageCommand]] =
        (1 to garageCount).map { j =>
          val id = s"GATE_$j"
          id -> ctx.spawn(GarageActor(id, ctx.self), s"garage-$id-${slot.name}")
        }.toMap

      val totalFlights = schedule.values.flatten.count(_.kind == Arrival)
      ctx.log.info(
        s"[TowerControl/${slot.name}] Démarrage simulation du $simDate — " +
        s"${runways.size} piste(s), ${garages.size} garage(s), $totalFlights vol(s) planifiés"
      )

      // Spawn du ScheduleManager avec le mode demandé
      val runwayIds = runways.keySet.toList.sorted
      val scheduleManagerRef = ctx.spawn(
        ScheduleManagerActor(
          towerRef    = ctx.self,
          schedule    = schedule,
          runwayIds   = runwayIds,
          runwayCount = runwayCount,
          garageCount = garageCount,
          mode        = mode
        ),
        s"schedule-manager-${slot.name}"
      )

      val data = TowerData(
        runways              = runways,
        garages              = garages,
        airplanes            = Map.empty,
        schedule             = schedule,
        landingQueue         = List.empty,
        takeoffQueue         = List.empty,
        runwayOccupancy      = Map.empty,
        takeoffOccupancy     = Map.empty,
        freeRunways          = runways.keySet,
        freeGarages          = garages.keySet,
        launchedAirplanes    = Set.empty,
        simDate              = simDate,
        simTime              = simDate.atTime(6, 0),
        flightStates         = Map.empty,
        tickCount            = 0,
        scheduleManagerRef   = scheduleManagerRef,
        notifiedEventIds     = Set.empty,
        delayInfo            = Map.empty,
        cancelledFlights     = List.empty
      )
      running(ctx, data, slot)
    }

  // ─────────────────────────────────────────────
  // Comportement principal — machine à état unique
  // (pas de sous-états car la ControlTower doit
  //  toujours rester réactive à tous les messages)
  // ─────────────────────────────────────────────
  private def running(
    ctx:  ActorContext[ControlTowerCommand],
    data: TowerData,
    slot: SimSlot
  ): Behavior[ControlTowerCommand] =
    Behaviors.receiveMessage {

      // ── Tick horloge simulée ──────────────────────────────────
      case Tick(simTime) =>
        val d0 = data.copy(simTime = simTime, tickCount = data.tickCount + 1)
        val d1 = notifyScheduleManager(ctx, d0)
        val d2 = spawnDueAirplanes(ctx, d1, simTime)
        d2.runways.values.foreach(_ ! RunwayCommand.Tick(simTime))
        d2.garages.values.foreach(_ ! GarageCommand.Tick(simTime))
        d2.airplanes.values.foreach(_ ! AirplaneCommand.Tick(simTime))
        // Mise à jour du slot dédié à cette simulation
        slot.update(d2)
        running(ctx, d2, slot)

      // ── Demande d'atterrissage standard ──────────────────────
      case RequestLanding(airplaneId, urgency, replyTo) =>
        ctx.log.info(s"[TowerControl/${slot.name} ${fmt(data.simTime)}] Demande atterrissage $airplaneId (urgence: ${urgency.label})")
        val pending = PendingLanding(airplaneId, urgency, replyTo)
        val d1 = data.copy(landingQueue = insertByPriority(data.landingQueue, pending))
        running(ctx, drainLandingQueue(ctx, d1), slot)

      // ── Atterrissage d'urgence ────────────────────────────────
      case EmergencyLand(airplaneId, replyTo) =>
        ctx.log.warn(s"[TowerControl/${slot.name} ${fmt(data.simTime)}] URGENCE $airplaneId — passage en tête de file")
        val pending = PendingLanding(airplaneId, UrgencyLevel.Emergency, replyTo, emergency = true)
        val d1 = data.copy(landingQueue = pending :: data.landingQueue)
        running(ctx, drainLandingQueue(ctx, d1), slot)

      // ── Demande de décollage ──────────────────────────────────
      case RequestTakeoff(airplaneId, replyTo) =>
        ctx.log.info(s"[TowerControl/${slot.name} ${fmt(data.simTime)}] Demande décollage $airplaneId")
        val d1 = data.copy(
          takeoffQueue = data.takeoffQueue :+ PendingTakeoff(airplaneId, replyTo),
          flightStates = data.flightStates + (airplaneId -> "Taxi vers piste")
        )
        running(ctx, drainTakeoffQueue(ctx, d1), slot)

      // ── Piste libérée ─────────────────────────────────────────
      case RunwayFreed(runwayId) =>
        val justLandedId   = data.runwayOccupancy.get(runwayId)
        val justDepartedId = data.takeoffOccupancy.get(runwayId)
        justDepartedId match {
          case Some(id) => ctx.log.info(s"[TowerControl/${slot.name} ${fmt(data.simTime)}] $id a décollé — piste $runwayId libre")
          case None     =>
        }
        val d1 = data.copy(
          freeRunways      = data.freeRunways + runwayId,
          runwayOccupancy  = data.runwayOccupancy  - runwayId,
          takeoffOccupancy = data.takeoffOccupancy - runwayId,
          airplanes        = justDepartedId.fold(data.airplanes)(data.airplanes - _),
          flightStates     = justDepartedId.fold(data.flightStates) { id =>
            // Ne pas écraser un statut final (BOOM, Annulé)
            if (isFinalStatus(data.flightStates.getOrElse(id, ""))) data.flightStates
            else data.flightStates + (id -> "Parti")
          }
        )
        val d2 = justLandedId.fold(d1)(id => assignGarage(ctx, d1, id))
        val d3 = drainLandingQueue(ctx, d2)
        running(ctx, drainTakeoffQueue(ctx, d3), slot)

      // ── Garage libéré ─────────────────────────────────────────
      case GarageFreed(garageId) =>
        ctx.log.info(s"[TowerControl/${slot.name} ${fmt(data.simTime)}] Garage $garageId libéré")
        running(ctx, data.copy(freeGarages = data.freeGarages + garageId), slot)

      // ── Replanification ───────────────────────────────────────
      case FlightAddedByManager(newSchedule) =>
        val added = newSchedule.values.flatten.count(_.kind == Arrival) -
                    data.schedule.values.flatten.count(_.kind == Arrival)
        ctx.log.info(s"[TowerControl/${slot.name}] ScheduleManager a ajouté $added vol(s) — schedule mis à jour")

        // Détecter les vols existants dont l'heure a changé (retards) — clé = flightId
        val oldByFlightId = data.schedule.values.flatten.toList.groupBy(_.flightId)
        val newDelays: Map[String, Int] = newSchedule.values.flatten.toList.flatMap { nf =>
          oldByFlightId.get(nf.flightId).flatMap(_.headOption).flatMap { of =>
            val delta = java.time.temporal.ChronoUnit.MINUTES
              .between(of.scheduledTime, nf.scheduledTime).toInt
            if (delta > 0) Some(nf.flightId -> delta) else None
          }
        }.toMap

        val updatedDelays = data.delayInfo ++ newDelays
        if (newDelays.nonEmpty)
          ctx.log.info(s"[TowerControl/${slot.name}] Retards appliqués : ${newDelays.map { case (id, d) => s"$id +${d}min" }.mkString(", ")}")

        running(ctx, data.copy(schedule = newSchedule, delayInfo = updatedDelays), slot)

      // ── Annulation de vol(s) par le ScheduleManager ───────────
      case FlightCancelledByManager(cancelled, newSchedule) =>
        ctx.log.warn(s"[TowerControl/${slot.name}] SM annule ${cancelled.size} vol(s) : ${cancelled.map(_.flightId).mkString(", ")}")
        // Ne pas écrire dans flightStates : le dashboard affiche "Annulé" via cancelledFlights
        // (clé = flightId), ce qui évite de contaminer l'état de l'avion entier (ARR + DEP).
        running(ctx, data.copy(
          schedule         = newSchedule,
          cancelledFlights = data.cancelledFlights ++ cancelled
        ), slot)

      // ── BOOM piste (conflit atterrissage) ─────────────────────
      case BoomRunway(runway, planes, smFlights) =>
        ctx.log.warn("=" * 60)
        ctx.log.warn(s"[TowerControl/${slot.name}] 💥 BOOM — CONFLIT PISTE $runway")
        planes.foreach(p => ctx.log.warn(s"  💥 BOOM — avion $p"))
        ctx.log.warn("=" * 60)
        slot.addBoom(s"Collision piste $runway — ${planes.mkString(", ")}")
        running(ctx, applyBoomRunway(ctx, withSmFlights(data, smFlights), runway, planes), slot)

      // ── BOOM taxi (conflit taxi-out / départ) ─────────────────
      case BoomTaxi(runway, planes, smFlights) =>
        ctx.log.warn("=" * 60)
        ctx.log.warn(s"[TowerControl/${slot.name}] 💥 BOOM — CONFLIT TAXI sur voie $runway")
        planes.foreach(p => ctx.log.warn(s"  💥 BOOM — avion $p"))
        ctx.log.warn("=" * 60)
        slot.addBoom(s"Collision taxi $runway — ${planes.mkString(", ")}")
        running(ctx, applyBoomTaxi(ctx, withSmFlights(data, smFlights), runway, planes), slot)

      // ── BOOM garage (débordement gates) ──────────────────────
      case BoomGarage(planes, smFlights) =>
        ctx.log.warn("=" * 60)
        ctx.log.warn(s"[TowerControl/${slot.name}] 💥 BOOM — DÉBORDEMENT GARAGE — tous les avions annulés")
        data.airplanes.keys.foreach(p => ctx.log.warn(s"  💥 BOOM — avion $p"))
        ctx.log.warn("=" * 60)
        slot.addBoom(s"Débordement garage — ${planes.mkString(", ")}")
        running(ctx, applyBoomGarage(ctx, withSmFlights(data, smFlights), planes), slot)

      case RescheduleFlights(newPlan) =>
        ctx.log.info(s"[TowerControl/${slot.name}] Replanification : ${newPlan.size} vol(s) reçus")
        running(ctx, data, slot)

      case DelayFlight(flightId, delayMinutes) =>
        ctx.log.info(s"[TowerControl/${slot.name}] Retard vol $flightId : +$delayMinutes min")
        running(ctx, data, slot)

      case CancelFlight(flightId) =>
        ctx.log.warn(s"[TowerControl/${slot.name}] Annulation vol $flightId")
        running(ctx, data, slot)

      case InjectEmergencyArrival(urgency, targetTime) =>
        ctx.log.warn(s"[TowerControl/${slot.name}] Injection urgence ${urgency.label} pour $targetTime")
        running(ctx, data, slot)

      case InjectRunwayClosure =>
        data.freeRunways.headOption match {
          case Some(runwayId) =>
            ctx.log.warn(s"[TowerControl/${slot.name}] Fermeture piste $runwayId suite à incident injecté")
            data.runways(runwayId) ! RunwayCommand.StormShutdown
            running(ctx, data.copy(freeRunways = data.freeRunways - runwayId), slot)
          case None =>
            ctx.log.warn(s"[TowerControl/${slot.name}] Fermeture piste demandée mais aucune piste libre")
            running(ctx, data, slot)
        }
    }

  // ─────────────────────────────────────────────
  // Spawner les avions dont l'heure d'arrivée est atteinte
  //
  // Parcourt le schedule pour trouver les vols Arrival dont
  // scheduledTime ≤ simTime.toLocalTime et qui n'ont pas encore
  // été instanciés. Crée un AirplaneActor par vol trouvé.
  // ─────────────────────────────────────────────
  private def spawnDueAirplanes(
    ctx:     ActorContext[ControlTowerCommand],
    data:    TowerData,
    simTime: LocalDateTime
  ): TowerData = {
    val simLocalTime = simTime.toLocalTime

    val dueArrivals = data.schedule.toList.flatMap { case (runwayId, flights) =>
      flights.collect {
        case f if f.kind == Arrival
              && !f.scheduledTime.isAfter(simLocalTime)
              && !data.launchedAirplanes.contains(f.airplaneId) =>
          (runwayId, f)
      }
    }

    dueArrivals.foldLeft(data) { case (d, (runwayId, arrival)) =>
      // Trouver l'heure de départ correspondante.
      // Si le départ a été annulé par le SM (dans cancelledFlights), l'avion
      // reste garé jusqu'à la fin de la journée (23:59) — il n'essaie pas de décoller.
      val depCancelled = d.cancelledFlights.exists(f =>
        f.airplaneId == arrival.airplaneId && f.kind == Departure)
      val departureTime =
        if (depCancelled) java.time.LocalTime.of(23, 59)
        else d.schedule.get(runwayId)
          .flatMap(_.find(f => f.airplaneId == arrival.airplaneId && f.kind == Departure))
          .map(_.scheduledTime)
          .getOrElse(arrival.scheduledTime.plusHours(2))

      val flightData = FlightData(
        airplaneId       = arrival.airplaneId,
        flightNumber     = arrival.flightId,
        urgencyLevel     = UrgencyLevel.Commercial,
        scheduledArrival = d.simDate.atTime(arrival.scheduledTime),
        scheduledDepart  = d.simDate.atTime(departureTime)
      )

      val actorName  = s"airplane-${arrival.airplaneId}"
      val airplaneRef = ctx.spawn(AirplaneActor(flightData, ctx.self), actorName)

      ctx.log.info(
        s"[TowerControl ${fmt(d.simTime)}] Avion ${arrival.airplaneId} en approche " +
        s"(arrivée prévue ${arrival.scheduledTime}, départ ${departureTime})"
      )

      d.copy(
        airplanes         = d.airplanes + (arrival.airplaneId -> airplaneRef),
        launchedAirplanes = d.launchedAirplanes + arrival.airplaneId,
        flightStates      = d.flightStates + (arrival.airplaneId -> "En approche")
      )
    }
  }

  // ─────────────────────────────────────────────
  // Notifier le ScheduleManager 30 minutes simulées avant chaque vol
  //
  // Pour chaque vol Arrival dont l'heure planifiée est dans les
  // 30 prochaines minutes simulées et qui n'a pas encore été notifié,
  // envoie PrepareNewFlight au ScheduleManager.
  // ─────────────────────────────────────────────
  // ─────────────────────────────────────────────
  // Notifier le ScheduleManager 30 minutes simulées avant chaque
  // InjectedEvent (événements ajoutés via le dashboard).
  //
  // Le déclencheur = targetTime − 30 min.
  // Dès que simTime atteint ce seuil, PrepareNewFlight est envoyé.
  // notifiedEventIds garantit un envoi unique par événement.
  // ─────────────────────────────────────────────
  private def notifyScheduleManager(
    ctx:  ActorContext[ControlTowerCommand],
    data: TowerData
  ): TowerData = {
    val simMin = data.simTime.toLocalTime.getHour * 60 +
                 data.simTime.toLocalTime.getMinute

    val toNotify = SimState.injectedEventsSnapshot.filter { e =>
      !data.notifiedEventIds.contains(e.id) && {
        val triggerMin = e.targetHour * 60 + e.targetMinute - 30
        triggerMin == simMin
      }
    }

    if (toNotify.isEmpty) data
    else {
      toNotify.foreach { e =>
        ctx.log.info(
          s"[TowerControl ${fmt(data.simTime)}] Événement ${e.id} (${e.eventType.displayName}) " +
          s"dans 30 min → ScheduleManager notifié (arrivée cible ${e.targetTimeStr})"
        )
        // Un événement de type EmergencyArrival est toujours traité comme Emergency
        // quelle que soit la valeur du champ urgencyLevel dans le formulaire.
        val effectiveUrgency =
          if (e.eventType == cysky.model.EventType.EmergencyArrival)
            cysky.model.UrgencyLevel.Emergency
          else e.urgencyLevel
        data.scheduleManagerRef ! ScheduleManagerCommand.PrepareNewFlight(
          e.id, data.simTime, e.targetHour, e.targetMinute, effectiveUrgency
        )
      }
      data.copy(notifiedEventIds = data.notifiedEventIds ++ toNotify.map(_.id))
    }
  }

  // ─────────────────────────────────────────────
  // Vider la file d'atterrissage
  //
  // Tant qu'il y a des pistes libres et des avions en attente :
  //   - Autoriser l'atterrissage (avion + piste)
  //   - Marquer la piste comme occupée
  // Récursif pour traiter plusieurs pistes libres en un seul appel.
  // ─────────────────────────────────────────────
  private def drainLandingQueue(
    ctx:  ActorContext[ControlTowerCommand],
    data: TowerData
  ): TowerData =
    (data.landingQueue, data.freeRunways.headOption) match {
      case (pending :: rest, Some(runwayId)) =>
        val runwayCmd =
          if (pending.emergency) RunwayCommand.EmergencyLandRequest(pending.airplaneId, ctx.self)
          else                   RunwayCommand.LandRequest(pending.airplaneId, ctx.self)
        data.runways(runwayId) ! runwayCmd
        pending.replyTo ! LandingAuthorized(runwayId)
        ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] Atterrissage ${pending.airplaneId} → piste $runwayId")
        drainLandingQueue(ctx, data.copy(
          landingQueue    = rest,
          freeRunways     = data.freeRunways - runwayId,
          runwayOccupancy = data.runwayOccupancy + (runwayId -> pending.airplaneId),
          flightStates    = data.flightStates + (pending.airplaneId -> "Atterrissage en cours")
        ))
      case _ => data
    }

  // ─────────────────────────────────────────────
  // Vider la file de décollage
  //
  // Priorité aux atterrissages : on n'utilise une piste pour
  // un décollage que s'il n'y a aucun atterrissage en attente.
  // ─────────────────────────────────────────────
  // Les atterrissages sont prioritaires : pas de décollage si la file landing est non vide
  private def drainTakeoffQueue(
    ctx:  ActorContext[ControlTowerCommand],
    data: TowerData
  ): TowerData =
    (data.takeoffQueue, data.freeRunways.headOption, data.landingQueue) match {
      case (pending :: rest, Some(runwayId), Nil) =>
        data.runways(runwayId) ! RunwayCommand.TakeoffRequest(pending.airplaneId, ctx.self)
        pending.replyTo ! TakeoffAuthorized(runwayId)
        ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] Décollage ${pending.airplaneId} → piste $runwayId")
        drainTakeoffQueue(ctx, data.copy(
          takeoffQueue     = rest,
          freeRunways      = data.freeRunways - runwayId,
          takeoffOccupancy = data.takeoffOccupancy + (runwayId -> pending.airplaneId),
          flightStates     = data.flightStates + (pending.airplaneId -> "Décollage en cours")
        ))
      case _ => data
    }

  // ─────────────────────────────────────────────
  // Assigner un garage à un avion qui vient d'atterrir
  //
  // Envoie TaxiToGarage à l'avion (transition Landing → Taxiing)
  // puis ParkRequest au garage (le garage envoie ParkConfirmed
  // directement à l'avion, déclenchant la transition Taxiing → Parked).
  // ─────────────────────────────────────────────
  private def assignGarage(
    ctx:        ActorContext[ControlTowerCommand],
    data:       TowerData,
    airplaneId: String
  ): TowerData =
    (data.freeGarages.headOption, data.airplanes.get(airplaneId)) match {
      case (Some(garageId), Some(airplaneRef)) =>
        val garageRef = data.garages(garageId)
        airplaneRef ! TaxiToGarage(garageRef)
        garageRef   ! ParkRequest(airplaneId, airplaneRef)
        ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] $airplaneId stationné au $garageId")
        data.copy(
          freeGarages  = data.freeGarages - garageId,
          flightStates = data.flightStates + (airplaneId -> s"Au sol — $garageId")
        )

      case (None, _) =>
        ctx.log.warn(s"[TowerControl ${fmt(data.simTime)}] Aucun garage disponible pour $airplaneId")
        data

      case (_, None) =>
        ctx.log.error(s"[TowerControl ${fmt(data.simTime)}] ActorRef introuvable pour $airplaneId")
        data
    }

  // ─────────────────────────────────────────────
  // Insertion dans la file d'atterrissage
  // par priorité décroissante (score le plus haut en tête)
  // ─────────────────────────────────────────────
  private def insertByPriority(
    queue:   List[PendingLanding],
    pending: PendingLanding
  ): List[PendingLanding] =
    (pending :: queue).sortBy(_.urgency.priorityScore)(Ordering.Int.reverse)

  // ─────────────────────────────────────────────
  // BOOM helpers
  // ─────────────────────────────────────────────

  /** Injecte les vols du SM plane rejeté dans le schedule du Tower
   *  afin qu'ils apparaissent dans le tableau frontend avec le statut BOOM. */
  private def withSmFlights(data: TowerData, smFlights: List[Flight]): TowerData =
    if (smFlights.isEmpty) data
    else data.copy(
      schedule = smFlights.foldLeft(data.schedule) { (sched, f) =>
        sched.updatedWith(f.runway) {
          case Some(fs) => Some(fs :+ f)
          case None     => Some(List(f))
        }
      }
    )

  /** Un statut final ne doit jamais être écrasé par un statut opérationnel. */
  private def isFinalStatus(s: String): Boolean =
    s == "BOOM" || s == "Annulé" || s == "Parti"

  /** Conflit piste ou taxi.
   *  - BOOM uniquement pour les 2 avions en conflit (planes).
   *  - "Annulé" pour les autres vols FUTURS (pas encore lancés) sur cette piste.
   *  - Les avions actifs (déjà en vol/sol) ne sont PAS touchés.
   *  - Schedule intact pour l'affichage frontend. */
  private def applyBoomRunway(
    ctx:    ActorContext[ControlTowerCommand],
    data:   TowerData,
    runway: String,
    planes: List[String]
  ): TowerData = {
    // Vols planifiés sur cette piste non encore lancés (futurs uniquement)
    val futureUnlaunched = data.schedule.getOrElse(runway, Nil)
      .filter(_.kind == Arrival)
      .map(_.airplaneId)
      .filterNot(data.launchedAirplanes.contains)

    // Divert uniquement les BOOM planes qui sont actives
    val boomActive = planes.filter(data.airplanes.contains)
    boomActive.foreach(id => data.airplanes(id) ! AirplaneCommand.Divert)

    // Libérer TOUS les runways occupés par les avions boom'd (pas seulement runway)
    val boomLandingRwys  = data.runwayOccupancy.collect  { case (r, id) if planes.contains(id) => r }.toSet + runway
    val boomTakeoffRwys  = data.takeoffOccupancy.collect { case (r, id) if planes.contains(id) => r }.toSet + runway

    // États : BOOM pour les 2 ciblés, Annulé pour les autres futurs non lancés
    val boomStates     = planes.map(_ -> "BOOM").toMap
    val cancelStates   = futureUnlaunched.filterNot(planes.contains).map(_ -> "Annulé").toMap
    val affectedPlanes = planes.toSet ++ futureUnlaunched

    data.copy(
      airplanes         = data.airplanes -- boomActive,
      flightStates      = data.flightStates ++ boomStates ++ cancelStates,
      launchedAirplanes = data.launchedAirplanes ++ affectedPlanes,
      freeRunways       = data.freeRunways ++ boomLandingRwys ++ boomTakeoffRwys,
      runwayOccupancy   = data.runwayOccupancy  -- boomLandingRwys,
      takeoffOccupancy  = data.takeoffOccupancy -- boomTakeoffRwys,
      landingQueue      = data.landingQueue.filterNot(p => affectedPlanes(p.airplaneId)),
      takeoffQueue      = data.takeoffQueue.filterNot(p => affectedPlanes(p.airplaneId))
    )
  }

  /** Conflit taxi : même logique (même ressource piste de départ). */
  private def applyBoomTaxi(
    ctx:    ActorContext[ControlTowerCommand],
    data:   TowerData,
    runway: String,
    planes: List[String]
  ): TowerData = applyBoomRunway(ctx, data, runway, planes)

  /** Débordement garage.
   *  - BOOM pour les 2 avions ciblés (planes).
   *  - "Annulé" pour tous les vols FUTURS non encore lancés (toutes pistes).
   *  - Les avions actifs ne sont PAS touchés. */
  private def applyBoomGarage(
    ctx:    ActorContext[ControlTowerCommand],
    data:   TowerData,
    planes: List[String]
  ): TowerData = {
    // Divert uniquement les BOOM planes actives
    val boomActive = planes.filter(data.airplanes.contains)
    boomActive.foreach(id => data.airplanes(id) ! AirplaneCommand.Divert)

    // Libérer tous les runways des avions boom'd
    val boomLandingRwys = data.runwayOccupancy.collect  { case (r, id) if planes.contains(id) => r }.toSet
    val boomTakeoffRwys = data.takeoffOccupancy.collect { case (r, id) if planes.contains(id) => r }.toSet

    // Tous les futurs non lancés (toutes pistes)
    val futureUnlaunched = data.schedule.values.flatten.toList
      .filter(_.kind == Arrival)
      .map(_.airplaneId)
      .filterNot(data.launchedAirplanes.contains)
      .distinct

    val boomStates   = planes.map(_ -> "BOOM").toMap
    val cancelStates = futureUnlaunched.filterNot(planes.contains).map(_ -> "Annulé").toMap
    val affected     = planes.toSet ++ futureUnlaunched

    data.copy(
      airplanes         = data.airplanes -- boomActive,
      flightStates      = data.flightStates ++ boomStates ++ cancelStates,
      launchedAirplanes = data.launchedAirplanes ++ affected,
      freeRunways       = data.freeRunways ++ boomLandingRwys ++ boomTakeoffRwys,
      runwayOccupancy   = data.runwayOccupancy  -- boomLandingRwys,
      takeoffOccupancy  = data.takeoffOccupancy -- boomTakeoffRwys,
      landingQueue      = data.landingQueue.filterNot(p => affected(p.airplaneId)),
      takeoffQueue      = data.takeoffQueue.filterNot(p => affected(p.airplaneId))
    )
  }
}
