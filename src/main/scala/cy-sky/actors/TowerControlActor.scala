package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import cysky.model._
import cysky.models.{Flight, Arrival, Departure}
import cysky.protocol._
import cysky.protocol.ControlTowerCommand._
import cysky.protocol.AirplaneCommand.{LandingAuthorized, TakeoffAuthorized, TaxiToGarage}
import cysky.protocol.GarageCommand.ParkRequest
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

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
    runways:           Map[String, ActorRef[RunwayCommand]],
    garages:           Map[String, ActorRef[GarageCommand]],
    airplanes:         Map[String, ActorRef[AirplaneCommand]],
    schedule:          Map[String, List[Flight]],
    landingQueue:      List[PendingLanding],
    takeoffQueue:      List[PendingTakeoff],
    runwayOccupancy:   Map[String, String],   // runwayId -> airplaneId (atterrissages)
    takeoffOccupancy:  Map[String, String],   // runwayId -> airplaneId (décollages)
    freeRunways:       Set[String],
    freeGarages:       Set[String],
    launchedAirplanes: Set[String],
    simDate:           LocalDate,
    simTime:           LocalDateTime          // heure simulée courante
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
    simDate:     LocalDate
  ): Behavior[ControlTowerCommand] =
    Behaviors.setup { ctx =>
      // Spawn des pistes — ctx.self est l'ActorRef[ControlTowerCommand]
      val runways: Map[String, ActorRef[RunwayCommand]] =
        (1 to runwayCount).map { i =>
          val id = s"RWY_$i"
          id -> ctx.spawn(RunwayActor(id, ctx.self), s"runway-$id")
        }.toMap

      // Spawn des garages
      val garages: Map[String, ActorRef[GarageCommand]] =
        (1 to garageCount).map { j =>
          val id = s"GATE_$j"
          id -> ctx.spawn(GarageActor(id, ctx.self), s"garage-$id")
        }.toMap

      val totalFlights = schedule.values.flatten.count(_.kind == Arrival)
      ctx.log.info(
        s"[TowerControl] Démarrage simulation du $simDate — " +
        s"${runways.size} piste(s), ${garages.size} garage(s), $totalFlights vol(s) planifiés"
      )

      val data = TowerData(
        runways           = runways,
        garages           = garages,
        airplanes         = Map.empty,
        schedule          = schedule,
        landingQueue      = List.empty,
        takeoffQueue      = List.empty,
        runwayOccupancy   = Map.empty,
        takeoffOccupancy  = Map.empty,
        freeRunways       = runways.keySet,
        freeGarages       = garages.keySet,
        launchedAirplanes = Set.empty,
        simDate           = simDate,
        simTime           = simDate.atTime(6, 0)
      )
      running(ctx, data)
    }

  // ─────────────────────────────────────────────
  // Comportement principal — machine à état unique
  // (pas de sous-états car la ControlTower doit
  //  toujours rester réactive à tous les messages)
  // ─────────────────────────────────────────────
  private def running(
    ctx:  ActorContext[ControlTowerCommand],
    data: TowerData
  ): Behavior[ControlTowerCommand] =
    Behaviors.receiveMessage {

      // ── Tick horloge simulée ──────────────────────────────────
      case Tick(simTime) =>
        val d1 = spawnDueAirplanes(ctx, data.copy(simTime = simTime), simTime)
        d1.runways.values.foreach(_ ! RunwayCommand.Tick(simTime))
        d1.garages.values.foreach(_ ! GarageCommand.Tick(simTime))
        d1.airplanes.values.foreach(_ ! AirplaneCommand.Tick(simTime))
        running(ctx, d1)

      // ── Demande d'atterrissage standard ──────────────────────
      case RequestLanding(airplaneId, urgency, replyTo) =>
        ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] Demande atterrissage $airplaneId (urgence: ${urgency.label})")
        val pending = PendingLanding(airplaneId, urgency, replyTo)
        val d1 = data.copy(landingQueue = insertByPriority(data.landingQueue, pending))
        running(ctx, drainLandingQueue(ctx, d1))

      // ── Atterrissage d'urgence ────────────────────────────────
      case EmergencyLand(airplaneId, replyTo) =>
        ctx.log.warn(s"[TowerControl ${fmt(data.simTime)}] URGENCE $airplaneId — passage en tête de file")
        val pending = PendingLanding(airplaneId, UrgencyLevel.Emergency, replyTo, emergency = true)
        val d1 = data.copy(landingQueue = pending :: data.landingQueue)
        running(ctx, drainLandingQueue(ctx, d1))

      // ── Demande de décollage ──────────────────────────────────
      case RequestTakeoff(airplaneId, replyTo) =>
        ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] Demande décollage $airplaneId")
        val d1 = data.copy(takeoffQueue = data.takeoffQueue :+ PendingTakeoff(airplaneId, replyTo))
        running(ctx, drainTakeoffQueue(ctx, d1))

      // ── Piste libérée ─────────────────────────────────────────
      case RunwayFreed(runwayId) =>
        val justLandedId   = data.runwayOccupancy.get(runwayId)
        val justDepartedId = data.takeoffOccupancy.get(runwayId)
        justDepartedId.foreach(id => ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] $id a décollé — piste $runwayId libre"))
        val d1 = data.copy(
          freeRunways      = data.freeRunways + runwayId,
          runwayOccupancy  = data.runwayOccupancy  - runwayId,
          takeoffOccupancy = data.takeoffOccupancy - runwayId,
          airplanes        = justDepartedId.fold(data.airplanes)(data.airplanes - _)
        )
        val d2 = justLandedId.fold(d1)(id => assignGarage(ctx, d1, id))
        val d3 = drainLandingQueue(ctx, d2)
        running(ctx, drainTakeoffQueue(ctx, d3))

      // ── Garage libéré ─────────────────────────────────────────
      case GarageFreed(garageId) =>
        ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] Garage $garageId libéré")
        running(ctx, data.copy(freeGarages = data.freeGarages + garageId))

      // ── Replanification ───────────────────────────────────────
      case RescheduleFlights(newPlan) =>
        ctx.log.info(s"[TowerControl] Replanification : ${newPlan.size} vol(s) reçus")
        running(ctx, data)

      case DelayFlight(flightId, delayMinutes) =>
        ctx.log.info(s"[TowerControl] Retard vol $flightId : +$delayMinutes min")
        running(ctx, data)

      case CancelFlight(flightId) =>
        ctx.log.warn(s"[TowerControl] Annulation vol $flightId")
        running(ctx, data)
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
      // Trouver l'heure de départ correspondante
      val departureTime = d.schedule.get(runwayId)
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
        launchedAirplanes = d.launchedAirplanes + arrival.airplaneId
      )
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
  ): TowerData = {
    if (data.landingQueue.isEmpty || data.freeRunways.isEmpty) return data

    val pending   = data.landingQueue.head
    val runwayId  = data.freeRunways.head
    val runwayRef = data.runways(runwayId)

    val runwayCmd =
      if (pending.emergency)
        RunwayCommand.EmergencyLandRequest(pending.airplaneId, ctx.self)
      else
        RunwayCommand.LandRequest(pending.airplaneId, ctx.self)

    runwayRef    ! runwayCmd
    pending.replyTo ! LandingAuthorized(runwayId)

    ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] Atterrissage ${pending.airplaneId} → piste $runwayId")

    val d1 = data.copy(
      landingQueue    = data.landingQueue.tail,
      freeRunways     = data.freeRunways - runwayId,
      runwayOccupancy = data.runwayOccupancy + (runwayId -> pending.airplaneId)
    )
    drainLandingQueue(ctx, d1)
  }

  // ─────────────────────────────────────────────
  // Vider la file de décollage
  //
  // Priorité aux atterrissages : on n'utilise une piste pour
  // un décollage que s'il n'y a aucun atterrissage en attente.
  // ─────────────────────────────────────────────
  private def drainTakeoffQueue(
    ctx:  ActorContext[ControlTowerCommand],
    data: TowerData
  ): TowerData = {
    if (data.takeoffQueue.isEmpty || data.freeRunways.isEmpty) return data
    // Les atterrissages sont prioritaires sur les décollages
    if (data.landingQueue.nonEmpty) return data

    val pending   = data.takeoffQueue.head
    val runwayId  = data.freeRunways.head
    val runwayRef = data.runways(runwayId)

    runwayRef       ! RunwayCommand.TakeoffRequest(pending.airplaneId, ctx.self)
    pending.replyTo ! TakeoffAuthorized(runwayId)

    ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] Décollage ${pending.airplaneId} → piste $runwayId")

    val d1 = data.copy(
      takeoffQueue     = data.takeoffQueue.tail,
      freeRunways      = data.freeRunways - runwayId,
      takeoffOccupancy = data.takeoffOccupancy + (runwayId -> pending.airplaneId)
    )
    drainTakeoffQueue(ctx, d1)
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
  ): TowerData = {
    if (data.freeGarages.isEmpty) {
      ctx.log.warn(s"[TowerControl ${fmt(data.simTime)}] Aucun garage disponible pour $airplaneId — avion en attente")
      return data
    }

    val garageId    = data.freeGarages.head
    val garageRef   = data.garages(garageId)
    val airplaneRef = data.airplanes.get(airplaneId)

    airplaneRef match {
      case Some(ref) =>
        ref ! TaxiToGarage(garageRef)
        garageRef ! ParkRequest(airplaneId, ref)
        ctx.log.info(s"[TowerControl ${fmt(data.simTime)}] $airplaneId stationné au $garageId")
        data.copy(freeGarages = data.freeGarages - garageId)

      case None =>
        ctx.log.error(s"[TowerControl ${fmt(data.simTime)}] ActorRef introuvable pour $airplaneId")
        data
    }
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
}
