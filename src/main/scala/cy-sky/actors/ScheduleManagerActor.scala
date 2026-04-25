package cysky.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.models.{AircraftFlight, Arrival, Departure, Landing, Takeoff}
import cysky.petri.PetriScheduleVerifier
import cysky.petri.PetriScheduleVerifier.{VerificationResult, RunwayConflict, GateOverflow, Deadlock}
import cysky.protocol.{ControlTowerCommand, ScheduleManagerCommand}
import cysky.protocol.ControlTowerCommand.{FlightAddedByManager, FlightCancelledByManager, BoomRunway, BoomTaxi, BoomGarage}
import cysky.SimState
import java.time.{LocalDateTime, LocalTime}
import scala.util.Random

// ═══════════════════════════════════════════════════════════════
// ScheduleManagerActor
//
// Mode Libre :
//   Ajoute le vol directement sans replanification.
//   Conflit Pétri → BOOM immédiat.
//
// Mode Contrôle :
//   Pour chaque piste :
//     1. Tenter le placement sans délai → vérifie Pétri.
//     2. En cas de conflit, identifie le(s) vol(s) bloquants
//        (arrivée OU départ qui occupe la ressource) dont
//        l'heure est encore dans le futur (> simTime).
//     3. Teste des décalages par paliers (+15 … +120 min) sur
//        ces vols, re-vérifie chaque fois avec le Pétri.
//     4. Si aucune piste ni aucun délai ne sont viables → annulation.
// ═══════════════════════════════════════════════════════════════
object ScheduleManagerActor {

  sealed trait Mode
  case object Libre    extends Mode
  case object Controle extends Mode

  // ── Paliers de décalage testés (minutes) ─────────────────────
  private val DelaySteps          = List(15, 30, 45, 60, 90, 120)
  // Pour les vols d'urgence : décalages plus agressifs des bloquants
  private val EmergencyDelaySteps = List(15, 30, 45, 60, 90, 120, 150, 180, 240)

  // Un résultat est acceptable si toutes les issues restantes sont des GateOverflow.
  // Le GateOverflow ne cause pas de BOOM : la simulation assigne les gates
  // dynamiquement par file d'attente. Seul un RunwayConflict est un vrai danger.
  private def isAcceptable(r: VerificationResult): Boolean =
    r.valid || r.issues.forall(_.isInstanceOf[GateOverflow])


  def apply(
    towerRef    : ActorRef[ControlTowerCommand],
    schedule    : Map[String, List[AircraftFlight]],
    runwayIds   : List[String],
    runwayCount : Int,
    garageCount : Int,
    mode        : Mode = Libre
  ): Behavior[ScheduleManagerCommand] =
    running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode,
            counter = 0, rng = new Random(), emergencyAirplaneIds = Set.empty)

  private def running(
    towerRef             : ActorRef[ControlTowerCommand],
    schedule             : Map[String, List[AircraftFlight]],
    runwayIds            : List[String],
    runwayCount          : Int,
    garageCount          : Int,
    mode                 : Mode,
    counter              : Int,
    rng                  : Random,
    emergencyAirplaneIds : Set[String]   // airplaneIds des urgences — jamais décalés
  ): Behavior[ScheduleManagerCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case ScheduleManagerCommand.PrepareNewFlight(triggeringEventId, simTime, arrivalHour, arrivalMinute, urgencyLevel) =>
          val newCounter    = counter + 1
          val isEmergency   = urgencyLevel == cysky.model.UrgencyLevel.Emergency
          val arrivalTime   = LocalTime.of(arrivalHour, arrivalMinute)
          val departureTime = arrivalTime.plusHours(1)

          if (arrivalTime.isAfter(LocalTime.of(22, 59))) {
            ctx.log.info(s"[ScheduleManager] Événement $triggeringEventId ignoré — heure cible trop tardive")
            running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng, emergencyAirplaneIds)
          } else {
            val airplaneId = s"SM_PLANE_$newCounter"
            val dest       = randomDestination(rng)

            // Squelettes — la piste sera affectée dans findValidPlacement
            val baseArrival = AircraftFlight(
              flightId      = s"SM_ARR_$newCounter",
              airplaneId    = airplaneId,
              runway        = "",
              scheduledTime = arrivalTime,
              destination   = dest,
              kind          = Arrival
            )
            val baseDep = AircraftFlight(
              flightId      = s"SM_DEP_$newCounter",
              airplaneId    = airplaneId,
              runway        = "",
              scheduledTime = departureTime,
              destination   = dest,
              kind          = Departure
            )

            mode match {

              // ─── Mode Libre : vérification Pétri sans replanification
              //     Conflit → BOOM immédiat (pas de tentative de décalage)
              case Libre =>
                val runway    = runwayIds(rng.nextInt(runwayIds.size))
                val arr       = baseArrival.copy(runway = runway)
                val dep       = baseDep.copy(runway = runway)
                val candidate = addToSchedule(schedule, runway, arr, dep)
                val result    = PetriScheduleVerifier.verify(candidate, runwayCount, garageCount)

                if (result.valid) {
                  ctx.log.info(
                    s"[SM/Libre ✓] SM_ARR_$newCounter/$airplaneId → $runway" +
                    s" arrivée ${fmt(arrivalTime)}, départ ${fmt(departureTime)}"
                  )
                  SimState.setSchedule(candidate)
                  towerRef ! FlightAddedByManager(candidate)
                  running(towerRef, candidate, runwayIds, runwayCount, garageCount, mode, newCounter, rng, emergencyAirplaneIds)
                } else {
                  ctx.log.warn(s"[SM/Libre ✗] Conflit pour SM_ARR_$newCounter → BOOM (mode libre, pas de replanification)")
                  result.issues.foreach(i => ctx.log.warn(s"  ${i.msg}"))
                  sendBoom(towerRef, airplaneId, schedule, result, arr, dep)
                  running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng, emergencyAirplaneIds)
                }

              // ─── Mode Contrôle : placement intelligent ───────────
              case Controle =>
                // Si le vol est une urgence, enregistrer son airplaneId pour que
                // les futurs placements ne tentent JAMAIS de le décaler.
                val updatedEmergencyIds =
                  if (isEmergency) emergencyAirplaneIds + airplaneId
                  else emergencyAirplaneIds

                if (isEmergency) {
                  ctx.log.warn(s"[SM/URGENCE] Placement $airplaneId — urgences connues: $emergencyAirplaneIds — pistes: $runwayIds")
                  ctx.log.warn(s"[SM/URGENCE] Vols sur schedule par piste: ${schedule.map { case (r, fs) => s"$r=${fs.map(f => s"${f.airplaneId}@${f.scheduledTime}(${f.kind})").mkString(",")}" }.mkString(" | ")}")
                }

                findValidPlacement(
                  schedule, runwayIds, airplaneId, newCounter,
                  baseArrival, baseDep,
                  runwayCount, garageCount,
                  simTime.toLocalTime,
                  isEmergency,
                  updatedEmergencyIds,
                  if (isEmergency) Some(ctx.log) else None
                ) match {

                  case Right((newSchedule, logMsg)) =>
                    ctx.log.info(s"[SM/Controle ✓] $logMsg")
                    SimState.setSchedule(newSchedule)
                    towerRef ! FlightAddedByManager(newSchedule)
                    running(towerRef, newSchedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng, updatedEmergencyIds)

                  case Left((_, lastRunway)) =>
                    // Toutes les tentatives ARR+DEP ont échoué.
                    // Un vol d'urgence ne peut jamais être annulé ou retardé :
                    // cette branche ne devrait pas être atteinte pour une urgence
                    // (findValidPlacement force le placement), mais par sécurité on
                    // force quand même ici plutôt que d'annuler.
                    if (isEmergency) {
                      ctx.log.warn(s"[SM/Controle URGENCE] Placement forcé sur $lastRunway pour SM_ARR_$newCounter")
                      val arr = baseArrival.copy(runway = lastRunway)
                      val dep = baseDep.copy(runway = lastRunway)
                      val forced = addToSchedule(schedule, lastRunway, arr, dep)
                      SimState.setSchedule(forced)
                      towerRef ! FlightAddedByManager(forced)
                      running(towerRef, forced, runwayIds, runwayCount, garageCount, mode, newCounter, rng, updatedEmergencyIds)
                    } else {
                      // Essayer de placer l'atterrissage seul (DEP annulé — gate occupé).
                      ctx.log.warn(s"[SM/Controle ✗] Aucun placement ARR+DEP viable pour SM_ARR_$newCounter")
                      val arrOnlyResult =
                        runwayIds.view.flatMap { rwy =>
                          val a = baseArrival.copy(runway = rwy)
                          val d = baseDep.copy(runway = rwy)
                          val candidate = addArrivalOnlyToSchedule(schedule, rwy, a)
                          val res = PetriScheduleVerifier.verify(candidate, runwayCount, garageCount)
                          Option.when(res.valid)((candidate, rwy, a, d))
                        }.headOption

                      arrOnlyResult match {
                        case Some((newSched, rwy, arr, dep)) =>
                          ctx.log.warn(s"[SM/Controle ~] ARR seul placé sur $rwy — DEP annulé (gate occupé)")
                          SimState.setSchedule(newSched)
                          towerRef ! FlightCancelledByManager(List(dep), newSched)
                          running(towerRef, newSched, runwayIds, runwayCount, garageCount, mode, newCounter, rng, emergencyAirplaneIds)
                        case None =>
                          ctx.log.warn(s"[SM/Controle ✗] Impossible de placer l'ARR — vol SM_ARR_$newCounter totalement annulé")
                          val arr = baseArrival.copy(runway = lastRunway)
                          val dep = baseDep.copy(runway = lastRunway)
                          towerRef ! FlightCancelledByManager(List(arr, dep), schedule)
                          running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng, emergencyAirplaneIds)
                      }
                    }
                }
            }
          }
      }
    }

  // ═══════════════════════════════════════════════════════════════
  // findValidPlacement
  //
  // Parcourt chaque piste :
  //   1. Tente sans délai.
  //   2. Si conflit, identifie les vols bloquants futurs (arrivée
  //      OU départ qui occupent la piste/gate) et teste des
  //      décalages croissants.
  //
  // Retourne :
  //   Right((schedule, logMsg))        placement trouvé
  //   Left((lastResult, lastRunway))   toutes pistes épuisées
  // ═══════════════════════════════════════════════════════════════
  private def findValidPlacement(
    schedule             : Map[String, List[AircraftFlight]],
    runwayIds            : List[String],
    airplaneId           : String,
    counter              : Int,
    baseArrival          : AircraftFlight,
    baseDep              : AircraftFlight,
    runwayCount          : Int,
    garageCount          : Int,
    simTime              : LocalTime,
    isEmergency          : Boolean     = false,
    emergencyAirplaneIds : Set[String] = Set.empty,
    dbg                  : Option[org.slf4j.Logger] = None
  ): Either[(VerificationResult, String), (Map[String, List[AircraftFlight]], String)] = {

    // Paliers utilisés pour décaler les bloquants — plus agressifs pour une urgence.
    val blockerSteps = if (isEmergency) EmergencyDelaySteps else DelaySteps

    // Pour une urgence : trier les pistes de façon à essayer en PREMIER celles
    // qui n'ont pas déjà une autre urgence planifiée.
    // Cela évite d'empiler deux urgences sur la même piste quand une piste libre existe.
    val orderedRunways = if (isEmergency) {
      val (free, occupied) = runwayIds.partition { rwy =>
        schedule.getOrElse(rwy, Nil).forall(f => !emergencyAirplaneIds.contains(f.airplaneId))
      }
      dbg.foreach(_.warn(s"[SM/DBG] orderedRunways: sans-urgence=$free avec-urgence=$occupied"))
      free ++ occupied
    } else runwayIds

    val found: Option[(Map[String, List[AircraftFlight]], String)] =
      orderedRunways.view.flatMap { runway =>
        val arr = baseArrival.copy(runway = runway)
        val dep = baseDep.copy(runway = runway)

        // ── Tentative 0 : sans délai ─────────────────────────────
        // Valide ou seulement GateOverflow (pas de RunwayConflict) → accepté.
        lazy val t0: Option[(Map[String, List[AircraftFlight]], String)] = {
          val c = addToSchedule(schedule, runway, arr, dep)
          val r = PetriScheduleVerifier.verify(c, runwayCount, garageCount)
          val ok = isAcceptable(r)
          dbg.foreach(_.warn(s"[SM/DBG] t0 $runway ok=$ok issues=${r.issues.map(_.msg).mkString("|")}"))
          Option.when(ok)((c,
            s"SM_ARR_$counter/$airplaneId → $runway (sans délai)" +
            s" arrivée ${fmt(arr.scheduledTime)}, départ ${fmt(dep.scheduledTime)}"))
        }

        // ── Tentative 1 : décaler les vols BLOQUANTS (piste seulement) ──
        // Résout les RunwayConflict sans toucher les occupants de gate.
        // Les vols d'urgence déjà planifiés (emergencyAirplaneIds) ne sont
        // JAMAIS décalés : si le seul bloquant est une urgence, t1 échoue
        // et on passe à la piste suivante (t0 y trouvera peut-être un créneau libre).
        lazy val t1: Option[(Map[String, List[AircraftFlight]], String)] = {
          val c0 = addToSchedule(schedule, runway, arr, dep)
          val r0 = PetriScheduleVerifier.verify(c0, runwayCount, garageCount)
          val allBlockers = extractRunwayBlockingIds(c0, r0, airplaneId, simTime)
          val blockers = allBlockers.filterNot(emergencyAirplaneIds.contains)
          dbg.foreach(_.warn(s"[SM/DBG] t1 $runway allBlockers=$allBlockers filteredBlockers=$blockers emergencyIds=$emergencyAirplaneIds"))
          if (blockers.isEmpty) None
          else {
            val who = blockers.mkString(", ")
            blockerSteps.view.flatMap { deltaMin =>
              val rescheduled = blockers.foldLeft(schedule)(delayFlight(_, _, deltaMin, simTime))
              val c = addToSchedule(rescheduled, runway, arr, dep)
              val r = PetriScheduleVerifier.verify(c, runwayCount, garageCount)
              val ok = isAcceptable(r)
              dbg.foreach(_.warn(s"[SM/DBG] t1 $runway +${deltaMin}min ok=$ok issues=${r.issues.map(_.msg).mkString("|")}"))
              Option.when(ok)((c,
                s"SM_ARR_$counter/$airplaneId → $runway (+${deltaMin}min sur $who)" +
                s" arrivée ${fmt(arr.scheduledTime)}, départ ${fmt(dep.scheduledTime)}"))
            }.headOption
          }
        }

        // ── Tentative 2 : reculer l'atterrissage du NOUVEAU vol ──────
        // NON applicable pour un vol d'urgence : on ne retarde jamais une urgence.
        // Pour un vol normal : décale arr+dep jusqu'à trouver un créneau libre.
        lazy val t2: Option[(Map[String, List[AircraftFlight]], String)] =
          if (isEmergency) None
          else DelaySteps.view.flatMap { deltaMin =>
            val arrS = arr.copy(scheduledTime = arr.scheduledTime.plusMinutes(deltaMin))
            val depS = dep.copy(scheduledTime = dep.scheduledTime.plusMinutes(deltaMin))
            if (depS.scheduledTime.isAfter(LocalTime.of(22, 59))) None
            else {
              val c = addToSchedule(schedule, runway, arrS, depS)
              val r = PetriScheduleVerifier.verify(c, runwayCount, garageCount)
              Option.when(r.valid)((c,
                s"SM_ARR_$counter/$airplaneId → $runway (atterrissage décalé +${deltaMin}min)" +
                s" arrivée ${fmt(arrS.scheduledTime)}, départ ${fmt(depS.scheduledTime)}"))
            }
          }.headOption

        t0 orElse t1 orElse t2
      }.headOption

    found match {
      case Some(result) => Right(result)

      // ── Urgence sans placement valide trouvé : forcer sur la piste la moins chargée ──
      // Règle de priorité pour choisir la piste :
      //   1. Préférer une piste dont le seul bloquant est un VOL NORMAL (décalable).
      //      Une piste dont le bloquant est une autre urgence (indécalable) est évitée.
      //   2. À égalité, prendre la piste avec le moins de bloquants décalables.
      //   3. En dernier recours, n'importe quelle piste.
      case None if isEmergency =>
        val runwayOptions = runwayIds.map { rwy =>
          val a  = baseArrival.copy(runway = rwy)
          val d  = baseDep.copy(runway = rwy)
          val c  = addToSchedule(schedule, rwy, a, d)
          val r  = PetriScheduleVerifier.verify(c, runwayCount, garageCount)
          val allBlockers       = extractRunwayBlockingIds(c, r, airplaneId, simTime)
          val delayableBlockers = allBlockers.filterNot(emergencyAirplaneIds.contains)
          val hasEmergencyBlocker = allBlockers.exists(emergencyAirplaneIds.contains)
          (rwy, delayableBlockers, hasEmergencyBlocker)
        }

        // Priorité 1 : pistes sans urgence bloquante (bloquants décalables seulement)
        val preferred = runwayOptions.filter(!_._3)
        val (bestRunway, delayableBlockers, _) =
          preferred.minByOption(_._2.size)
            .orElse(runwayOptions.minByOption(_._2.size))
            .getOrElse((runwayIds.head, Nil, false))

        val arr = baseArrival.copy(runway = bestRunway)
        val dep = baseDep.copy(runway = bestRunway)
        val forcedSchedule =
          if (delayableBlockers.nonEmpty) {
            val maxDelay    = EmergencyDelaySteps.last
            val rescheduled = delayableBlockers.foldLeft(schedule)(delayFlight(_, _, maxDelay, simTime))
            addToSchedule(rescheduled, bestRunway, arr, dep)
          } else {
            addToSchedule(schedule, bestRunway, arr, dep)
          }
        Right((forcedSchedule,
          s"SM_ARR_$counter/$airplaneId → $bestRunway [URGENCE FORCÉE — bloquants repoussés]" +
          s" arrivée ${fmt(arr.scheduledTime)}, départ ${fmt(dep.scheduledTime)}"))

      case None =>
        val lastRunway = runwayIds.last
        val lastResult = PetriScheduleVerifier.verify(
          addToSchedule(schedule, lastRunway,
            baseArrival.copy(runway = lastRunway), baseDep.copy(runway = lastRunway)),
          runwayCount, garageCount
        )
        Left((lastResult, lastRunway))
    }
  }

  // ═══════════════════════════════════════════════════════════════
  // extractRunwayBlockingIds
  //
  // Variante de extractBlockingIds qui ne traite QUE les RunwayConflict.
  // Les GateOverflow sont délibérément ignorés : décaler les occupants
  // de gate les maintient plus longtemps en gate (aggrave le problème),
  // et crée de nouveaux conflits en cascade entre vols T1 dos-à-dos.
  // Le GateOverflow résiduel est accepté via isAcceptable.
  // ═══════════════════════════════════════════════════════════════
  private def extractRunwayBlockingIds(
    candidate : Map[String, List[AircraftFlight]],
    result    : VerificationResult,
    newId     : String,
    simTime   : LocalTime
  ): List[String] =
    result.issues.flatMap {

      case RunwayConflict(atMin, rwy, _, "LAND") =>
        val flights = candidate.getOrElse(rwy, Nil).filter(_.airplaneId != newId)
        val byLanding = flights
          .filter(f => f.kind == Arrival && f.scheduledTime.isAfter(simTime))
          .find { f => val s = toMin(f.scheduledTime); s <= atMin && atMin < s + Landing.durationMinutes }
          .map(_.airplaneId)
        val byTakeoff = flights
          .filter(f => f.kind == Departure && f.scheduledTime.isAfter(simTime))
          .find { f => val t = toMin(f.scheduledTime); (t - Takeoff.durationMinutes) <= atMin && atMin < (t + Takeoff.durationMinutes) }
          .map(_.airplaneId)
        byLanding.orElse(byTakeoff)

      case RunwayConflict(atMin, rwy, _, "TAXI") =>
        val flights = candidate.getOrElse(rwy, Nil).filter(_.airplaneId != newId)
        flights.find { f =>
          val t = toMin(f.scheduledTime)
          f.kind match {
            case Arrival   => t <= atMin && atMin < t + Landing.durationMinutes
            case Departure => val ts = t - Takeoff.durationMinutes; ts <= atMin && atMin < t + Takeoff.durationMinutes
          }
        }.filter(_.scheduledTime.isAfter(simTime)).map(_.airplaneId)

      case _ => None  // GateOverflow et Deadlock ignorés intentionnellement
    }.distinct.filterNot(_ == newId)

  // ═══════════════════════════════════════════════════════════════
  // extractBlockingIds
  //
  // Identifie les airplaneIds qui bloquent la ressource au moment
  // du conflit Pétri.
  //
  // IMPORTANT : un conflit de type "LAND" peut être causé par un
  // ATTERRISSAGE en cours OU par un DÉCOLLAGE en cours (qui occupe
  // la même piste). Les deux cas sont traités.
  //
  // Seuls les vols dont l'heure est encore dans le futur (>simTime)
  // sont retournés — on ne peut pas décaler un vol déjà passé.
  // ═══════════════════════════════════════════════════════════════
  private def extractBlockingIds(
    candidate : Map[String, List[AircraftFlight]],
    result    : VerificationResult,
    newId     : String,
    simTime   : LocalTime
  ): List[String] =
    result.issues.flatMap {

      case RunwayConflict(atMin, rwy, _, "LAND") =>
        val flights = candidate.getOrElse(rwy, Nil).filter(_.airplaneId != newId)

        val byLanding: Option[String] = flights
          .filter(f => f.kind == Arrival && f.scheduledTime.isAfter(simTime))
          .find { f =>
            val s = toMin(f.scheduledTime)
            s <= atMin && atMin < s + Landing.durationMinutes
          }
          .map(_.airplaneId)

        val byTakeoff: Option[String] = flights
          .filter(f => f.kind == Departure && f.scheduledTime.isAfter(simTime))
          .find { f =>
            val t = toMin(f.scheduledTime)
            (t - Takeoff.durationMinutes) <= atMin && atMin < (t + Takeoff.durationMinutes)
          }
          .map(_.airplaneId)

        byLanding.orElse(byTakeoff)

      case RunwayConflict(atMin, rwy, _, "TAXI") =>
        val flights = candidate.getOrElse(rwy, Nil).filter(_.airplaneId != newId)

        flights.find { f =>
          val t = toMin(f.scheduledTime)
          f.kind match {
            case Arrival   => t <= atMin && atMin < t + Landing.durationMinutes
            case Departure =>
              val ts = t - Takeoff.durationMinutes
              ts <= atMin && atMin < t + Takeoff.durationMinutes
          }
        }
        .filter(_.scheduledTime.isAfter(simTime))
        .map(_.airplaneId)

      case GateOverflow(atMin, _, _, _) =>
        val allFlights = candidate.values.flatten.toList
        allFlights
          .filter(f => f.kind == Arrival && f.airplaneId != newId)
          .flatMap { arr =>
            allFlights
              .find(d => d.kind == Departure
                      && d.airplaneId == arr.airplaneId
                      && d.scheduledTime.isAfter(simTime))
              .filter { dep =>
                val landEnd = toMin(arr.scheduledTime) + Landing.durationMinutes
                val taxiOut = toMin(dep.scheduledTime) - Takeoff.durationMinutes
                landEnd <= atMin && atMin <= taxiOut
              }
              .map(_ => arr.airplaneId)
          }

      case Deadlock(_, _)             => None
      case RunwayConflict(_, _, _, _) => None
    }.distinct.filterNot(_ == newId)

  // ═══════════════════════════════════════════════════════════════
  // delayFlight
  //
  // Décale les horaires d'un avion dans le schedule.
  // Seuls les vols dont scheduledTime > simTime sont modifiés.
  // ═══════════════════════════════════════════════════════════════
  private def delayFlight(
    schedule  : Map[String, List[AircraftFlight]],
    airplaneId: String,
    deltaMin  : Int,
    simTime   : LocalTime
  ): Map[String, List[AircraftFlight]] =
    schedule.view.mapValues { flights =>
      flights.map { f =>
        if (f.airplaneId == airplaneId && f.scheduledTime.isAfter(simTime))
          f.copy(scheduledTime = f.scheduledTime.plusMinutes(deltaMin))
        else f
      }
    }.toMap

  // ═══════════════════════════════════════════════════════════════
  // sendBoom — détermine le type de BOOM et le transmet à la tour.
  // ═══════════════════════════════════════════════════════════════
  private def sendBoom(
    towerRef   : ActorRef[ControlTowerCommand],
    airplaneId : String,
    schedule   : Map[String, List[AircraftFlight]],
    result     : VerificationResult,
    arrival    : AircraftFlight,
    departure  : AircraftFlight
  ): Unit =
    result.issues.headOption match {

      case Some(RunwayConflict(atMin, rwy, _, "LAND")) =>
        val other = schedule.getOrElse(rwy, Nil)
          .filter(f => f.airplaneId != airplaneId)
          .find { f =>
            val t = toMin(f.scheduledTime)
            f.kind match {
              case Arrival   => t <= atMin && atMin <= t + Landing.durationMinutes
              case Departure =>
                (t - Takeoff.durationMinutes) <= atMin && atMin < (t + Takeoff.durationMinutes)
            }
          }
          .map(_.airplaneId)
        val boomPlanes = (airplaneId :: other.toList).distinct
        towerRef ! BoomRunway(rwy, boomPlanes,
          List(arrival.copy(runway = rwy), departure.copy(runway = rwy)))

      case Some(RunwayConflict(atMin, rwy, _, "TAXI")) =>
        val other = schedule.getOrElse(rwy, Nil)
          .filter(f => f.airplaneId != airplaneId)
          .find { f =>
            val t = toMin(f.scheduledTime)
            f.kind match {
              case Arrival   => t <= atMin && atMin <= t + Landing.durationMinutes
              case Departure =>
                val taxiStart = t - Takeoff.durationMinutes
                taxiStart <= atMin && atMin <= t + Takeoff.durationMinutes
            }
          }
          .map(_.airplaneId)
        val boomPlanes = (airplaneId :: other.toList).distinct
        towerRef ! BoomTaxi(rwy, boomPlanes,
          List(arrival.copy(runway = rwy), departure.copy(runway = rwy)))

      case Some(GateOverflow(_, _, _, _)) | Some(Deadlock(_, _)) | _ =>
        val existing = schedule.values.flatten.toList
          .filter(_.kind == Arrival).map(_.airplaneId).distinct
        val boomPlanes = (airplaneId :: existing).distinct
        towerRef ! BoomGarage(boomPlanes, List(arrival, departure))
    }

  // ── Helpers ───────────────────────────────────────────────────

  private def addToSchedule(
    schedule: Map[String, List[AircraftFlight]],
    runway  : String,
    arr     : AircraftFlight,
    dep     : AircraftFlight
  ): Map[String, List[AircraftFlight]] =
    schedule.updatedWith(runway) {
      case Some(fs) => Some(fs :+ arr :+ dep)
      case None     => Some(List(arr, dep))
    }

  private def addArrivalOnlyToSchedule(
    schedule: Map[String, List[AircraftFlight]],
    runway  : String,
    arr     : AircraftFlight
  ): Map[String, List[AircraftFlight]] =
    schedule.updatedWith(runway) {
      case Some(fs) => Some(fs :+ arr)
      case None     => Some(List(arr))
    }

  private def toMin(t: LocalTime): Int = t.getHour * 60 + t.getMinute

  private val HHmm = java.time.format.DateTimeFormatter.ofPattern("HH:mm")
  private def fmt(t: LocalTime): String = t.format(HHmm)

  private val destinations = Vector(
    "CDG", "JFK", "LHR", "AMS", "FRA", "MAD", "FCO", "IST",
    "DXB", "SIN", "NRT", "LAX", "ORD", "MUC", "BCN"
  )
  private def randomDestination(rng: Random): String =
    destinations(rng.nextInt(destinations.length))
}
