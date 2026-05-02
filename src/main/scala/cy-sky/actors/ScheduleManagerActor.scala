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

/***
 * Classe [[ScheduleManagerActor]] (Akka Typed) possédant 2 modes : "Libre" et "Controle".
 * Il permet le gestion de l'emploi du temps, notamment si un vol prioritaire arrive ou qu'une piste se ferme.
 *
 * Mode [[Libre]] :
 *   Ajoute le vol directement sans replanification.
 *   Conflit dans le réseau de Pétri -> "Boom" immédiat.
 *
 * Mode [[Controle]] :
 *   Pour chaque piste :
 *     1. Tenter le placement sans délai -> vérifie réseau Pétri.
 *     2. Si conflit, identifie les vols bloquants (arrivée ou départ qui occupe la ressource) dont l'heure est encore dans le futur (> simTime).
 *     3. Teste des décalages par paliers (+15, ..., +120 min) sur ces vols, revérifie chaque fois avec le réseau de Pétri.
 *     4. Si aucune piste ni aucun délai ne sont viables -> annulation.
 */
object ScheduleManagerActor {   // object : singleton (1 seule instance possible). C'est comme "static class" en java.

  sealed trait Mode
  case object Libre    extends Mode
  case object Controle extends Mode

  // Paliers de décalage testés (minutes)
  private val DelaySteps          = List(15, 30, 45, 60, 90, 120)
  // Pour les vols d'urgence : décalages plus agressifs des bloquants
  private val EmergencyDelaySteps = List(15, 30, 45, 60, 90, 120, 150, 180, 240)

  // Un résultat est acceptable si toutes les issues restantes sont des GateOverflow.
  // Le GateOverflow ne cause pas de "Boom" : la simulation assigne les gates dynamiquement par file d'attente.
  // Seul un RunwayConflict est un vrai danger.
  private def isAcceptable(r: VerificationResult): Boolean =
    r.valid || r.issues.forall(_.isInstanceOf[GateOverflow])


  /***
   * Point d'entrée de [[ScheduleManagerActor]].
   * Initialise l'acteur avec un compteur à zéro, un générateur aléatoire neuf et aucun vol d'urgence connu.
   * 
   * @param towerRef      référence de l'acteur [[TowerControlActor]] destinataire des messages/notifications (ajouts, annulations, Booms)
   * @param schedule      planning courant. C'est un dictionnaire qui relie chaque id de piste à la liste ordonnée des vols (`AircraftFlight`) qui lui sont affectés.
   * @param runwayIds     liste des ids des pistes
   * @param runwayCount   nombre de pistes
   * @param garageCount   nombre de garages
   * @param mode          mode de la simulation ([[Libre]] ou [[Controle]])
   * 
   * @return le [[akka.actor.typed.Behavior]] initial de l'acteur, pret à recevoir des messages de type [[ScheduleManagerCommand]].
   */
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

  /***
   * Méthode récursive principale.
   * 
   * @param towerRef      référence de l'acteur [[TowerControlActor]] destinataire des messages/notifications (ajouts, annulations, Booms)
   * @param schedule      planning courant. C'est un dictionnaire qui relie chaque id de piste à la liste ordonnée des vols (`AircraftFlight`) qui lui sont affectés.
   * @param runwayIds     liste des ids des pistes
   * @param runwayCount   nombre de pistes
   * @param garageCount   nombre de garages
   * @param mode          mode de la simulation (Libre ou Controle)
   * @param counter       compteur de nouveau vol créé (utiliser pour génération d'identifiants)
   * @param rng           valeur aléatoire utilisé pour sélectionner une piste (en mode [[Libre]]) et une destination
   * @param emergencyAirplaneIds  liste des ids des vols avec niveau d'urgence
   * 
   * @return le prochain [[akka.actor.typed.Behavior]] de l'acteur après traitement du message de type [[ScheduleManagerCommand]].
   */
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

        // un nouveau vol arrive
        case ScheduleManagerCommand.PrepareNewFlight(triggeringEventId, simTime, arrivalHour, arrivalMinute, urgencyLevel) =>
          val newCounter    = counter + 1
          val isEmergency   = urgencyLevel == cysky.model.UrgencyLevel.Emergency
          val arrivalTime   = LocalTime.of(arrivalHour, arrivalMinute)
          val departureTime = arrivalTime.plusHours(1)

          // heure d'arrivée trop tardive
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

              // Mode Libre : vérification Pétri (sans replanification)
              // => si conflit, on a un "Boom" (pas de tentative de décalage)
              case Libre =>
                val runway    = runwayIds(rng.nextInt(runwayIds.size))
                val arr       = baseArrival.copy(runway = runway)
                val dep       = baseDep.copy(runway = runway)
                val candidate = addToSchedule(schedule, runway, arr, dep)
                val result    = PetriScheduleVerifier.verify(candidate, runwayCount, garageCount)

                // vol conforme au réseau de Pétri (vol ajouté sans pb)
                if (result.valid) {
                  ctx.log.info(
                    s"[SM/Libre ✓] SM_ARR_$newCounter/$airplaneId → $runway" +
                    s" arrivée ${fmt(arrivalTime)}, départ ${fmt(departureTime)}"
                  )
                  SimState.setSchedule(candidate)
                  towerRef ! FlightAddedByManager(candidate)
                  running(towerRef, candidate, runwayIds, runwayCount, garageCount, mode, newCounter, rng, emergencyAirplaneIds)
                } else {
                  // Vol non conforme au réseau de Pétri => Boom
                  ctx.log.warn(s"[SM/Libre ✗] Conflit pour SM_ARR_$newCounter → BOOM (mode libre, pas de replanification)")
                  result.issues.foreach(i => ctx.log.warn(s"  ${i.msg}"))
                  sendBoom(towerRef, airplaneId, schedule, result, arr, dep)
                  running(towerRef, schedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng, emergencyAirplaneIds)
                }

              // Mode Contrôle
              // => utilisation du réseau de Pétri avec placement intelligent (replannification)
              case Controle =>
                // Si le vol est une urgence, enregistrer son airplaneId pour que les futurs placements ne tentent pas de le décaler.
                val updatedEmergencyIds =
                  if (isEmergency) emergencyAirplaneIds + airplaneId
                  else emergencyAirplaneIds

                if (isEmergency) {
                  ctx.log.warn(s"[SM/URGENCE] Placement $airplaneId — urgences connues: $emergencyAirplaneIds — pistes: $runwayIds")
                   simTime.toLocalTime,
                  isEmergency,
                  updatedEmergencyIds,
                  if (isEmergency) Some(ctx.log) else None
                ) match {

                  // 1. Placement trouvé
                  case Right((newSchedule, logMsg)) =>
                    ctx.log.info(s"[SM/Controle ✓] $logMsg")
                    SimState.setSchedule(newSchedule)
                    towerRef ! FlightAddedByManager(newSchedule)
                    running(towerRef, newSchedule, runwayIds, runwayCount, garageCount, mode, newCounter, rng, updatedEmergencyIds)

                  // 2. Aucune piste libre trouvée
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

  /***
   * Trouve une piste libre et un créneau valide pour le vol ajouté (mode [[Controle]]).
   * 
   * Trois tentatives par piste, dans l'ordre :
   *  - t0 : placement direct, sans décalage.
   *  - t1 : décalage des vols bloquants existants (les urgences ne sont jamais décalées).
   *  - t2 : décalage du nouveau vol lui-meme (vols normaux uniquement, limite 22h59).
   *
   * Si rien ne fonctionne et que le vol est une urgence, un placement forcé est calculé sur la piste la moins encombrée.
   * 
   * @param schedule      planning courant. C'est un dictionnaire qui relie chaque id de piste à la liste ordonnée des vols (`AircraftFlight`) qui lui sont affectés.
   * @param runwayIds     liste des ids des pistes
   * @param airplaneId    id de l'avion
   * @param counter       valeur courante du compteur de nouveau vol créé (utiliser pour génération d'identifiants de vol)
   * @param baseArrival   Squelette du vol d'arrivée (piste non encore affectée).
   * @param baseDep       Squelette du vol de départ (piste non encore affectée).
   * @param runwayCount   nombre de pistes
   * @param garageCount   nombre de garages
   * @param simTime       heure de la simulation
   * @param isEmergency   indique si le vol est une urgence (false par defaut)
   * @param emergencyAirplaneIds  liste des ids des vols avec niveau d'urgence
   * @param dbg           Logger optionnel pour traces fines (activé pour les urgences)
   * 
   * @return `Right((newSchedule, logMsg))` si un placement est trouvé ; `Left((lastResult, lastRunway))` si toutes les tentatives ont échoué.
   */
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

    // Pour une urgence : trier les pistes de façon à essayer d'abord celles qui n'ont pas déjà une urgence planifiée.
    // Cela évite d'empiler 2 urgences sur la même piste.
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

        // ***** Tentative 0 : sans délai *****
        // Valide ou seulement GateOverflow (pas de RunwayConflict) -> accepté.
        lazy val t0: Option[(Map[String, List[AircraftFlight]], String)] = {
          val c = addToSchedule(schedule, runway, arr, dep)
          val r = PetriScheduleVerifier.verify(c, runwayCount, garageCount)
          val ok = isAcceptable(r)
          dbg.foreach(_.warn(s"[SM/DBG] t0 $runway ok=$ok issues=${r.issues.map(_.msg).mkString("|")}"))
          Option.when(ok)((c,
            s"SM_ARR_$counter/$airplaneId → $runway (sans délai)" +
            s" arrivée ${fmt(arr.scheduledTime)}, départ ${fmt(dep.scheduledTime)}"))
        }

        // ***** Tentative 1 : décaler les vols bloquants (piste seulement) *****
        // Résout les RunwayConflict sans toucher les occupants de gate.
        // Les vols d'urgence déjà planifiés (emergencyAirplaneIds) ne sont jamais décalés
        // Si le seul bloquant est une urgence, t1 échoue et on passe à la piste suivante (t0 y trouvera peut-être un créneau libre).
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

        // ***** Tentative 2 : reculer l'atterrissage du nouveau vol *****
        // /!\ Cas non applicable pour un vol d'urgence.
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


  // extractRunwayBlockingIds
  //
  // Variante de extractBlockingIds qui ne traite que les RunwayConflict.
  // Les GateOverflow sont délibérément ignorés : décaler les occupants de gate les maintient plus longtemps en gate (aggrave le problème),
  // et crée de nouveaux conflits en cascade entre vols T1 dos-à-dos.
  // Le GateOverflow résiduel est accepté via isAcceptable.

  /***
   * Retourne les ids des avions qui causent un `RunwayConflict` sur la piste (en ignorant volontairement les GateOverflow et Deadlock).
   *
   * Utilisée par [[findValidPlacement]] (t1) : décaler des occupants de gate aggraverait les conflits plutôt que de les résoudre.
   *
   * @param candidate Planning candidat (nouveau vol déjà inclus). => C'est un dictionnaire qui relie chaque id de piste à la liste ordonnée des vols (`AircraftFlight`) qui lui sont affectés.
   * @param result    résultat de vérification Pétri.
   * @param newId     id du nouveau vol, exclu du résultat.
   * @param simTime   heure courante  de la simulation. (permet de retenir uniquement les vols futurs)
   * @return liste des ids bloquants.
   */
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

  /***
   * Identifie les ids de tous les avions avions responsables des conflits de Pétri détectés ([[RunwayConflict]] LAND/TAXI et [[GateOverflow]]) .
   *
   * Un conflit de type "LAND" peut être causé par un atterrissage en cours ou un décollage (qui occupe la même piste).
   * Seuls les vols dont l'heure est encore dans le futur (>simTime) sont retournés.
   * 
   * @param candidate Planning candidat (nouveau vol déjà inclus). => C'est un dictionnaire qui relie chaque id de piste à la liste ordonnée des vols (`AircraftFlight`) qui lui sont affectés.
   * @param result    résultat de vérification Pétri
   * @param newId     id du nouveau vol, exclu du résultat
   * @param simTime   heure courante de la simulation. (permet de retenir uniquement les vols futurs)
   * @return liste des ids bloquants.
   */
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

  /***
   * Décale les horaires d'un avion dans le schedule.
   * Seuls les vols dont scheduledTime > simTime sont modifiés.
   * 
   * @param schedule      planning courant. C'est un dictionnaire qui relie chaque id de piste à la liste ordonnée des vols (`AircraftFlight`) qui lui sont affectés.
   * @param airplaneId    id de l'vion à décaler
   * @param deltaMin      décalage en minutes
   * @param simTime       heure courante de la simulation
   * @return nouveau planning ("schedule") avec les horaires mis à jour.
   */
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

  /***
   * Détermine le type de Boom à partir du 1er conflit détecté et le transmet à la [[TowerControl]].
   * 
   * 3 types de Boom possibles :
   *  - RunwayConflict LAND -> [[BoomRunway]] : collision sur la piste à l'atterrissage.
   *  - RunwayConflict TAXI -> [[BoomTaxi]]   : collision sur la voie de circulation.
   *  - GateOverflow, `Deadlock` ou autre -> [[BoomGarage]] : débordement de gate.
   * 
   * @param towerRef   référence de l'acteur [[TowerControlActor]] destinataire des messages
   * @param airplaneId id de l'avion qui a provoqué le conflit
   * @param schedule   Planning en vigueur. => dictionnaire qui relie chaque id de piste à la liste ordonnée des vols (`AircraftFlight`) qui lui sont affectés.
   * @param result     résultat de vérification Pétri
   * @param arrival    vol d'arrivée du nouvel avion
   * @param departure  vol de départ du nouvel avion
   */
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


  /**
   * Ajoute les vols d'arrivée et de départ d'un avion au planning d'une piste donnée.
   *
   * @param schedule Planning courant. => dictionnaire qui relie chaque id de piste à la liste ordonnée des vols (`AircraftFlight`) qui lui sont affectés
   * @param runway   id de la piste cible
   * @param arr      vol d'arrivée à insérer
   * @param dep      vol de départ à insérer
   * @return nouveau planning avec `arr` et `dep` ajoutés en fin de liste pour `runway`.
   */
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

  /**
   * Ajoute uniquement le vol d'arrivée d'un avion au planning d'une piste donnée.
   * Utilisé lorsque le départ est annulé (cad gate saturé) ou différé.
   *
   * @param schedule Planning courant. => dictionnaire qui relie chaque id de piste à la liste ordonnée des vols (`AircraftFlight`) qui lui sont affectés
   * @param runway   id de la piste cible
   * @param arr      vol d'arrivée à insérer
   * @return nouveau planning avec `arr` ajouté en fin de liste pour `runway`.
   */
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
