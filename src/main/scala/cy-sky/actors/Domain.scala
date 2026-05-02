package cysky.model

import java.time.LocalDateTime


/***
 * Définit les caractéristiques de niveau d'urgence de [[UrgencyLevel]] (object lié).
 * priorityScore est utilisé par le [[ScheduleManager]] pour classer les vols et par [[ControlTower]].
 */
sealed trait UrgencyLevel {
  /***
   * Score de priorité de l'urgence.
   * /!\ A noter : val peut implémenter un def (mais pas l'inverse). => utiliser val suffit (dans les objets "extends").
   */
  def priorityScore: Int
  /***
   * Label de priorité de l'urgence.
   * /!\ A noter : val peut implémenter un def (mais pas l'inverse). => utiliser val suffit (dans les objets "extends").
   */
  def label: String
}

/***
 * Niveau d'urgence utilisé par [[ScheduleManager]] pour planifier/ordonner les atterrissages et par [[ControlTower]] pour la priorité des queues.
 */
object UrgencyLevel {

  /** Aviation légère / vol privé. Premier sacrifié en cas de saturation. */
  case object Civil extends UrgencyLevel {
    val priorityScore = 10
    val label         = "Civil"
  }

  /** Vol commercial standard (Air France, EasyJet…). Gestion FIFO. */
  case object Commercial extends UrgencyLevel {
    val priorityScore = 50
    val label         = "Commercial"
  }

  /** Vol militaire. Priorité sur le trafic civil, piste dédiée si dispo. */
  case object Military extends UrgencyLevel {
    val priorityScore = 70
    val label         = "Military"
  }

  /** MEDEVAC / urgence médicale déclarée. Garage prioritaire à l'arrivée. */
  case object Medical extends UrgencyLevel {
    val priorityScore = 80
    val label         = "Medical"
  }

  /** Vol présidentiel / diplomatique. Piste réservée dès annonce. */
  case object Presidential extends UrgencyLevel {
    val priorityScore = 90
    val label         = "Presidential"
  }

  /**
   * Urgence déclarée en vol (panne moteur, dépressurisation…).
   * Arc inhibiteur Pétri : bloque toutes les autres transitions T_land et T_takeoff tant que non traité.
   */
  case object Emergency extends UrgencyLevel {
    val priorityScore = 100
    val label         = "Emergency"
  }

  // Ordre naturel pour la Priority Queue
  implicit val ordering: Ordering[UrgencyLevel] =
    Ordering.by(_.priorityScore)

  // Liste de tous les niveaux, du plus bas au plus haut
  val values: List[UrgencyLevel] =
    List(Civil, Commercial, Military, Medical, Presidential, Emergency)
}




// ─────────────────────────────────────────────
// AirplaneState — places Pétri du cycle de vie avion
// Chaque état correspond à une place P_airplane_* dans le réseau
// ─────────────────────────────────────────────
sealed trait AirplaneState

/***
 * Etats possibles de l'avion.
 * Chaque état correspond à une place P_airplane_* dans le réseau de Pétri.
 */
object AirplaneState {
  /** En vol, en approche — place P_airplane_inflight */
  case object InFlight  extends AirplaneState
  /** Atterrissage en cours sur piste assignée */
  case object Landing   extends AirplaneState
  /** Taxi de la piste vers le garage (libère la piste immédiatement) */
  case object Taxiing   extends AirplaneState
  /** Garé — embarquement, ravitaillement, maintenance */
  case object Parked    extends AirplaneState
  /** Taxi du garage vers la piste de décollage */
  case object TaxiOut   extends AirplaneState
  /** Décollage en cours */
  case object Takeoff   extends AirplaneState
  /** Parti — état terminal, acteur arrêté après */
  case object Departed  extends AirplaneState
  /** Dérouté vers un autre aéroport — état terminal */
  case object Diverted  extends AirplaneState
}



// ─────────────────────────────────────────────
// RunwayState — places Pétri de la piste
// ─────────────────────────────────────────────
sealed trait RunwayState

/***
 * Etats possibles des pistes d'atterrissage.
 */
object RunwayState {
  /** Piste disponible — P_runway_free_i contient 1 jeton */
  case object Free              extends RunwayState
  /** Atterrissage en cours — P_runway_landing_i */
  case object Landing           extends RunwayState
  /** Décollage en cours — P_runway_takeoff_i */
  case object TakeoffInProgress extends RunwayState
  /** État transitoire : avion en taxi, piste déjà libérée */
  case object TaxiToGarage      extends RunwayState
  /** Piste fermée (météo, panne) — aucune transition franchissable */
  case object Blocked           extends RunwayState
}

// ─────────────────────────────────────────────
// GarageState — places Pétri du garage
// ─────────────────────────────────────────────
sealed trait GarageState

/***
 * Etats possibles des garages.
 */
object GarageState {
  /** Garage disponible — P_garage_free_j contient 1 jeton */
  case object Free     extends GarageState
  /** Avion stationné — P_garage_occupied_j */
  case object Occupied extends GarageState
}

// ─────────────────────────────────────────────
// EventType — types d'événements injectables dynamiquement
// ─────────────────────────────────────────────
sealed trait EventType {
  def displayName: String
}

/***
 * Types d'événements pouvant être injectés en cours de simulation.
 */
object EventType {
  case object EmergencyArrival extends EventType { val displayName = "Atterrissage d'urgence" }
}



// ─────────────────────────────────────────────
// InjectedEvent — événement créé depuis le front
// et mis en file d'attente par EventInjectorActor
// ─────────────────────────────────────────────

/***
 * Evenement injectable.
 * 
 * @param id            id de l'evement
 * @param eventType     type d'evenement injecté
 * @param targetHour    heure d'injection
 * @param targetMinute  minute d'injection
 * @param urgencyLevel  niveau d'urgence de l'evenement
 * @param note          commentaires additionnels
 * @param status        statut
 */
final case class InjectedEvent(     // case class : a un constructeur automatique  + genere des méthodes comme equals, toString, ...
  id:           String,
  eventType:    EventType,
  targetHour:   Int,
  targetMinute: Int,
  urgencyLevel: UrgencyLevel,
  note:         String  = "",
  status:       String  = "pending"
) {
  /***
   * Donne l'horaire d'injection de l'evenement au format String.
   * 
   * @return l'horaire HH:mm d'injection de l'evenement (en String).
   */
  def targetTimeStr: String = f"$targetHour%02d:$targetMinute%02d"
}



/***
 * Données immuables d'un vol planifié.
 * Généré par le [[ScheduleGenerator]], partagé entre les deux arbres.
 * 
 * @param airplaneId        id de l'avion
 * @param flightNumber      numero du vol
 * @param urgencyLevel      niveau d'urgence du vol
 * @param scheduledArrival  horaire d'arrivée
 * @param scheduledDepart   horaire de départ
 */
final case class FlightData(
  airplaneId:       String,
  flightNumber:     String,
  urgencyLevel:     UrgencyLevel,
  scheduledArrival: LocalDateTime,
  scheduledDepart:  LocalDateTime,
) {
  /***
   * Durée au sol en minutes — alimente le PetriNetEngine temporisé.
   * @return la durée au sol en minutes.
   */
  lazy val groundDurationMinutes: Long =
    java.time.Duration
      .between(scheduledArrival, scheduledDepart)
      .toMinutes

  /***
   * Score de priorité délégué au niveau d'urgence.
   * @return le score de priorité du vol.
   */
  def priorityScore: Int = urgencyLevel.priorityScore
}
