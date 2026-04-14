package cysky.model

import java.time.LocalDateTime

// ─────────────────────────────────────────────
// Urgency levels — ordered by priority (highest last)
// priorityScore is used by ScheduleManager to rank plans
// and by ControlTower's Priority Queue
// ─────────────────────────────────────────────
sealed trait UrgencyLevel {
  def priorityScore: Int
  def label: String
}

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
   * Arc inhibiteur Pétri : bloque toutes les autres transitions
   * T_land et T_takeoff tant que non traité.
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

object GarageState {
  /** Garage disponible — P_garage_free_j contient 1 jeton */
  case object Free     extends GarageState
  /** Avion stationné — P_garage_occupied_j */
  case object Occupied extends GarageState
}

// ─────────────────────────────────────────────
// Données immuables d'un vol planifié
// Généré par le ScheduleGenerator, partagé entre les deux arbres
// ─────────────────────────────────────────────
final case class FlightData(
  airplaneId:       String,
  flightNumber:     String,
  urgencyLevel:     UrgencyLevel,
  scheduledArrival: LocalDateTime,
  scheduledDepart:  LocalDateTime,
) {
  /** Durée au sol en minutes — alimente le PetriNetEngine temporisé */
  lazy val groundDurationMinutes: Long =
    java.time.Duration
      .between(scheduledArrival, scheduledDepart)
      .toMinutes

  /** Score de priorité délégué au niveau d'urgence */
  def priorityScore: Int = urgencyLevel.priorityScore
}
