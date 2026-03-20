package cyairsim.models

import java.time.LocalTime

/**
 * Représente un voyage complet d'un avion dans le système AeroSim.
 *
 * @param flightId        Identifiant unique du vol (ex: "AF1234")
 * @param airplaneId      Identifiant de l'avion effectuant ce vol (ex: "PLANE_42")
 * @param arrivalTime     Heure d'arrivée prévue à l'aéroport
 * @param arrivalRunway   Identifiant de la piste d'atterrissage assignée (ex: "RWY_1")
 * @param departureRunway Identifiant de la piste de décollage assignée (ex: "RWY_2")
 * @param departureTime   Heure de décollage prévue
 * @param destination     Destination du vol au départ (ex: "CDG", "JFK")
 * @param boardingTime    Heure de début d'embarquement
 * @param gateId          Identifiant de la porte d'embarquement / garage (ex: "GATE_3")
 */
final case class Flight(
  flightId      : String,
  airplaneId    : String,
  arrivalTime   : LocalTime,
  arrivalRunway : String,
  departureRunway: String,
  departureTime : LocalTime,
  destination   : String,
  boardingTime  : LocalTime,
  gateId        : String
) {

  /** Vérifie que l'embarquement est planifié avant le décollage. */
  val isValid: Boolean = boardingTime.isBefore(departureTime) && arrivalTime.isBefore(boardingTime)

  override def toString: String =
    s"Flight[$flightId | $airplaneId | " +
    s"ARR $arrivalTime on $arrivalRunway | " +
    s"GATE $gateId (boarding $boardingTime) | " +
    s"DEP $departureTime on $departureRunway -> $destination]"
}

object Flight {

  /**
   * Constructeur alternatif avec les heures sous forme de String "HH:mm".
   * Utile pour les tests et la génération de schedules.
   */
  def fromStrings(
    flightId        : String,
    airplaneId      : String,
    arrivalTime     : String,
    arrivalRunway   : String,
    departureRunway : String,
    departureTime   : String,
    destination     : String,
    boardingTime    : String,
    gateId          : String
  ): Flight =
    Flight(
      flightId        = flightId,
      airplaneId      = airplaneId,
      arrivalTime     = LocalTime.parse(arrivalTime),
      arrivalRunway   = arrivalRunway,
      departureRunway = departureRunway,
      departureTime   = LocalTime.parse(departureTime),
      destination     = destination,
      boardingTime    = LocalTime.parse(boardingTime),
      gateId          = gateId
    )

  /** Ordonne les vols par heure d'arrivée — utile pour le ScheduleGenerator. */
  implicit val orderByArrival: Ordering[Flight] =
    Ordering.by(_.arrivalTime)
}