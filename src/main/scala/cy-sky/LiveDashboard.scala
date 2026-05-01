package cysky

import cysky.actors.TowerControlActor.TowerData
import cysky.models.{Arrival, Departure}
import java.nio.file.{Files, Paths}
import java.time.format.DateTimeFormatter

object LiveDashboard {

  private val HHmm    = DateTimeFormatter.ofPattern("HH:mm")
  private val outPath = Paths.get("simulation_live.html")

  def write(data: TowerData): Unit = {
    val simTimeStr = data.simTime.format(HHmm)

    val allFlights = data.schedule.toList
      .flatMap { case (rwy, flights) => flights.map(f => (rwy, f)) }
      .sortBy(_._2.scheduledTime)
      .filter(_._2.kind == Arrival)

    val rows = allFlights.zipWithIndex.map { case ((rwy, f), i) =>
      val state = data.flightStates.getOrElse(f.airplaneId, "Planifie")
      val departureTime = data.schedule.get(rwy)
        .flatMap(_.find(d => d.airplaneId == f.airplaneId && d.kind == Departure))
        .map(_.scheduledTime.format(HHmm))
        .getOrElse("-")

      s"""<tr>
        <td>${i + 1}</td>
        <td>${f.flightId}</td>
        <td>${f.airplaneId}</td>
        <td>${f.scheduledTime.format(HHmm)}</td>
        <td>$departureTime</td>
        <td>$rwy</td>
        <td>${f.destination}</td>
        <td>$state</td>
      </tr>"""
    }.mkString("\n")

    val activeCount   = data.airplanes.size
    val departedCount = data.flightStates.values.count(_ == "Parti")
    val totalCount    = allFlights.size

    val html =
      s"""<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="refresh" content="3">
  <title>CySky - Live Dashboard</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 20px; }
    h1 { color: darkblue; }
    table { border-collapse: collapse; width: 100%; }
    th, td { border: 1px solid black; padding: 5px 10px; text-align: left; }
    th { background-color: #ccccee; }
    tr:nth-child(even) { background-color: #f0f0f0; }
    .stats { margin-bottom: 16px; }
    .stats span { margin-right: 20px; font-weight: bold; }
  </style>
</head>
<body>
  <h1>CySky - Live Dashboard</h1>
  <p>Heure simulee : <b>$simTimeStr</b> &nbsp;&nbsp; (rafraichissement auto toutes les 3s)</p>

  <div class="stats">
    <span>Total vols : $totalCount</span>
    <span>En cours : $activeCount</span>
    <span>Partis : $departedCount</span>
    <span>Pistes libres : ${data.freeRunways.size}</span>
    <span>Garages libres : ${data.freeGarages.size}</span>
  </div>

  <table>
    <thead>
      <tr>
        <th>#</th><th>Vol</th><th>Avion</th>
        <th>Arrivee</th><th>Depart</th><th>Piste</th><th>Destination</th><th>Etat</th>
      </tr>
    </thead>
    <tbody>
      $rows
    </tbody>
  </table>
</body>
</html>"""

    Files.writeString(outPath, html)
  }
}
