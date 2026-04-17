package cysky

import cysky.models.{ AircraftFlight => Flight, Arrival, Departure }
import scala.io.Source

object HtmlReport {

  private def renderRow(f: Flight, index: Int): String = {
    val typeTag = f.kind match {
      case Arrival   => """<span class="tag arrival">&#8600; Arrivée</span>"""
      case Departure => """<span class="tag departure">&#8599; Départ</span>"""
    }
    s"""<tr>
          <td>${index + 1}</td>
          <td><span class="badge">${f.flightId}</span></td>
          <td>${f.airplaneId}</td>
          <td>$typeTag</td>
          <td>${f.scheduledTime}</td>
          <td>${f.runway}</td>
          <td><span class="dest">${f.destination}</span></td>
        </tr>"""
  }

  private def renderTable(flights: List[Flight]): String = {
    val rows = flights.sortBy(_.scheduledTime).zipWithIndex
      .map { case (f, i) => renderRow(f, i) }.mkString
    s"""<table>
          <thead>
            <tr>
              <th>#</th><th>Vol</th><th>Avion</th><th>Type</th>
              <th>Heure</th><th>Piste</th><th>Destination</th>
            </tr>
          </thead>
          <tbody>$rows</tbody>
        </table>"""
  }

  def generate(
    terminalId: String,
    schedule  : Map[String, List[Flight]]
  ): String = {
    val stream = getClass.getResourceAsStream("/report.html")
    require(stream != null,
      "report.html introuvable. Vérifie que le fichier est dans src/main/resources/.")

    val template      = Source.fromInputStream(stream, "UTF-8").mkString
    val sortedRunways = schedule.keys.toList.sorted
    val allFlights    = sortedRunways.flatMap(schedule(_))
    val totalVols     = allFlights.size

    val tabs = (List("ALL") ++ sortedRunways).map { id =>
      val label = if (id == "ALL") "Tous les vols" else id
      s"""<button class="tab-btn" onclick="showTab('$id', this)">$label</button>"""
    }.mkString("\n")

    val globalPanel =
      s"""<div class="tab-panel" id="panel-ALL">${renderTable(allFlights)}</div>"""

    val runwayPanels = sortedRunways.map { rwy =>
      s"""<div class="tab-panel" id="panel-$rwy">${renderTable(schedule(rwy))}</div>"""
    }.mkString

    template
      .replace("{{TERMINAL_ID}}",   terminalId)
      .replace("{{RUNWAY_COUNT}}",  sortedRunways.size.toString)
      .replace("{{TOTAL_FLIGHTS}}", totalVols.toString)
      .replace("{{AVG_FLIGHTS}}",   if (sortedRunways.nonEmpty) (totalVols / sortedRunways.size).toString else "0")
      .replace("{{TABS}}",          tabs)
      .replace("{{PANELS}}",        globalPanel + runwayPanels)
  }
}