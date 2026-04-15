package cysky

import cysky.actors.TowerControlActor.TowerData
import cysky.models.{Arrival, Departure}
import java.nio.file.{Files, Paths}
import java.time.format.DateTimeFormatter

// ═══════════════════════════════════════════════════════════════
// LiveDashboard — génère un fichier HTML mis à jour en temps réel
//
// Appelé depuis TowerControlActor toutes les 10 ticks.
// Le fichier HTML se recharge automatiquement toutes les 3 secondes
// via <meta http-equiv="refresh">.
// ═══════════════════════════════════════════════════════════════
object LiveDashboard {

  private val HHmm    = DateTimeFormatter.ofPattern("HH:mm")
  private val outPath = Paths.get("simulation_live.html")

  // Couleur et libellé selon l'état
  private def stateStyle(state: String): (String, String) = state match {
    case s if s.startsWith("En approche")          => ("#1d4ed8", "#bfdbfe")
    case s if s.startsWith("Atterrissage en cours") => ("#0369a1", "#bae6fd")
    case s if s.startsWith("Au sol")               => ("#065f46", "#6ee7b7")
    case s if s.startsWith("Taxi vers piste")      => ("#92400e", "#fde68a")
    case s if s.startsWith("Décollage en cours")   => ("#7e22ce", "#e9d5ff")
    case s if s.startsWith("Parti")                => ("#374151", "#9ca3af")
    case _                                          => ("#1e2430", "#64748b")
  }

  private def stateTag(airplaneId: String, states: Map[String, String]): String = {
    val state = states.getOrElse(airplaneId, "Planifie")
    val (bg, fg) = stateStyle(state)
    s"""<span style="background:$bg;color:$fg;padding:0.2rem 0.6rem;border-radius:4px;font-size:0.8rem;font-weight:600;white-space:nowrap">$state</span>"""
  }

  def write(data: TowerData): Unit = {
    val simTimeStr = data.simTime.format(HHmm)

    // Toutes les paires (runwayId, flight) triées par heure
    val allFlights = data.schedule.toList
      .flatMap { case (rwy, flights) => flights.map(f => (rwy, f)) }
      .sortBy(_._2.scheduledTime)
      // On garde uniquement les arrivées (1 ligne par avion, statut global)
      .filter(_._2.kind == Arrival)

    val rows = allFlights.zipWithIndex.map { case ((rwy, f), i) =>
      val kindTag = """<span style="background:#1d4ed8;color:#bfdbfe;padding:0.2rem 0.5rem;border-radius:4px;font-size:0.8rem">Arrivee</span>"""

      // Chercher l'heure de départ correspondante
      val departureTime = data.schedule.get(rwy)
        .flatMap(_.find(d => d.airplaneId == f.airplaneId && d.kind == Departure))
        .map(_.scheduledTime.format(HHmm))
        .getOrElse("—")

      s"""<tr>
            <td style="padding:0.7rem 1rem;border-top:1px solid #2d3748;font-size:0.875rem">${i + 1}</td>
            <td style="padding:0.7rem 1rem;border-top:1px solid #2d3748;font-size:0.875rem">
              <span style="background:#1d4ed8;color:#bfdbfe;padding:0.2rem 0.6rem;border-radius:4px;font-size:0.8rem;font-weight:600">${f.flightId}</span>
            </td>
            <td style="padding:0.7rem 1rem;border-top:1px solid #2d3748;font-size:0.875rem">${f.airplaneId}</td>
            <td style="padding:0.7rem 1rem;border-top:1px solid #2d3748;font-size:0.875rem">${f.scheduledTime.format(HHmm)}</td>
            <td style="padding:0.7rem 1rem;border-top:1px solid #2d3748;font-size:0.875rem">$departureTime</td>
            <td style="padding:0.7rem 1rem;border-top:1px solid #2d3748;font-size:0.875rem">$rwy</td>
            <td style="padding:0.7rem 1rem;border-top:1px solid #2d3748;font-size:0.875rem">
              <span style="background:#064e3b;color:#6ee7b7;padding:0.2rem 0.6rem;border-radius:4px;font-size:0.8rem;font-weight:600">${f.destination}</span>
            </td>
            <td style="padding:0.7rem 1rem;border-top:1px solid #2d3748;font-size:0.875rem">${stateTag(f.airplaneId, data.flightStates)}</td>
          </tr>"""
    }.mkString("\n")

    val activeCount  = data.airplanes.size
    val departedCount = data.flightStates.values.count(_ == "Parti")
    val totalCount   = allFlights.size

    val html =
      s"""<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="refresh" content="3">
  <title>CySky — Live Dashboard</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: 'Segoe UI', sans-serif;
      background: #0f1117;
      color: #e2e8f0;
      min-height: 100vh;
      padding: 2rem;
    }
    header { margin-bottom: 2rem; display: flex; align-items: center; justify-content: space-between; }
    header h1 { font-size: 1.8rem; color: #60a5fa; letter-spacing: 0.05em; }
    .clock {
      font-size: 2.5rem;
      font-weight: 700;
      color: #34d399;
      font-variant-numeric: tabular-nums;
      letter-spacing: 0.05em;
    }
    .clock-label { font-size: 0.75rem; color: #64748b; text-align: center; margin-top: 0.2rem; }
    .stats { display: flex; gap: 1rem; margin-bottom: 2rem; }
    .stat-card {
      background: #1e2430;
      border: 1px solid #2d3748;
      border-radius: 8px;
      padding: 1rem 1.5rem;
      min-width: 120px;
    }
    .stat-card .label { font-size: 0.75rem; color: #64748b; text-transform: uppercase; letter-spacing: 0.08em; }
    .stat-card .value { font-size: 1.6rem; font-weight: 700; color: #60a5fa; margin-top: 0.2rem; }
    table { width: 100%; border-collapse: collapse; background: #1e2430; border-radius: 8px; overflow: hidden; }
    thead tr { background: #16213e; }
    th { padding: 0.75rem 1rem; text-align: left; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 0.08em; color: #64748b; }
    tr:hover td { background: #243044; }
    .live-dot {
      display: inline-block; width: 8px; height: 8px;
      background: #34d399; border-radius: 50%;
      margin-right: 0.5rem;
      animation: pulse 1.5s infinite;
    }
    @keyframes pulse { 0%,100%{opacity:1} 50%{opacity:0.3} }
  </style>
</head>
<body>
  <header>
    <div>
      <h1>&#9992; CySky — Live Dashboard</h1>
      <p style="color:#94a3b8;margin-top:0.3rem;font-size:0.9rem">
        <span class="live-dot"></span>Simulation en cours — rafraichissement auto toutes les 3s
      </p>
    </div>
    <div style="text-align:center">
      <div class="clock">$simTimeStr</div>
      <div class="clock-label">Heure simulee</div>
    </div>
  </header>

  <div class="stats">
    <div class="stat-card">
      <div class="label">Vols total</div>
      <div class="value">$totalCount</div>
    </div>
    <div class="stat-card">
      <div class="label">En cours</div>
      <div class="value" style="color:#34d399">$activeCount</div>
    </div>
    <div class="stat-card">
      <div class="label">Partis</div>
      <div class="value" style="color:#9ca3af">$departedCount</div>
    </div>
    <div class="stat-card">
      <div class="label">Pistes libres</div>
      <div class="value" style="color:#fbbf24">${data.freeRunways.size}</div>
    </div>
    <div class="stat-card">
      <div class="label">Garages libres</div>
      <div class="value" style="color:#a78bfa">${data.freeGarages.size}</div>
    </div>
  </div>

  <table>
    <thead>
      <tr>
        <th>#</th>
        <th>Vol</th>
        <th>Avion</th>
        <th>Arrivee</th>
        <th>Depart</th>
        <th>Piste</th>
        <th>Destination</th>
        <th>Etat</th>
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
