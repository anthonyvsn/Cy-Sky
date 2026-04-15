package cysky

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import cysky.models.{Arrival, Departure}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter

// ═══════════════════════════════════════════════════════════════
// DashboardServer — serveur HTTP léger (JDK intégré, 0 dépendance)
//
// Routes :
//   GET  /        → page HTML du dashboard
//   POST /start   → démarre la simulation (décompte le latch)
//   GET  /state   → état courant en JSON (pollé toutes les 1s)
// ═══════════════════════════════════════════════════════════════
object DashboardServer {

  private val HHmm = DateTimeFormatter.ofPattern("HH:mm")

  def start(): Unit = {
    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    server.createContext("/",      exchange => handle(exchange, routeRoot))
    server.createContext("/start", exchange => handle(exchange, routeStart))
    server.createContext("/state", exchange => handle(exchange, routeState))
    server.setExecutor(null)
    server.start()
  }

  // ── Dispatcher générique ──────────────────────────────────────
  private def handle(
    ex:    HttpExchange,
    route: HttpExchange => (Int, String, String)
  ): Unit = {
    val (code, contentType, body) = route(ex)
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    ex.getResponseHeaders.set("Content-Type", contentType)
    ex.getResponseHeaders.set("Access-Control-Allow-Origin", "*")
    ex.sendResponseHeaders(code, bytes.length)
    ex.getResponseBody.write(bytes)
    ex.getResponseBody.close()
  }

  // ── GET / ─────────────────────────────────────────────────────
  private def routeRoot(ex: HttpExchange): (Int, String, String) =
    (200, "text/html; charset=UTF-8", htmlPage)

  // ── POST /start ───────────────────────────────────────────────
  private def routeStart(ex: HttpExchange): (Int, String, String) = {
    SimState.triggerStart()
    (200, "application/json", """{"ok":true}""")
  }

  // ── GET /state ────────────────────────────────────────────────
  private def routeState(ex: HttpExchange): (Int, String, String) =
    (200, "application/json; charset=UTF-8", buildJson())

  // ── Construction du JSON ──────────────────────────────────────
  private def buildJson(): String = {
    val started  = SimState.isStarted
    val finished = SimState.isFinished

    SimState.snapshot match {
      case None =>
        s"""{"started":$started,"finished":$finished,"simTime":"--:--","stats":{"total":0,"active":0,"departed":0,"freeRunways":0,"freeGarages":0},"flights":[]}"""

      case Some(data) =>
        val simTimeStr   = data.simTime.format(HHmm)
        val activeCount  = data.airplanes.size
        val departedCount = data.flightStates.values.count(_ == "Parti")
        val totalCount   = data.schedule.values.flatten.count(_.kind == Arrival)

        val flightsJson = data.schedule.toList
          .flatMap { case (rwy, flights) => flights.map(f => (rwy, f)) }
          .filter(_._2.kind == Arrival)
          .sortBy(_._2.scheduledTime)
          .map { case (rwy, f) =>
            val depTime = data.schedule.get(rwy)
              .flatMap(_.find(d => d.airplaneId == f.airplaneId && d.kind == Departure))
              .map(_.scheduledTime.format(HHmm))
              .getOrElse("--:--")
            val status = data.flightStates.getOrElse(f.airplaneId, "Planifie")
            s"""{
              "flightId":"${f.flightId}",
              "airplaneId":"${f.airplaneId}",
              "arrivalTime":"${f.scheduledTime.format(HHmm)}",
              "departureTime":"$depTime",
              "runway":"$rwy",
              "destination":"${f.destination}",
              "status":"$status"
            }"""
          }.mkString(",")

        s"""{
          "started":$started,
          "finished":$finished,
          "simTime":"$simTimeStr",
          "stats":{
            "total":$totalCount,
            "active":$activeCount,
            "departed":$departedCount,
            "freeRunways":${data.freeRunways.size},
            "freeGarages":${data.freeGarages.size}
          },
          "flights":[$flightsJson]
        }"""
    }
  }

  // ── Page HTML ─────────────────────────────────────────────────
  private val htmlPage: String = """<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
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

    header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin-bottom: 2rem;
    }

    header h1 { font-size: 1.8rem; color: #60a5fa; letter-spacing: 0.05em; }
    header p  { color: #94a3b8; margin-top: 0.3rem; font-size: 0.9rem; }

    .clock-box { text-align: center; }
    .clock {
      font-size: 3rem;
      font-weight: 700;
      color: #34d399;
      font-variant-numeric: tabular-nums;
      letter-spacing: 0.08em;
      transition: color 0.3s;
    }
    .clock.finished { color: #6b7280; }
    .clock-label { font-size: 0.75rem; color: #64748b; margin-top: 0.2rem; }

    .stats {
      display: flex;
      gap: 1rem;
      margin-bottom: 2rem;
      flex-wrap: wrap;
    }

    .stat-card {
      background: #1e2430;
      border: 1px solid #2d3748;
      border-radius: 8px;
      padding: 1rem 1.5rem;
      min-width: 110px;
    }
    .stat-card .label {
      font-size: 0.7rem;
      color: #64748b;
      text-transform: uppercase;
      letter-spacing: 0.08em;
    }
    .stat-card .value {
      font-size: 1.6rem;
      font-weight: 700;
      color: #60a5fa;
      margin-top: 0.2rem;
    }

    /* Bouton Start */
    #start-screen {
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 1.5rem;
      padding: 4rem;
      background: #1e2430;
      border: 1px solid #2d3748;
      border-radius: 12px;
      margin: 2rem auto;
      max-width: 500px;
    }
    #start-screen p { color: #94a3b8; font-size: 1rem; text-align: center; }

    #btn-start {
      background: #2563eb;
      color: #fff;
      border: none;
      border-radius: 8px;
      padding: 1rem 3rem;
      font-size: 1.2rem;
      font-weight: 700;
      cursor: pointer;
      transition: background 0.2s, transform 0.1s;
      letter-spacing: 0.05em;
    }
    #btn-start:hover  { background: #1d4ed8; transform: scale(1.03); }
    #btn-start:active { transform: scale(0.97); }
    #btn-start:disabled {
      background: #374151;
      color: #6b7280;
      cursor: default;
      transform: none;
    }

    table {
      width: 100%;
      border-collapse: collapse;
      background: #1e2430;
      border-radius: 8px;
      overflow: hidden;
    }

    thead tr { background: #16213e; }

    th {
      padding: 0.75rem 1rem;
      text-align: left;
      font-size: 0.72rem;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      color: #64748b;
    }

    td {
      padding: 0.7rem 1rem;
      font-size: 0.875rem;
      border-top: 1px solid #2d3748;
      transition: background 0.2s;
    }

    tr:hover td { background: #243044; }

    .badge {
      padding: 0.2rem 0.6rem;
      border-radius: 4px;
      font-size: 0.8rem;
      font-weight: 600;
      white-space: nowrap;
    }

    .live-dot {
      display: inline-block;
      width: 8px; height: 8px;
      background: #34d399;
      border-radius: 50%;
      margin-right: 6px;
      animation: pulse 1.5s infinite;
    }
    .live-dot.off { background: #6b7280; animation: none; }

    @keyframes pulse { 0%,100%{opacity:1} 50%{opacity:0.3} }

    #live-table { display: none; }
  </style>
</head>
<body>

  <header>
    <div>
      <h1>&#9992; CySky &mdash; Live Dashboard</h1>
      <p>
        <span id="dot" class="live-dot off"></span>
        <span id="status-label">En attente de demarrage...</span>
      </p>
    </div>
    <div class="clock-box">
      <div id="clock" class="clock">--:--</div>
      <div class="clock-label">Heure simulee</div>
    </div>
  </header>

  <!-- Ecran Start -->
  <div id="start-screen">
    <p>La simulation est prete.<br>Cliquez sur Start pour lancer la journee a l aeropport.</p>
    <button id="btn-start" onclick="startSim()">&#9654; Demarrer la simulation</button>
  </div>

  <!-- Dashboard live (cache jusqu au start) -->
  <div id="live-table">
    <div class="stats">
      <div class="stat-card">
        <div class="label">Total vols</div>
        <div id="s-total" class="value">0</div>
      </div>
      <div class="stat-card">
        <div class="label">En cours</div>
        <div id="s-active" class="value" style="color:#34d399">0</div>
      </div>
      <div class="stat-card">
        <div class="label">Partis</div>
        <div id="s-departed" class="value" style="color:#9ca3af">0</div>
      </div>
      <div class="stat-card">
        <div class="label">Pistes libres</div>
        <div id="s-runways" class="value" style="color:#fbbf24">0</div>
      </div>
      <div class="stat-card">
        <div class="label">Garages libres</div>
        <div id="s-garages" class="value" style="color:#a78bfa">0</div>
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
      <tbody id="flights-body"></tbody>
    </table>
  </div>

  <script>
    const STATUS_STYLES = {
      'En approche':           { bg: '#1d4ed8', fg: '#bfdbfe' },
      'Atterrissage en cours': { bg: '#0369a1', fg: '#bae6fd' },
      'Taxi vers piste':       { bg: '#92400e', fg: '#fde68a' },
      'Décollage en cours':    { bg: '#7e22ce', fg: '#e9d5ff' },
      'Parti':                 { bg: '#374151', fg: '#9ca3af' },
    };

    function statusBadge(status) {
      let style = STATUS_STYLES[status];
      if (!style) {
        // "Au sol — GATE_x" ou "Planifie"
        style = status.startsWith('Au sol')
          ? { bg: '#065f46', fg: '#6ee7b7' }
          : { bg: '#1e2430', fg: '#64748b' };
      }
      return `<span class="badge" style="background:${style.bg};color:${style.fg}">${status}</span>`;
    }

    async function startSim() {
      const btn = document.getElementById('btn-start');
      btn.disabled = true;
      btn.textContent = 'Demarrage...';
      await fetch('/start', { method: 'POST' });
    }

    function updateUI(state) {
      // Horloge
      document.getElementById('clock').textContent = state.simTime;
      document.getElementById('clock').className = 'clock' + (state.finished ? ' finished' : '');

      // Status label
      const dot = document.getElementById('dot');
      const label = document.getElementById('status-label');
      if (state.finished) {
        dot.className = 'live-dot off';
        label.textContent = 'Simulation terminee';
      } else if (state.started) {
        dot.className = 'live-dot';
        label.textContent = 'Simulation en cours — actualisation toutes les secondes';
      }

      // Afficher/cacher les sections
      if (state.started) {
        document.getElementById('start-screen').style.display = 'none';
        document.getElementById('live-table').style.display   = 'block';
        const btn = document.getElementById('btn-start');
        if (btn) { btn.disabled = true; btn.textContent = 'Simulation en cours...'; }
      }

      // Stats
      document.getElementById('s-total').textContent    = state.stats.total;
      document.getElementById('s-active').textContent   = state.stats.active;
      document.getElementById('s-departed').textContent = state.stats.departed;
      document.getElementById('s-runways').textContent  = state.stats.freeRunways;
      document.getElementById('s-garages').textContent  = state.stats.freeGarages;

      // Tableau
      const tbody = document.getElementById('flights-body');
      tbody.innerHTML = state.flights.map((f, i) => `
        <tr>
          <td>${i + 1}</td>
          <td><span class="badge" style="background:#1d4ed8;color:#bfdbfe">${f.flightId}</span></td>
          <td>${f.airplaneId}</td>
          <td>${f.arrivalTime}</td>
          <td>${f.departureTime}</td>
          <td>${f.runway}</td>
          <td><span class="badge" style="background:#064e3b;color:#6ee7b7">${f.destination}</span></td>
          <td>${statusBadge(f.status)}</td>
        </tr>
      `).join('');
    }

    async function poll() {
      try {
        const resp  = await fetch('/state');
        const state = await resp.json();
        updateUI(state);
        if (!state.finished) setTimeout(poll, 1000);
      } catch(e) {
        setTimeout(poll, 2000);
      }
    }

    poll();
  </script>
</body>
</html>"""
}
