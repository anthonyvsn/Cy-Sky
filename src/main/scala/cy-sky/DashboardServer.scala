package cysky

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import cysky.model.{EventType, InjectedEvent, UrgencyLevel}
import cysky.models.{Arrival, Departure}
import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.util.UUID

object DashboardServer {

  private val HHmm = DateTimeFormatter.ofPattern("HH:mm")

  def start(): Unit = {
    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    server.createContext("/",                 exchange => handle(exchange, routeRoot))
    server.createContext("/start",            exchange => handle(exchange, routeStart))
    server.createContext("/schedule",         exchange => handle(exchange, routeSchedule))
    server.createContext("/add-event",        exchange => handle(exchange, routeAddEvent))
    server.createContext("/state/libre",      exchange => handle(exchange, routeStateLibre))
    server.createContext("/state/controle",   exchange => handle(exchange, routeStateControle))
    server.setExecutor(null)
    server.start()
  }

  // ── Dispatcher ────────────────────────────────────────────────
  private def handle(
    ex:    HttpExchange,
    route: HttpExchange => (Int, String, String)
  ): Unit = {
    val (code, ct, body) = route(ex)
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    ex.getResponseHeaders.set("Content-Type", ct)
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

  // ── GET /schedule ─────────────────────────────────────────────
  private def routeSchedule(ex: HttpExchange): (Int, String, String) =
    (200, "application/json; charset=UTF-8", buildScheduleJson())

  // ── POST /add-event ───────────────────────────────────────────
  private def routeAddEvent(ex: HttpExchange): (Int, String, String) = {
    if (ex.getRequestMethod != "POST") return (405, "text/plain", "Method Not Allowed")
    val raw    = new String(ex.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
    val params = parseForm(raw)

    val evtType = params.get("type").flatMap(parseEventType).getOrElse(EventType.FlightDelay)
    val hour    = params.get("hour")  .flatMap(s => scala.util.Try(s.toInt).toOption).getOrElse(12)
    val minute  = params.get("minute").flatMap(s => scala.util.Try(s.toInt).toOption).getOrElse(0)
    val urgency = params.get("urgency").flatMap(parseUrgency).getOrElse(UrgencyLevel.Commercial)
    val note    = params.getOrElse("note", "")

    val event = InjectedEvent(
      id           = UUID.randomUUID().toString.take(8),
      eventType    = evtType,
      targetHour   = hour,
      targetMinute = minute,
      urgencyLevel = urgency,
      note         = note
    )
    SimState.addInjectedEvent(event)
    (200, "application/json", """{"ok":true}""")
  }

  // ── GET /state/libre ─────────────────────────────────────────
  private def routeStateLibre(ex: HttpExchange): (Int, String, String) =
    (200, "application/json; charset=UTF-8", buildStateJson(SimState.libre))

  // ── GET /state/controle ──────────────────────────────────────
  private def routeStateControle(ex: HttpExchange): (Int, String, String) =
    (200, "application/json; charset=UTF-8", buildStateJson(SimState.controle))

  // ── JSON builders ─────────────────────────────────────────────

  private def buildScheduleJson(): String = {
    val flights = SimState.scheduleSnapshot.values.flatten.toList
      .sortBy(_.scheduledTime)
      .map { f =>
        val kind = if (f.kind == Arrival) "ARR" else "DEP"
        s"""{"flightId":"${esc(f.flightId)}","airplaneId":"${esc(f.airplaneId)}","runway":"${esc(f.runway)}","time":"${f.scheduledTime.format(HHmm)}","destination":"${esc(f.destination)}","kind":"$kind"}"""
      }.mkString("[", ",", "]")

    val events = SimState.injectedEventsSnapshot.map(eventToJson).mkString("[", ",", "]")

    s"""{"flights":$flights,"events":$events}"""
  }

  private def eventToJson(e: InjectedEvent): String = {
    val tH = e.targetHour
    val tM = e.targetMinute
    val totalTrig = tH * 60 + tM - 30
    val trigH = ((totalTrig / 60) % 24 + 24) % 24
    val trigM = ((totalTrig % 60) + 60) % 60
    val triggerTime = f"$trigH%02d:$trigM%02d"
    val targetTime  = f"$tH%02d:$tM%02d"
    val typeName    = e.eventType.getClass.getSimpleName.stripSuffix("$")
    s"""{"id":"${e.id}","type":"$typeName","typeLabel":"${esc(e.eventType.displayName)}","triggerTime":"$triggerTime","targetTime":"$targetTime","urgency":"${e.urgencyLevel.label}","note":"${esc(e.note)}","status":"${e.status}"}"""
  }

  private def buildStateJson(slot: SimSlot): String = {
    val started  = SimState.isStarted
    val finished = slot.isFinished

    slot.snapshot match {
      case None =>
        s"""{"started":$started,"finished":$finished,"simTime":"--:--","stats":{"total":0,"active":0,"departed":0,"freeRunways":0,"freeGarages":0},"flights":[],"boomVersion":0,"boomMessage":""}"""

      case Some(data) =>
        val simTimeStr    = data.simTime.format(HHmm)
        val activeCount   = data.airplanes.size
        val departedCount = data.flightStates.values.count(_ == "Parti")
        val totalCount    = data.schedule.values.flatten.count(_.kind == Arrival) +
                            data.cancelledFlights.count(_.kind == Arrival)

        val cancelledFlightIds = data.cancelledFlights.map(_.flightId).toSet
        val allFlights = (data.schedule.values.flatten.toList ++ data.cancelledFlights)
          .sortBy(_.scheduledTime)

        val flightsJson = allFlights.map { f =>
            val kind   = if (f.kind == Arrival) "ARR" else "DEP"
            val status = if (cancelledFlightIds.contains(f.flightId)) "Annulé"
                         else data.flightStates.getOrElse(f.airplaneId, "Planifié")
            val delay  = data.delayInfo.getOrElse(f.flightId, 0)
            s"""{"flightId":"${esc(f.flightId)}","airplaneId":"${esc(f.airplaneId)}","time":"${f.scheduledTime.format(HHmm)}","kind":"$kind","runway":"${esc(f.runway)}","destination":"${esc(f.destination)}","status":"$status","delay":$delay}"""
          }.mkString(",")

        val boomV = slot.boomVersion
        val boomM = esc(slot.boomMessage)
        s"""{"started":$started,"finished":$finished,"simTime":"$simTimeStr","stats":{"total":$totalCount,"active":$activeCount,"departed":$departedCount,"freeRunways":${data.freeRunways.size},"freeGarages":${data.freeGarages.size}},"flights":[$flightsJson],"boomVersion":$boomV,"boomMessage":"$boomM"}"""
    }
  }

  // ── Helpers ───────────────────────────────────────────────────

  private def parseForm(body: String): Map[String, String] =
    body.split("&").flatMap { kv =>
      kv.split("=", 2) match {
        case Array(k, v) => Some(URLDecoder.decode(k, "UTF-8") -> URLDecoder.decode(v, "UTF-8"))
        case _           => None
      }
    }.toMap

  private def esc(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"")

  private def parseEventType(s: String): Option[EventType] = s match {
    case "EmergencyArrival"   => Some(EventType.EmergencyArrival)
    case "RunwayClosure"      => Some(EventType.RunwayClosure)
    case "FlightDelay"        => Some(EventType.FlightDelay)
    case "FlightCancellation" => Some(EventType.FlightCancellation)
    case _                    => None
  }

  private def parseUrgency(s: String): Option[UrgencyLevel] = s match {
    case "Civil"         => Some(UrgencyLevel.Civil)
    case "Commercial"    => Some(UrgencyLevel.Commercial)
    case "Military"      => Some(UrgencyLevel.Military)
    case "Medical"       => Some(UrgencyLevel.Medical)
    case "Presidential"  => Some(UrgencyLevel.Presidential)
    case "Emergency"     => Some(UrgencyLevel.Emergency)
    case _               => None
  }

  // ── Page HTML ─────────────────────────────────────────────────
  private val htmlPage: String = """<!DOCTYPE html>
<html lang="fr">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>CySky — Simulation Duale</title>
<style>
*{margin:0;padding:0;box-sizing:border-box}
body{font-family:'Segoe UI',sans-serif;background:#0a0e17;color:#c8d8f0;min-height:100vh}

/* ── Header ── */
header{display:flex;align-items:center;justify-content:space-between;padding:18px 40px;border-bottom:1px solid #1e2d4a;background:#0d1220}
.logo{font-size:22px;font-weight:700;letter-spacing:.1em;color:#f5a623}
.logo-sub{font-size:10px;color:#4a6080;letter-spacing:.08em;margin-top:2px}
.live-badge{display:flex;align-items:center;gap:6px;font-size:11px;font-family:monospace;color:#00e676}
.live-dot{width:7px;height:7px;border-radius:50%;background:#00e676;animation:pulse 2s ease-in-out infinite}
.live-dot.off{background:#4a6080;animation:none}
@keyframes pulse{0%,100%{opacity:1}50%{opacity:.3}}
@keyframes boom-flash{0%,100%{opacity:1;transform:scale(1)}50%{opacity:.6;transform:scale(1.05)}}

/* ── Layout pré-sim ── */
.page{max-width:1200px;margin:0 auto;padding:28px 40px}

/* ── Section titles ── */
.section-title{font-size:12px;font-weight:600;letter-spacing:.12em;text-transform:uppercase;color:#4a6080;margin-bottom:14px;display:flex;align-items:center;gap:8px}
.section-title::after{content:'';flex:1;height:1px;background:#1e2d4a}

/* ── Event form ── */
.form-card{background:#0f1520;border:1px solid #1e2d4a;border-radius:6px;padding:20px 24px;margin-bottom:28px}
.form-grid{display:grid;grid-template-columns:1fr 100px 80px 1fr 1fr;gap:10px;align-items:end}
.field{display:flex;flex-direction:column;gap:5px}
.field label{font-size:10px;letter-spacing:.08em;text-transform:uppercase;color:#4a6080}
.field select,.field input{background:#141c2e;border:1px solid #1e2d4a;border-radius:4px;padding:8px 10px;color:#c8d8f0;font-size:13px;outline:none;width:100%;transition:border-color .2s}
.field select:focus,.field input:focus{border-color:#f5a623}
.field input[type=number]{-moz-appearance:textfield}
.btn-add{background:#1e3a5f;border:1px solid #2a5080;border-radius:4px;padding:9px 20px;color:#40c4ff;font-size:12px;font-weight:600;letter-spacing:.06em;cursor:pointer;transition:all .2s;white-space:nowrap}
.btn-add:hover{background:#2a5080;border-color:#40c4ff}

/* ── Event chips ── */
.events-list{display:flex;flex-wrap:wrap;gap:8px;margin-top:14px;min-height:16px}
.event-chip{display:flex;align-items:center;gap:6px;background:#141c2e;border:1px solid #1e2d4a;border-radius:20px;padding:4px 12px 4px 8px;font-size:11px}
.chip-dot{width:8px;height:8px;border-radius:50%;flex-shrink:0}
.chip-label{color:#7a9ab8}
.chip-time{color:#f5a623;font-family:monospace}
.chip-note{color:#4a6080;font-style:italic}
.no-events{font-size:12px;color:#4a6080;font-style:italic}

/* ── Schedule table ── */
.table-card{background:#0f1520;border:1px solid #1e2d4a;border-radius:6px;overflow:hidden;margin-bottom:28px}
.sched-table{width:100%;border-collapse:collapse;font-size:13px}
.sched-table th{font-size:10px;letter-spacing:.1em;text-transform:uppercase;color:#4a6080;padding:10px 14px;border-bottom:1px solid #1e2d4a;text-align:left;background:#0a0e17}
.sched-table td{padding:9px 14px;border-bottom:1px solid rgba(30,45,74,.4);vertical-align:middle}
.sched-table tr:last-child td{border-bottom:none}
.sched-table tr:hover td{background:rgba(255,255,255,.015)}
.badge{display:inline-flex;align-items:center;font-size:10px;font-weight:700;padding:2px 7px;border-radius:3px;letter-spacing:.05em;font-family:monospace}
.badge-arr{background:#1a4a6e;color:#40c4ff}
.badge-dep{background:#00553a;color:#00e676}
.badge-evt{background:#5c1d00;color:#ff9800}
.badge-rwy{background:#141c2e;color:#7a9ab8;border:1px solid #1e2d4a}
.row-evt td{background:rgba(255,152,0,.04)}
.row-evt td:first-child{border-left:2px solid #ff9800}
.time-cell{font-family:monospace;color:#f5a623;font-size:13px}
.flight-id{font-family:monospace;color:#40c4ff}
.dest-cell{color:#7a9ab8}
.urgency-badge{font-size:10px;padding:1px 6px;border-radius:3px;font-weight:600}
.urg-Emergency{background:#5c1a1a;color:#ff5252}
.urg-Medical{background:#1a1a5c;color:#7986cb}
.urg-Military{background:#1a3a1a;color:#66bb6a}
.urg-Presidential{background:#3a2a00;color:#ffb300}
.urg-Commercial{background:#1a2a3a;color:#7a9ab8}
.urg-Civil{background:#141c2e;color:#4a6080}

/* ── Start button ── */
.start-bar{display:flex;justify-content:flex-end;margin-bottom:40px}
.btn-start{background:#1e3a1e;border:1px solid #2d602d;border-radius:6px;padding:14px 40px;color:#00e676;font-size:14px;font-weight:700;letter-spacing:.08em;cursor:pointer;transition:all .2s}
.btn-start:hover{background:#2d602d;box-shadow:0 0 20px rgba(0,230,118,.15)}
.btn-start:disabled{background:#141c2e;border-color:#1e2d4a;color:#4a6080;cursor:default;box-shadow:none}

/* ── Dual simulation layout ── */
#live-sim{display:none}
.dual-sim{display:grid;grid-template-columns:1fr 1fr;min-height:calc(100vh - 80px)}
.sim-panel{border-right:1px solid #1e2d4a;display:flex;flex-direction:column}
.sim-panel:last-child{border-right:none}

/* ── Panel headers ── */
.panel-title{padding:12px 24px;font-size:11px;font-weight:700;letter-spacing:.14em;text-transform:uppercase;border-bottom:1px solid #1e2d4a;display:flex;align-items:center;gap:8px}
.panel-libre   .panel-title{color:#00e676;background:rgba(0,230,118,.04);border-top:2px solid rgba(0,230,118,.3)}
.panel-controle .panel-title{color:#40c4ff;background:rgba(64,196,255,.04);border-top:2px solid rgba(64,196,255,.3)}
.panel-mode-dot{width:8px;height:8px;border-radius:50%;flex-shrink:0}
.panel-libre .panel-mode-dot{background:#00e676}
.panel-controle .panel-mode-dot{background:#40c4ff}

/* ── Stats bar per panel ── */
.stats-bar{display:flex;border-bottom:1px solid #1e2d4a;overflow-x:auto}
.stat{flex:1;min-width:80px;padding:10px 16px;border-right:1px solid #1e2d4a;display:flex;flex-direction:column;gap:2px}
.stat:last-child{border-right:none}
.stat-label{font-size:9px;letter-spacing:.1em;text-transform:uppercase;color:#4a6080;font-family:monospace}
.stat-value{font-size:18px;font-weight:600;color:#f5a623}
.stat-value.green{color:#00e676}.stat-value.blue{color:#40c4ff}

/* ── Live table per panel ── */
.panel-body{padding:16px 20px;flex:1;overflow-y:auto}
.panel-section-title{font-size:11px;font-weight:600;letter-spacing:.1em;text-transform:uppercase;color:#4a6080;margin-bottom:10px;display:flex;align-items:center;gap:6px}
.panel-section-title::after{content:'';flex:1;height:1px;background:#1e2d4a}
.panel-table-card{background:#0f1520;border:1px solid #1e2d4a;border-radius:4px;overflow:hidden}
.live-table{width:100%;border-collapse:collapse;font-size:12px}
.live-table th{font-size:9px;letter-spacing:.1em;text-transform:uppercase;color:#4a6080;padding:8px 10px;border-bottom:1px solid #1e2d4a;text-align:left;background:#0a0e17}
.live-table td{padding:7px 10px;border-bottom:1px solid rgba(30,45,74,.4);vertical-align:middle}
.live-table tr:hover td{background:rgba(255,255,255,.015)}

/* ── Badge retard ── */
.delay-badge{display:inline-flex;align-items:center;gap:3px;background:#2a1800;border:1px solid #7a4400;border-radius:3px;padding:1px 6px;font-size:10px;font-weight:700;color:#ff9800;font-family:monospace;white-space:nowrap;vertical-align:middle}
.delay-badge::before{content:'⏱';font-size:9px}

/* ── Statuts ── */
.st{display:inline-flex;align-items:center;gap:4px;font-size:10px;padding:2px 6px;border-radius:3px;font-weight:600}
.st::before{content:'';width:5px;height:5px;border-radius:50%;flex-shrink:0}
.st-scheduled{background:#141c2e;color:#4a6080}.st-scheduled::before{background:#4a6080}
.st-inprogress{background:#1a3a5c;color:#40c4ff}.st-inprogress::before{background:#40c4ff;animation:pulse 1s ease-in-out infinite}
.st-boarding{background:#3a2800;color:#ff9800}.st-boarding::before{background:#ff9800;animation:pulse 1.5s ease-in-out infinite}
.st-completed{background:#00553a;color:#00e676}.st-completed::before{background:#00e676}
.st-boom{background:#5c0000;color:#ff1744;font-weight:900;animation:boom-flash 0.5s ease-in-out infinite}.st-boom::before{background:#ff1744}
.st-cancelled{background:#2a1a00;color:#9e9e9e}.st-cancelled::before{background:#9e9e9e;opacity:.5}

/* ── Séparateur central ── */
.sim-divider{position:relative;display:flex;align-items:center;justify-content:center;width:1px;background:#1e2d4a}
.sim-divider-label{position:absolute;top:50%;transform:translateY(-50%);background:#0d1220;border:1px solid #1e2d4a;border-radius:4px;padding:4px 8px;font-size:9px;letter-spacing:.1em;color:#4a6080;white-space:nowrap;writing-mode:vertical-rl;text-orientation:mixed}

/* ── Toasts ── */
.toast{position:fixed;top:24px;right:24px;width:310px;border-radius:6px;padding:12px 15px;display:flex;flex-direction:column;gap:3px;pointer-events:auto;animation:toastIn .18s ease;transition:opacity .35s ease}
.toast-title{font-weight:700;font-size:12px;letter-spacing:.04em;display:flex;align-items:center;gap:5px}
.toast-body{color:#b0b8c8;font-size:11px;line-height:1.4;font-family:monospace;word-break:break-all}
.toast-side{font-size:9px;font-weight:700;letter-spacing:.1em;text-transform:uppercase;margin-top:2px}
.toast-side.libre{color:#00e676}.toast-side.controle{color:#40c4ff}
.toast-boom{background:#1e0005;border:1px solid #c62828;box-shadow:0 4px 24px rgba(255,23,68,.25)}
.toast-boom .toast-title{color:#ff5252}
.toast-delay{background:#1a1000;border:1px solid #5c3200;box-shadow:0 4px 16px rgba(255,152,0,.15)}
.toast-delay .toast-title{color:#ff9800}
.toast-cancelled{background:#111111;border:1px solid #333;box-shadow:0 4px 12px rgba(0,0,0,.4)}
.toast-cancelled .toast-title{color:#757575}
@keyframes toastIn{from{opacity:0;transform:translateX(20px)}to{opacity:1;transform:translateX(0)}}
</style>
</head>
<body>

  <header>
    <div>
      <h1>&#9992; CySky &mdash; Live Dashboard</h1>
      <p>
        <span id="live-dot" class="live-dot off"></span>
        <span id="live-label">En attente de demarrage...</span>
      </p>
    </div>
    <div class="clock-box">
      <div id="sim-clock" class="clock">--:--</div>
      <div class="clock-label">Heure simulee</div>
    </div>
  </header>

  <div id="pre-sim" class="page">
  <!-- Ecran Start -->
  <div id="start-screen">
    <p>La simulation est prête.<br>Cliquez sur Start pour lancer la journée à l'aéroport.</p>
    <button id="btn-start" onclick="startSim()">&#9654; Démarrer la simulation</button>
  </div>

  <!-- Dashboard live (cache jusqu au start) -->
  <div id="live-table">
    <div class="form-grid">
      <div class="field">
        <label>Type d'événement</label>
        <select id="evt-type">
          <option value="FlightDelay">Retard vol</option>
          <option value="FlightCancellation">Annulation vol</option>
          <option value="EmergencyArrival">Arrivée urgence</option>
          <option value="RunwayClosure">Fermeture piste</option>
        </select>
      </div>
      <div class="field">
        <label>Heure cible</label>
        <input type="number" id="evt-hour" min="6" max="23" value="12" placeholder="HH">
      </div>
      <div class="field">
        <label>Minute</label>
        <input type="number" id="evt-min" min="0" max="59" value="0" placeholder="MM">
      </div>
      <div class="field">
        <label>Urgence</label>
        <select id="evt-urgency">
          <option value="Commercial">Commercial</option>
          <option value="Civil">Civil</option>
          <option value="Military">Military</option>
          <option value="Medical">Medical</option>
          <option value="Presidential">Presidential</option>
          <option value="Emergency">Emergency</option>
        </select>
      </div>
      <div class="field">
        <label>Note</label>
        <input type="text" id="evt-note" placeholder="Description (optionnel)">
      </div>
    </div>
    <div style="display:flex;justify-content:flex-end;margin-top:12px">
      <button class="btn-add" onclick="addEvent()">+ Ajouter l'événement</button>
    </div>
    <div class="events-list" id="events-list">
      <span class="no-events">Aucun événement ajouté</span>
    </div>
  </div>

  <!-- Emploi du temps validé -->
  <div class="section-title">Emploi du temps validé — aucun deadlock</div>
  <div class="table-card">
    <table class="sched-table">
      <thead>
        <tr>
          <th>Heure</th>
          <th>Type</th>
          <th>Vol / Événement</th>
          <th>Avion</th>
          <th>Piste</th>
          <th>Destination / Détail</th>
          <th>Info</th>
        </tr>
      </thead>
      <tbody id="schedule-body">
        <tr><td colspan="7" style="text-align:center;color:#4a6080;padding:20px">Chargement...</td></tr>
      </tbody>
    </table>
  </div>

  <!-- Bouton lancer -->
  <div class="start-bar">
    <button class="btn-start" id="btn-start" onclick="startSim()">&#9654; Lancer les deux simulations</button>
  </div>
</div>

<!-- ═══ SIMULATION LIVE — DUAL ═══ -->
<div id="live-sim">
  <div class="dual-sim">

    <!-- ── Panneau gauche : Mode Libre ── -->
    <div class="sim-panel panel-libre">
      <div class="panel-title">
        <span class="panel-mode-dot"></span>
        Mode Libre
        <span style="margin-left:auto;font-size:10px;color:#4a6080;font-weight:400;letter-spacing:.04em">Sans vérification Pétri</span>
      </div>
      <div class="stats-bar" id="stats-libre"></div>
      <div class="panel-body">
        <div class="panel-section-title">Vols en cours</div>
        <div class="panel-table-card">
          <table class="live-table">
            <thead><tr><th>#</th><th>Heure</th><th>Type</th><th>Vol</th><th>Avion</th><th>Piste</th><th>Destination</th><th>État</th></tr></thead>
            <tbody id="live-body-libre"></tbody>
          </table>
        </div>
      </div>
    </div>

    <!-- ── Panneau droit : Mode Contrôle ── -->
    <div class="sim-panel panel-controle">
      <div class="panel-title">
        <span class="panel-mode-dot"></span>
        Mode Contrôle
        <span style="margin-left:auto;font-size:10px;color:#4a6080;font-weight:400;letter-spacing:.04em">Vérification Pétri active</span>
      </div>
      <div class="stats-bar" id="stats-controle"></div>
      <div class="panel-body">
        <div class="panel-section-title">Vols en cours</div>
        <div class="panel-table-card">
          <table class="live-table">
            <thead><tr><th>#</th><th>Heure</th><th>Type</th><th>Vol</th><th>Avion</th><th>Piste</th><th>Destination</th><th>État</th><th>Retard</th></tr></thead>
            <tbody id="live-body-controle"></tbody>
          </table>
        </div>
      </div>
    </div>

  </div>
</div>

<script>
var scheduleData = {flights:[], events:[]};

// ── Couleurs événements ──────────────────────────────────────────
var EVT_COLORS = {
  EmergencyArrival:   '#ff5252',
  RunwayClosure:      '#ff9800',
  FlightDelay:        '#7986cb',
  FlightCancellation: '#9e9e9e'
};

// ── Charger le schedule ──────────────────────────────────────────
function loadSchedule() {
  fetch('/schedule')
    .then(function(r){ return r.json(); })
    .then(function(data){
      scheduleData = data;
      renderSchedule();
      renderEventChips();
    });
}

// ── Rendre la table fusionnée (vols + événements) ────────────────
function renderSchedule() {
  var rows = [];
  scheduleData.flights.forEach(function(f) {
    rows.push({time: f.time, sortKey: f.time, kind: 'flight', data: f});
  });
  scheduleData.events.forEach(function(e) {
    rows.push({time: e.triggerTime, sortKey: e.triggerTime, kind: 'event', data: e});
  });
  rows.sort(function(a, b){ return a.sortKey.localeCompare(b.sortKey); });

  var tbody = document.getElementById('schedule-body');
  if (rows.length === 0) {
    tbody.innerHTML = '<tr><td colspan="7" style="text-align:center;color:#4a6080;padding:20px">Aucun vol planifié</td></tr>';
    return;
  }
  tbody.innerHTML = rows.map(function(row) {
    if (row.kind === 'flight') {
      var f = row.data;
      var badgeCls = f.kind === 'ARR' ? 'badge-arr' : 'badge-dep';
      var badgeTxt = f.kind === 'ARR' ? 'ARR' : 'DEP';
      return '<tr>' +
        '<td class="time-cell">' + f.time + '</td>' +
        '<td><span class="badge ' + badgeCls + '">' + badgeTxt + '</span></td>' +
        '<td class="flight-id">' + f.flightId + '</td>' +
        '<td style="color:#7a9ab8">' + f.airplaneId + '</td>' +
        '<td><span class="badge badge-rwy">' + f.runway + '</span></td>' +
        '<td class="dest-cell">' + (f.kind === 'DEP' ? f.destination : '—') + '</td>' +
        '<td></td></tr>';
    } else {
      var e = row.data;
      var col = EVT_COLORS[e.type] || '#ff9800';
      var urgCls = 'urg-' + e.urgency;
      return '<tr class="row-evt">' +
        '<td class="time-cell">' + e.triggerTime + '</td>' +
        '<td><span class="badge badge-evt">EVT</span></td>' +
        '<td style="color:' + col + ';font-weight:600">' + e.typeLabel + '</td>' +
        '<td style="color:#4a6080">—</td>' +
        '<td style="color:#4a6080">—</td>' +
        '<td style="color:#7a9ab8">' + (e.note || '—') + '</td>' +
        '<td><span class="urgency-badge ' + urgCls + '">' + e.urgency + '</span> <span style="color:#4a6080;font-size:10px">→ ' + e.targetTime + '</span></td>' +
        '</tr>';
    }
  }).join('');
}

// ── Chips événements ─────────────────────────────────────────────
function renderEventChips() {
  var el = document.getElementById('events-list');
  if (!scheduleData.events || scheduleData.events.length === 0) {
    el.innerHTML = '<span class="no-events">Aucun événement ajouté</span>';
    return;
  }
  el.innerHTML = scheduleData.events.map(function(e) {
    var col = EVT_COLORS[e.type] || '#ff9800';
    return '<div class="event-chip">' +
      '<span class="chip-dot" style="background:' + col + '"></span>' +
      '<span class="chip-label">' + e.typeLabel + '</span>' +
      '<span class="chip-time">' + e.triggerTime + '</span>' +
      (e.note ? '<span class="chip-note">— ' + e.note + '</span>' : '') +
      '<span class="urgency-badge urg-' + e.urgency + '" style="margin-left:4px">' + e.urgency + '</span>' +
      '</div>';
  }).join('');
}

// ── Ajouter un événement ─────────────────────────────────────────
function addEvent() {
  var type    = document.getElementById('evt-type').value;
  var hour    = document.getElementById('evt-hour').value;
  var minute  = document.getElementById('evt-min').value;
  var urgency = document.getElementById('evt-urgency').value;
  var note    = document.getElementById('evt-note').value;

  var body = 'type=' + encodeURIComponent(type) +
             '&hour=' + encodeURIComponent(hour) +
             '&minute=' + encodeURIComponent(minute) +
             '&urgency=' + encodeURIComponent(urgency) +
             '&note=' + encodeURIComponent(note);

  fetch('/add-event', {method:'POST', headers:{'Content-Type':'application/x-www-form-urlencoded'}, body: body})
    .then(function(){ return fetch('/schedule'); })
    .then(function(r){ return r.json(); })
    .then(function(data){
      scheduleData = data;
      renderSchedule();
      renderEventChips();
      document.getElementById('evt-note').value = '';
    });
}

// ── Lancer la simulation ─────────────────────────────────────────
function startSim() {
  var btn = document.getElementById('btn-start');
  btn.disabled = true;
  btn.textContent = 'Démarrage...';
  fetch('/start', {method:'POST'}).then(function(){
    document.getElementById('pre-sim').style.display  = 'none';
    document.getElementById('live-sim').style.display = 'block';
    document.getElementById('live-dot').className     = 'live-dot';
    document.getElementById('live-label').textContent = 'Simulation en cours';
    setTimeout(pollState, 300);
  });
}

// ── Polling live (les deux simulations en parallèle) ─────────────
var STATUS_STYLES = {
  'En approche':           {cls:'st-inprogress', lbl:'En approche'},
  'Atterrissage en cours': {cls:'st-inprogress', lbl:'Atterrissage'},
  'Taxi vers piste':       {cls:'st-boarding',   lbl:'Taxi'},
  'Décollage en cours':    {cls:'st-inprogress', lbl:'Décollage'},
  'Parti':                 {cls:'st-completed',  lbl:'Parti'},
  'BOOM':                  {cls:'st-boom',       lbl:'💥 BOOM'},
  'Annulé':                {cls:'st-cancelled',  lbl:'Annulé'}
};

function statusBadge(s) {
  var st = STATUS_STYLES[s];
  if (!st) {
    st = s.startsWith('Au sol') ? {cls:'st-completed', lbl:s} : {cls:'st-scheduled', lbl:s||'Planifié'};
  }
  return '<span class="st ' + st.cls + '">' + st.lbl + '</span>';
}

var lastBoomLibre    = 0;
var lastBoomControle = 0;
var seenDelays    = {};   // "side:flightId" -> true
var seenCancelled = {};   // "side:flightId" -> true
var toastZ        = 9000;

function showToast(type, title, body, side) {
  var t = document.createElement('div');
  t.className = 'toast toast-' + type;
  t.style.zIndex = ++toastZ;
  t.innerHTML =
    '<div class="toast-title">' + title + '</div>' +
    '<div class="toast-body">' + body + '</div>' +
    '<div class="toast-side ' + side + '">' + (side === 'libre' ? 'Mode Libre' : 'Mode Contrôle') + '</div>';
  document.body.appendChild(t);
  setTimeout(function() {
    t.style.opacity = '0';
    setTimeout(function() { if (t.parentNode) t.parentNode.removeChild(t); }, 380);
  }, 5000);
}

function delayBadge(delay) {
  if (!delay || delay <= 0) return '';
  return '<span class="delay-badge">+' + delay + ' min</span>';
}

function updatePanel(side, s) {
  var statsId    = 'stats-' + side;
  var bodyId     = 'live-body-' + side;
  var showDelay  = (side === 'controle');

  document.getElementById(statsId).innerHTML =
    stat('Vols', s.stats.total, '') +
    stat('En cours', s.stats.active, 'blue') +
    stat('Partis', s.stats.departed, 'green') +
    stat('Pistes', s.stats.freeRunways, '') +
    stat('Gates', s.stats.freeGarages, '');

  var rows = s.flights.map(function(f, i) {
    var badgeCls = f.kind === 'ARR' ? 'badge-arr' : 'badge-dep';
    var delayCell = showDelay ? '<td>' + delayBadge(f.delay) + '</td>' : '';
    return '<tr><td>' + (i+1) + '</td>' +
      '<td class="time-cell">' + f.time + '</td>' +
      '<td><span class="badge ' + badgeCls + '">' + f.kind + '</span></td>' +
      '<td class="flight-id">' + f.flightId + '</td>' +
      '<td style="color:#7a9ab8">' + f.airplaneId + '</td>' +
      '<td><span class="badge badge-rwy">' + f.runway + '</span></td>' +
      '<td class="dest-cell">' + f.destination + '</td>' +
      '<td>' + statusBadge(f.status) + '</td>' +
      delayCell + '</tr>';
  });
  document.getElementById(bodyId).innerHTML = rows.join('');

  // Toasts retard
  s.flights.forEach(function(f) {
    var dk = side + ':' + f.flightId + ':delay';
    var ck = side + ':' + f.flightId + ':cancel';
    if (f.delay > 0 && !seenDelays[dk]) {
      seenDelays[dk] = true;
      showToast('delay', '⏱ Vol retardé', f.flightId + '  +' + f.delay + ' min', side);
    }
    if (f.status === 'Annulé' && !seenCancelled[ck]) {
      seenCancelled[ck] = true;
      showToast('cancelled', 'Vol annulé', f.flightId + ' — ' + f.destination, side);
    }
  });

  // Toast BOOM
  if (side === 'libre' && s.boomVersion > lastBoomLibre) {
    lastBoomLibre = s.boomVersion;
    showToast('boom', '💥 Collision détectée', s.boomMessage, 'libre');
  }
  if (side === 'controle' && s.boomVersion > lastBoomControle) {
    lastBoomControle = s.boomVersion;
    showToast('boom', '💥 Collision détectée', s.boomMessage, 'controle');
  }
}

function pollState() {
  Promise.all([
    fetch('/state/libre').then(function(r){ return r.json(); }),
    fetch('/state/controle').then(function(r){ return r.json(); })
  ]).then(function(results) {
    var libre    = results[0];
    var controle = results[1];

    // Horloge = la plus avancée des deux (ou libre par défaut)
    var clk = libre.simTime !== '--:--' ? libre.simTime : controle.simTime;
    document.getElementById('sim-clock').textContent = clk;

    updatePanel('libre',    libre);
    updatePanel('controle', controle);

    var bothDone = libre.finished && controle.finished;
    if (bothDone) {
      document.getElementById('live-dot').className     = 'live-dot off';
      document.getElementById('live-label').textContent = 'Simulations terminées';
    } else {
      setTimeout(pollState, 1000);
    }
  }).catch(function(){ setTimeout(pollState, 1000); });
}

function stat(label, val, cls) {
  return '<div class="stat"><div class="stat-label">' + label + '</div><div class="stat-value ' + cls + '">' + val + '</div></div>';
}

// ── Init ────────────────────────────────────────────────────────
loadSchedule();
</script>
</body>
</html>"""
}
