package cysky

import cysky.models._
import cysky.actors._
import java.time.LocalTime
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object Main extends App {

  println("=== AeroSim — Démarrage ===\n")

  // ─── 1. Génération ────────────────────────────────────────────────────────
  val generator = ScheduleGenerator.default(
    runways               = 2,
    garages               = 3,
    start                 = LocalTime.of(6, 0),
    end                   = LocalTime.of(23, 59),
    acceleration          = 1000,
    maxGroundStayMinutes  = 360,
    minAvgGarageOccupancy = 0.60,
    seed                  = None
  )

  println("Génération du schedule...\n")
  val schedule   = generator.generate()
  val validation = generator.validate(schedule)
  println(s"Schedule généré — ${schedule.flights.size} vols")
  println(validation)
  println()

  // ─── 2. Export HTML ───────────────────────────────────────────────────────
  val json     = scheduleToJson(schedule, generator)
  val htmlPath = "schedule_output.html"
  Files.write(Paths.get(htmlPath), buildHtml(json).getBytes(StandardCharsets.UTF_8))
  println(s"Frontend généré : $htmlPath")

  val absPath = Paths.get(htmlPath).toAbsolutePath.toString
  openBrowser(absPath)
  println(s"Navigateur ouvert : file://$absPath")
  println("\n=== AeroSim — Schedule prêt ===")

  // ─── Sérialisation JSON ───────────────────────────────────────────────────

  def q(s: String): String = "\"" + s + "\""

  def flightToJson(f: Flight): String =
    s"""{${q("flightId")}:${q(f.flightId)},${q("airplaneId")}:${q(f.airplaneId)},${q("arrivalTime")}:${q(f.arrivalTime.toString)},${q("arrivalRunway")}:${q(f.arrivalRunway)},${q("departureRunway")}:${q(f.departureRunway)},${q("departureTime")}:${q(f.departureTime.toString)},${q("destination")}:${q(f.destination)},${q("boardingTime")}:${q(f.boardingTime.toString)},${q("gateId")}:${q(f.gateId)}}"""

  def scheduleToJson(s: Schedule, gen: ScheduleGenerator): String = {
    val byGarageJson = s.byGarage.toList.sortBy(_._1)
      .map { case (gid, fs) => s"${q(gid)}:[${fs.map(flightToJson).mkString(",")}]" }
      .mkString(",")

    val byRunwayJson = s.byRunway.toList.sortBy(_._1)
      .map { case (rid, fs) => s"${q(rid)}:[${fs.map(flightToJson).mkString(",")}]" }
      .mkString(",")

    val globalJson = gen.globalSchedule(s).map { e =>
      val t = e.operation match {
        case Landing => "ARR"
        case Takeoff => "DEP"
        case _       => "OTH"
      }
      s"""{${q("time")}:${q(e.time.toString)},${q("type")}:${q(t)},${q("runway")}:${q(e.runway)},${q("gate")}:${q(e.gate)},${q("flight")}:${flightToJson(e.flight)}}"""
    }.mkString(",")

    val (gRates, rRates, avgRate) = validation match {
      case ScheduleValid(_, gr, rr, avg) =>
        val g = gr.toList.sortBy(_._1).map { case (k, v) => s"${q(k)}:${f"$v%.2f"}" }.mkString(",")
        val r = rr.toList.sortBy(_._1).map { case (k, v) => s"${q(k)}:${f"$v%.2f"}" }.mkString(",")
        (g, r, avg)
      case _ => ("", "", 0.0)
    }

    // Injecter les durées d'opérations pour que le front soit synchronisé
    val opsJson =
      s"""${q("landing")}:${Landing.durationMinutes},""" +
      s"""${q("taxiToGarage")}:${TaxiToGarage.durationMinutes},""" +
      s"""${q("taxiToRunway")}:${TaxiToRunway.durationMinutes},""" +
      s"""${q("takeoff")}:${Takeoff.durationMinutes},""" +
      s"""${q("boarding")}:${Boarding.durationMinutes},""" +
      s"""${q("arrToGarage")}:${Landing.durationMinutes + TaxiToGarage.durationMinutes},""" +
      s"""${q("garageToTakeoff")}:${TaxiToRunway.durationMinutes + Takeoff.durationMinutes}"""

    s"""{${q("runwayCount")}:${s.runwayCount},${q("garageCount")}:${s.garageCount},""" +
    s"""${q("dayStart")}:${q(s.dayStart.toString)},${q("dayEnd")}:${q(s.dayEnd.toString)},""" +
    s"""${q("totalFlights")}:${s.flights.size},${q("avgGarageRate")}:${f"$avgRate%.2f"},""" +
    s"""${q("garageRates")}:{$gRates},${q("runwayRates")}:{$rRates},""" +
    s"""${q("ops")}:{$opsJson},""" +
    s"""${q("byGarage")}:{$byGarageJson},${q("byRunway")}:{$byRunwayJson},${q("global")}:[$globalJson]}"""
  }

  def buildHtml(json: String): String =
    htmlTemplate.replace("__SCHEDULE_DATA__", json)

  // ─── Ouverture navigateur (Linux / WSL / Mac / Windows) ──────────────────

  def openBrowser(path: String): Unit = {
    val url = "file://" + path
    val os  = System.getProperty("os.name").toLowerCase
    try {
      val isWsl = scala.util.Try(
        scala.io.Source.fromFile("/proc/version").mkString.toLowerCase.contains("microsoft")
      ).getOrElse(false)

      if (isWsl) {
        Runtime.getRuntime.exec(Array("explorer.exe", path))
      } else if (os.contains("linux")) {
        Runtime.getRuntime.exec(Array("xdg-open", url))
      } else if (os.contains("mac")) {
        Runtime.getRuntime.exec(Array("open", url))
      } else if (os.contains("win")) {
        Runtime.getRuntime.exec(Array("rundll32", "url.dll,FileProtocolHandler", url))
      }
    } catch {
      case e: Exception => println(s"Impossible d'ouvrir le navigateur : ${e.getMessage}")
    }
  }

  // ─── Template HTML ────────────────────────────────────────────────────────

  def htmlTemplate: String = """<!DOCTYPE html>
<html lang="fr">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>AeroSim — Emploi du temps</title>
<link href="https://fonts.googleapis.com/css2?family=Share+Tech+Mono&family=Barlow:wght@300;400;500;600&family=Barlow+Condensed:wght@400;600;700&display=swap" rel="stylesheet">
<style>
:root{--bg:#0a0e17;--bg2:#0f1520;--bg3:#141c2e;--border:#1e2d4a;--amber:#f5a623;--green:#00e676;--green-dim:#00553a;--red:#ff5252;--red-dim:#5c1d1d;--blue:#40c4ff;--blue-dim:#1a4a6e;--orange:#ff9800;--orange-dim:#5c3600;--purple:#ce93d8;--purple-dim:#4a1f5c;--text:#c8d8f0;--text-dim:#4a6080;--text-mid:#7a9ab8;--mono:'Share Tech Mono',monospace;--sans:'Barlow',sans-serif;--cond:'Barlow Condensed',sans-serif}
*{margin:0;padding:0;box-sizing:border-box}
body{background:var(--bg);color:var(--text);font-family:var(--sans);min-height:100vh;overflow-x:hidden}
header{padding:24px 48px;border-bottom:1px solid var(--border);display:flex;align-items:center;justify-content:space-between;background:linear-gradient(180deg,#0d1422 0%,transparent 100%)}
.logo{font-family:var(--cond);font-size:26px;font-weight:700;letter-spacing:.15em;color:var(--amber)}
.logo-sub{font-family:var(--mono);font-size:10px;color:var(--text-dim);letter-spacing:.1em;margin-top:2px}
.header-right{display:flex;flex-direction:column;align-items:flex-end;gap:4px}
.live-badge{display:flex;align-items:center;gap:6px;font-family:var(--mono);font-size:11px;color:var(--green)}
.live-dot{width:7px;height:7px;border-radius:50%;background:var(--green);animation:pulse 2s ease-in-out infinite}
@keyframes pulse{0%,100%{opacity:1}50%{opacity:.3}}
.clock{font-family:var(--mono);font-size:20px;color:var(--amber)}
.stats-bar{display:flex;border-bottom:1px solid var(--border);overflow-x:auto}
.stat{flex:1;min-width:130px;padding:14px 24px;border-right:1px solid var(--border);display:flex;flex-direction:column;gap:3px}
.stat-label{font-family:var(--mono);font-size:10px;color:var(--text-dim);letter-spacing:.1em;text-transform:uppercase}
.stat-value{font-family:var(--cond);font-size:24px;font-weight:600;color:var(--amber)}
.stat-value.green{color:var(--green)}.stat-value.blue{color:var(--blue)}
.tabs{display:flex;padding:0 48px;border-bottom:1px solid var(--border);background:var(--bg2)}
.tab{padding:13px 22px;font-family:var(--cond);font-size:12px;font-weight:600;letter-spacing:.1em;text-transform:uppercase;color:var(--text-dim);cursor:pointer;border-bottom:2px solid transparent;transition:all .2s;white-space:nowrap}
.tab:hover{color:var(--text-mid)}.tab.active{color:var(--amber);border-bottom-color:var(--amber)}
main{padding:28px 48px;position:relative;z-index:1}
.view{display:none}.view.active{display:block}
.st{display:inline-flex;align-items:center;gap:5px;font-family:var(--mono);font-size:10px;padding:3px 8px;border-radius:2px;font-weight:600;letter-spacing:.05em;white-space:nowrap}
.st::before{content:'';width:6px;height:6px;border-radius:50%;flex-shrink:0}
.st-scheduled{background:var(--bg3);color:var(--text-dim);border:1px solid var(--border)}.st-scheduled::before{background:var(--text-dim)}
.st-boarding{background:var(--orange-dim);color:var(--orange)}.st-boarding::before{background:var(--orange);animation:pulse 1.5s ease-in-out infinite}
.st-inprogress{background:var(--blue-dim);color:var(--blue)}.st-inprogress::before{background:var(--blue);animation:pulse 1s ease-in-out infinite}
.st-completed{background:var(--green-dim);color:var(--green)}.st-completed::before{background:var(--green)}
.st-cancelled{background:#2a1a2e;color:var(--purple)}.st-cancelled::before{background:var(--purple)}
.global-table{width:100%;border-collapse:collapse;font-size:13px}
.global-table th{font-family:var(--mono);font-size:10px;letter-spacing:.1em;text-transform:uppercase;color:var(--text-dim);padding:10px 14px;border-bottom:1px solid var(--border);text-align:left;background:var(--bg3);white-space:nowrap}
.global-table td{padding:8px 14px;border-bottom:1px solid rgba(30,45,74,.5);vertical-align:middle}
.global-table tr:hover td{background:rgba(255,255,255,.015)}.global-table tr:last-child td{border-bottom:none}
.filter-bar{display:flex;gap:10px;margin-bottom:18px;align-items:center;flex-wrap:wrap}
.filter-input{background:var(--bg3);border:1px solid var(--border);border-radius:3px;padding:7px 12px;font-family:var(--mono);font-size:12px;color:var(--text);outline:none;width:210px;transition:border-color .2s}
.filter-input::placeholder{color:var(--text-dim)}.filter-input:focus{border-color:var(--amber)}
.filter-btn{background:var(--bg3);border:1px solid var(--border);border-radius:3px;padding:7px 12px;font-family:var(--mono);font-size:11px;color:var(--text-mid);cursor:pointer;letter-spacing:.07em;transition:all .2s}
.filter-btn:hover,.filter-btn.active{border-color:var(--amber);color:var(--amber)}
.garages-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(380px,1fr));gap:18px}
.garage-card{background:var(--bg2);border:1px solid var(--border);border-radius:4px;overflow:hidden}
.garage-header{display:flex;align-items:center;justify-content:space-between;padding:11px 14px;background:var(--bg3);border-bottom:1px solid var(--border)}
.garage-id{font-family:var(--cond);font-size:14px;font-weight:700;letter-spacing:.1em;color:var(--amber)}
.tl-item{display:grid;grid-template-columns:58px 1fr 100px;align-items:center;gap:10px;padding:8px 14px;border-bottom:1px solid rgba(30,45,74,.5);transition:background .15s}
.tl-item:hover{background:rgba(255,255,255,.02)}.tl-item:last-child{border-bottom:none}
.tl-time{font-family:var(--mono);font-size:12px;color:var(--amber)}
.tl-body{display:flex;flex-direction:column;gap:2px}
.tl-id{font-family:var(--mono);font-size:12px;color:var(--blue)}
.tl-dest{font-size:11px;color:var(--text-dim)}
.tl-boarding{font-family:var(--mono);font-size:10px;color:var(--text-dim)}
.badge{display:inline-flex;align-items:center;justify-content:center;font-family:var(--mono);font-size:10px;padding:2px 6px;border-radius:2px;letter-spacing:.05em;font-weight:600}
.badge-arr{background:var(--blue-dim);color:var(--blue)}.badge-dep{background:var(--green-dim);color:var(--green)}.badge-rwy{background:var(--bg3);color:var(--text-mid);border:1px solid var(--border)}
.runways-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(460px,1fr));gap:18px}
.runway-card{background:var(--bg2);border:1px solid var(--border);border-radius:4px;overflow:hidden}
.runway-header{display:flex;align-items:center;justify-content:space-between;padding:11px 14px;background:var(--bg3);border-bottom:1px solid var(--border)}
.runway-id{font-family:var(--cond);font-size:14px;font-weight:700;letter-spacing:.1em;color:var(--blue)}
.rwy-row{display:grid;grid-template-columns:58px 72px 1fr 72px 110px;align-items:center;gap:10px;padding:8px 14px;border-bottom:1px solid rgba(30,45,74,.5);transition:background .15s}
.rwy-row:hover{background:rgba(255,255,255,.02)}.rwy-row:last-child{border-bottom:none}
::-webkit-scrollbar{width:5px;height:5px}::-webkit-scrollbar-track{background:var(--bg)}::-webkit-scrollbar-thumb{background:var(--border);border-radius:3px}
.mono{font-family:var(--mono)}.dim{color:var(--text-dim)}.mid{color:var(--text-mid)}
</style>
</head>
<body>
<header>
  <div>
    <div class="logo">AeroSim</div>
    <div class="logo-sub">FLIGHT OPERATIONS SYSTEM v1.0</div>
  </div>
  <div class="header-right">
    <div class="live-badge"><span class="live-dot"></span>SIMULATION ACTIVE</div>
    <div class="clock" id="clock">--:--:--</div>
  </div>
</header>
<div class="stats-bar" id="stats-bar"></div>
<nav class="tabs">
  <div class="tab active" onclick="switchTab('global',this)">Vue generale</div>
  <div class="tab" onclick="switchTab('gates',this)">Par gate</div>
  <div class="tab" onclick="switchTab('runways',this)">Par piste</div>
</nav>
<main>
  <div class="view active" id="view-global">
    <div class="filter-bar">
      <input class="filter-input" id="search-global" placeholder="Rechercher vol, gate, dest..." oninput="filterGlobal()">
      <button class="filter-btn active" onclick="filterType('all',this)">Tous</button>
      <button class="filter-btn" onclick="filterType('ARR',this)">Arrivees</button>
      <button class="filter-btn" onclick="filterType('DEP',this)">Departs</button>
    </div>
    <table class="global-table">
      <thead><tr><th>Heure</th><th>Type</th><th>Vol</th><th>Piste</th><th>Gate</th><th>Boarding</th><th>Destination</th><th>Statut</th></tr></thead>
      <tbody id="global-body"></tbody>
    </table>
  </div>
  <div class="view" id="view-gates"><div class="garages-grid" id="gates-grid"></div></div>
  <div class="view" id="view-runways"><div class="runways-grid" id="runways-grid"></div></div>
</main>
<script>
const DATA = __SCHEDULE_DATA__;
const OPS  = DATA.ops || {landing:10,taxiToGarage:8,taxiToRunway:8,takeoff:5,boarding:45,arrToGarage:18,garageToTakeoff:13};

let currentTypeFilter = 'all';

function toMins(t) {
  if (!t) return 0;
  const p = t.split(':').map(Number);
  return p[0] * 60 + p[1];
}

function simNow() {
  const now = new Date();
  return now.getHours() * 60 + now.getMinutes() + now.getSeconds() / 60;
}

function getStatus(event, nowMins) {
  const f   = event.flight;
  const arr = toMins(f.arrivalTime);
  const dep = toMins(f.departureTime);
  const boarding = toMins(f.boardingTime);

  if (event.type === 'ARR') {
    if (nowMins < arr)                           return {cls:'st-scheduled',  label:'Prevu'};
    if (nowMins < arr + OPS.landing)             return {cls:'st-inprogress', label:'Atterrissage'};
    return                                              {cls:'st-completed',  label:'Termine'};
  } else {
    const taxiDep = dep - OPS.taxiToRunway;
    if (nowMins < boarding)                      return {cls:'st-scheduled',  label:'Prevu'};
    if (nowMins < taxiDep)                       return {cls:'st-boarding',   label:'Embarquement'};
    if (nowMins < dep + OPS.takeoff)             return {cls:'st-inprogress', label:'Decollage'};
    return                                              {cls:'st-completed',  label:'Parti'};
  }
}

function flightStatus(f, now) {
  const arr      = toMins(f.arrivalTime);
  const dep      = toMins(f.departureTime);
  const boarding = toMins(f.boardingTime);
  const taxiDep  = dep - OPS.taxiToRunway;
  if (now < arr)                      return {cls:'st-scheduled',  label:'Prevu'};
  if (now < arr + OPS.arrToGarage)    return {cls:'st-inprogress', label:'Atterrissage'};
  if (now < boarding)                 return {cls:'st-scheduled',  label:'Au sol'};
  if (now < taxiDep)                  return {cls:'st-boarding',   label:'Embarquement'};
  if (now < dep + OPS.takeoff)        return {cls:'st-inprogress', label:'Decollage'};
  return                                     {cls:'st-completed',  label:'Parti'};
}

function renderStats() {
  document.getElementById('stats-bar').innerHTML =
    '<div class="stat"><div class="stat-label">Vols totaux</div><div class="stat-value">' + DATA.totalFlights + '</div></div>' +
    '<div class="stat"><div class="stat-label">Arrivees</div><div class="stat-value blue">' + DATA.totalFlights + '</div></div>' +
    '<div class="stat"><div class="stat-label">Departs</div><div class="stat-value green">' + DATA.totalFlights + '</div></div>' +
    '<div class="stat"><div class="stat-label">Gates</div><div class="stat-value">' + DATA.garageCount + '</div></div>' +
    '<div class="stat"><div class="stat-label">Pistes</div><div class="stat-value">' + DATA.runwayCount + '</div></div>';
}

function renderGlobal(events) {
  const now = simNow();
  const rows = events.map(function(e) {
    const st = getStatus(e, now);
    return '<tr data-type="' + e.type + '">' +
      '<td><span class="mono" style="color:var(--amber)">' + e.time + '</span></td>' +
      '<td><span class="badge ' + (e.type==='ARR'?'badge-arr':'badge-dep') + '">' + e.type + '</span></td>' +
      '<td><span class="mono" style="color:var(--blue)">' + e.flight.flightId + '</span></td>' +
      '<td><span class="badge badge-rwy">' + e.runway + '</span></td>' +
      '<td><span class="mono mid">' + e.flight.gateId + '</span></td>' +
      '<td><span class="mono dim">' + (e.type==='DEP'?e.flight.boardingTime:'--') + '</span></td>' +
      '<td><span style="color:var(--text-mid)">' + (e.type==='DEP'?e.flight.destination:'--') + '</span></td>' +
      '<td><span class="st ' + st.cls + '">' + st.label + '</span></td>' +
      '</tr>';
  });
  document.getElementById('global-body').innerHTML = rows.join('');
}

function filterGlobal() {
  const el = document.getElementById('search-global');
  const q  = el ? el.value.toLowerCase() : '';
  const filtered = (DATA.global || []).filter(function(e) {
    const mt = currentTypeFilter === 'all' || e.type === currentTypeFilter;
    const mq = !q ||
      e.flight.flightId.toLowerCase().indexOf(q) >= 0 ||
      e.flight.gateId.toLowerCase().indexOf(q) >= 0 ||
      (e.flight.destination||'').toLowerCase().indexOf(q) >= 0 ||
      e.runway.toLowerCase().indexOf(q) >= 0;
    return mt && mq;
  });
  renderGlobal(filtered);
}

function filterType(type, btn) {
  currentTypeFilter = type;
  var btns = document.querySelectorAll('.filter-btn');
  for (var i=0; i<btns.length; i++) btns[i].classList.remove('active');
  btn.classList.add('active');
  filterGlobal();
}

function renderGates() {
  const now = simNow();
  var html = '';
  var gids = Object.keys(DATA.byGarage).sort();
  for (var gi=0; gi<gids.length; gi++) {
    var gid = gids[gi];
    var flights = DATA.byGarage[gid] || [];
    var rows = '';
    for (var i=0; i<flights.length; i++) {
      var f  = flights[i];
      var st = flightStatus(f, now);
      rows +=
        '<div class="tl-item">' +
        '<div class="tl-time">' + f.arrivalTime + '</div>' +
        '<div class="tl-body">' +
          '<div class="tl-id">' + f.flightId + '</div>' +
          '<div class="tl-dest">-> ' + f.destination + ' | DEP ' + f.departureTime + '</div>' +
          '<div class="tl-boarding">BOARDING ' + f.boardingTime + '</div>' +
        '</div>' +
        '<div style="display:flex;flex-direction:column;gap:4px;align-items:flex-end">' +
          '<span class="st ' + st.cls + '" style="font-size:9px;padding:2px 5px">' + st.label + '</span>' +
          '<span class="badge badge-arr">' + f.arrivalRunway + '</span>' +
          '<span class="badge badge-dep">' + f.departureRunway + '</span>' +
        '</div>' +
        '</div>';
    }
    html +=
      '<div class="garage-card">' +
        '<div class="garage-header"><div class="garage-id">' + gid + '</div>' +
        '<span class="mono dim">' + flights.length + ' vols</span></div>' +
        '<div>' + rows + '</div>' +
      '</div>';
  }
  document.getElementById('gates-grid').innerHTML = html;
}

function renderRunways() {
  const now = simNow();
  var html = '';
  var rids = Object.keys(DATA.byRunway).sort();
  for (var ri=0; ri<rids.length; ri++) {
    var rid = rids[ri];
    var ops = (DATA.global || []).filter(function(e){ return e.runway === rid; })
                .sort(function(a,b){ return a.time.localeCompare(b.time); });
    var arrCount = ops.filter(function(e){ return e.type==='ARR'; }).length;
    var depCount = ops.filter(function(e){ return e.type==='DEP'; }).length;
    var rows = '';
    for (var i=0; i<ops.length; i++) {
      var e  = ops[i];
      var st = getStatus(e, now);
      rows +=
        '<div class="rwy-row">' +
        '<span class="mono" style="color:var(--amber)">' + e.time + '</span>' +
        '<span class="badge ' + (e.type==='ARR'?'badge-arr':'badge-dep') + '">' + e.type + '</span>' +
        '<div>' +
          '<div class="mono" style="color:var(--blue);font-size:12px">' + e.flight.flightId + '</div>' +
          '<div class="dim" style="font-size:11px">' + (e.type==='DEP'?'-> '+e.flight.destination:'<- '+e.flight.gateId) + '</div>' +
        '</div>' +
        '<span class="badge badge-rwy">' + e.flight.gateId + '</span>' +
        '<span class="st ' + st.cls + '">' + st.label + '</span>' +
        '</div>';
    }
    html +=
      '<div class="runway-card">' +
        '<div class="runway-header">' +
          '<div class="runway-id">' + rid + '</div>' +
          '<div style="display:flex;gap:7px">' +
            '<span class="badge badge-arr">' + arrCount + ' ARR</span>' +
            '<span class="badge badge-dep">' + depCount + ' DEP</span>' +
          '</div>' +
        '</div>' +
        '<div>' + rows + '</div>' +
      '</div>';
  }
  document.getElementById('runways-grid').innerHTML = html;
}

function switchTab(name, el) {
  var tabs = document.querySelectorAll('.tab');
  var views = document.querySelectorAll('.view');
  for (var i=0; i<tabs.length; i++) tabs[i].classList.remove('active');
  for (var i=0; i<views.length; i++) views[i].classList.remove('active');
  el.classList.add('active');
  document.getElementById('view-' + name).classList.add('active');
}

function updateClock() {
  var now = new Date();
  var h = String(now.getHours()).padStart(2,'0');
  var m = String(now.getMinutes()).padStart(2,'0');
  var s = String(now.getSeconds()).padStart(2,'0');
  document.getElementById('clock').textContent = h+':'+m+':'+s;
}

function fullRefresh() {
  filterGlobal();
  renderGates();
  renderRunways();
}

function init() {
  renderStats();
  filterGlobal();
  renderGates();
  renderRunways();
  updateClock();
  setInterval(updateClock, 1000);
  setInterval(fullRefresh, 30000);
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}
</script>
</body>
</html>"""
}