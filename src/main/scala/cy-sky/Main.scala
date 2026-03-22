package cysky

import cysky.models._
import cysky.actors._
import java.time.LocalTime
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object Main extends App {

  println("=== AeroSim — Démarrage ===\n")

  val generator = ScheduleGenerator.default(
    runways               = 2,
    garages               = 3,
    start                 = LocalTime.of(6, 0),
    end                   = LocalTime.of(23, 59),
    acceleration          = 1000,
    maxGroundStayMinutes  = 360,
    minAvgGarageOccupancy = 0.60,
    seed                  = Some(42L)
  )

  println("Génération du schedule...\n")
  val schedule   = generator.generate()
  val validation = generator.validate(schedule)
  println(s"Schedule généré — ${schedule.flights.size} vols")
  println(validation)
  println()

  val json     = scheduleToJson(schedule, generator)
  val htmlPath = "schedule_output.html"
  Files.write(Paths.get(htmlPath), buildHtml(json).getBytes(StandardCharsets.UTF_8))
  println(s"Frontend généré : $htmlPath")

  val absPath = Paths.get(htmlPath).toAbsolutePath.toString
  openBrowser(absPath)
  println(s"Navigateur ouvert : file://$absPath")
  println("\n=== AeroSim — Schedule prêt ===")

  // ─── Sérialisation JSON ───────────────────────────────────────────────────

  def q(s: String) = s""""$s""""

  def flightToJson(f: Flight): String =
    s"""{"flightId":${q(f.flightId)},"airplaneId":${q(f.airplaneId)},"arrivalTime":${q(f.arrivalTime.toString)},"arrivalRunway":${q(f.arrivalRunway)},"departureRunway":${q(f.departureRunway)},"departureTime":${q(f.departureTime.toString)},"destination":${q(f.destination)},"boardingTime":${q(f.boardingTime.toString)},"gateId":${q(f.gateId)}}"""

  def scheduleToJson(s: Schedule, gen: ScheduleGenerator): String = {
    val byGarageJson = s.byGarage.toList.sortBy(_._1)
      .map { case (gid, fs) => s"${q(gid)}:[${fs.map(flightToJson).mkString(",")}]" }
      .mkString(",")

    val byRunwayJson = s.byRunway.toList.sortBy(_._1)
      .map { case (rid, fs) => s"${q(rid)}:[${fs.map(flightToJson).mkString(",")}]" }
      .mkString(",")

    val globalJson = gen.globalSchedule(s).map { e =>
      val t = e.operation match { case Landing => "ARR"; case Takeoff => "DEP"; case _ => "OTH" }
      s"""{"time":${q(e.time.toString)},"type":${q(t)},"runway":${q(e.runway)},"gate":${q(e.gate)},"flight":${flightToJson(e.flight)}}"""
    }.mkString(",")

    val (gRates, rRates, avgRate) = validation match {
      case ScheduleValid(_, gr, rr, avg) =>
        val g = gr.toList.sortBy(_._1).map { case (k, v) => s"${q(k)}:${f"$v%.2f"}" }.mkString(",")
        val r = rr.toList.sortBy(_._1).map { case (k, v) => s"${q(k)}:${f"$v%.2f"}" }.mkString(",")
        (g, r, avg)
      case _ => ("", "", 0.0)
    }

    s"""{"runwayCount":${s.runwayCount},"garageCount":${s.garageCount},"dayStart":${q(s.dayStart.toString)},"dayEnd":${q(s.dayEnd.toString)},"totalFlights":${s.flights.size},"avgGarageRate":${f"$avgRate%.2f"},"garageRates":{$gRates},"runwayRates":{$rRates},"byGarage":{$byGarageJson},"byRunway":{$byRunwayJson},"global":[$globalJson]}"""
  }

  def buildHtml(json: String): String =
    htmlTemplate.replace("__SCHEDULE_DATA__", json)

  def openBrowser(path: String): Unit = {
    val url = s"file://$path"
    val os  = System.getProperty("os.name").toLowerCase
    try {
      if      (os.contains("linux")) Runtime.getRuntime.exec(Array("xdg-open", url))
      else if (os.contains("mac"))   Runtime.getRuntime.exec(Array("open", url))
      else if (os.contains("win"))   Runtime.getRuntime.exec(Array("rundll32", "url.dll,FileProtocolHandler", url))
    } catch { case e: Exception => println(s"Impossible d'ouvrir le navigateur : ${e.getMessage}") }
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
body::before{content:'';position:fixed;inset:0;background-image:url("data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.03'/%3E%3C/svg%3E");pointer-events:none;z-index:0}
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
/* ── Status badges ── */
.st{display:inline-flex;align-items:center;gap:5px;font-family:var(--mono);font-size:10px;padding:3px 8px;border-radius:2px;font-weight:600;letter-spacing:.05em;white-space:nowrap}
.st::before{content:'';width:6px;height:6px;border-radius:50%;flex-shrink:0}
.st-scheduled{background:var(--bg3);color:var(--text-dim);border:1px solid var(--border)}.st-scheduled::before{background:var(--text-dim)}
.st-boarding{background:var(--orange-dim);color:var(--orange)}.st-boarding::before{background:var(--orange);animation:pulse 1.5s ease-in-out infinite}
.st-inprogress{background:var(--blue-dim);color:var(--blue)}.st-inprogress::before{background:var(--blue);animation:pulse 1s ease-in-out infinite}
.st-completed{background:var(--green-dim);color:var(--green)}.st-completed::before{background:var(--green)}
.st-delayed{background:var(--red-dim);color:var(--red)}.st-delayed::before{background:var(--red)}
.st-cancelled{background:#2a1a2e;color:var(--purple)}.st-cancelled::before{background:var(--purple)}
/* ── Global table ── */
.global-table{width:100%;border-collapse:collapse;font-size:13px}
.global-table th{font-family:var(--mono);font-size:10px;letter-spacing:.1em;text-transform:uppercase;color:var(--text-dim);padding:10px 14px;border-bottom:1px solid var(--border);text-align:left;background:var(--bg3);white-space:nowrap}
.global-table td{padding:8px 14px;border-bottom:1px solid rgba(30,45,74,.5);vertical-align:middle}
.global-table tr:hover td{background:rgba(255,255,255,.015)}.global-table tr:last-child td{border-bottom:none}
/* ── Filter bar ── */
.filter-bar{display:flex;gap:10px;margin-bottom:18px;align-items:center;flex-wrap:wrap}
.filter-input{background:var(--bg3);border:1px solid var(--border);border-radius:3px;padding:7px 12px;font-family:var(--mono);font-size:12px;color:var(--text);outline:none;width:210px;transition:border-color .2s}
.filter-input::placeholder{color:var(--text-dim)}.filter-input:focus{border-color:var(--amber)}
.filter-btn{background:var(--bg3);border:1px solid var(--border);border-radius:3px;padding:7px 12px;font-family:var(--mono);font-size:11px;color:var(--text-mid);cursor:pointer;letter-spacing:.07em;transition:all .2s}
.filter-btn:hover,.filter-btn.active{border-color:var(--amber);color:var(--amber)}
/* ── Gates grid ── */
.garages-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(380px,1fr));gap:18px}
.garage-card{background:var(--bg2);border:1px solid var(--border);border-radius:4px;overflow:hidden;animation:fadeUp .4s ease both}
@keyframes fadeUp{from{opacity:0;transform:translateY(10px)}to{opacity:1;transform:translateY(0)}}
.garage-header{display:flex;align-items:center;justify-content:space-between;padding:11px 14px;background:var(--bg3);border-bottom:1px solid var(--border)}
.garage-id{font-family:var(--cond);font-size:14px;font-weight:700;letter-spacing:.1em;color:var(--amber)}
.occ-wrap{display:flex;align-items:center;gap:7px}
.occ-label{font-family:var(--mono);font-size:11px;color:var(--text-mid)}
.occ-bar{width:72px;height:3px;background:var(--border);border-radius:2px;overflow:hidden}
.occ-fill{height:100%;background:var(--green);transition:width 1s ease}
.tl-item{display:grid;grid-template-columns:58px 1fr 100px;align-items:center;gap:10px;padding:8px 14px;border-bottom:1px solid rgba(30,45,74,.5);transition:background .15s}
.tl-item:hover{background:rgba(255,255,255,.02)}.tl-item:last-child{border-bottom:none}
.tl-time{font-family:var(--mono);font-size:12px;color:var(--amber)}
.tl-body{display:flex;flex-direction:column;gap:2px}
.tl-id{font-family:var(--mono);font-size:12px;color:var(--blue)}
.tl-dest{font-size:11px;color:var(--text-dim)}
.tl-boarding{font-family:var(--mono);font-size:10px;color:var(--text-dim)}
.badge{display:inline-flex;align-items:center;justify-content:center;font-family:var(--mono);font-size:10px;padding:2px 6px;border-radius:2px;letter-spacing:.05em;font-weight:600}
.badge-arr{background:var(--blue-dim);color:var(--blue)}.badge-dep{background:var(--green-dim);color:var(--green)}.badge-rwy{background:var(--bg3);color:var(--text-mid);border:1px solid var(--border)}
/* ── Runways ── */
.runways-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(460px,1fr));gap:18px}
.runway-card{background:var(--bg2);border:1px solid var(--border);border-radius:4px;overflow:hidden;animation:fadeUp .4s ease both}
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
  <div class="tab active" onclick="switchTab('global',this)">Vue générale</div>
  <div class="tab" onclick="switchTab('gates',this)">Par gate</div>
  <div class="tab" onclick="switchTab('runways',this)">Par piste</div>
</nav>
<main>
  <div class="view active" id="view-global">
    <div class="filter-bar">
      <input class="filter-input" id="search-global" placeholder="Rechercher vol, gate, dest..." oninput="filterGlobal()">
      <button class="filter-btn active" onclick="filterType('all',this)">Tous</button>
      <button class="filter-btn" onclick="filterType('ARR',this)">Arrivées</button>
      <button class="filter-btn" onclick="filterType('DEP',this)">Départs</button>
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

// ── Heure simulée = heure réelle courante en minutes depuis minuit ──
// Les heures du schedule (06:00, 11:25...) sont en minutes absolues depuis minuit.
// On compare directement avec l'heure réelle du navigateur.
// Ex: si maintenant = 13h42, simNow() = 822. Un vol à 11:25 (685 min) est donc "Terminé".
function simNow() {
  const now = new Date();
  return now.getHours() * 60 + now.getMinutes() + now.getSeconds() / 60;
}

function toMins(t) {
  if (!t) return 0;
  const [h, m] = t.split(':').map(Number);
  return h * 60 + m;
}

/**
 * Calcule le statut d'un événement selon l'heure simulée courante.
 *
 * Pour un ARR (atterrissage) :
 *   - scheduled   : avant arrivalTime
 *   - in-progress : pendant Landing (arrivalTime + 10 min)
 *   - completed   : après arrivalTime + Landing
 *
 * Pour un DEP (décollage) :
 *   - scheduled   : avant boardingTime
 *   - boarding    : entre boardingTime et departureTime - TaxiToRunway
 *   - in-progress : pendant Takeoff (departureTime + 5 min)
 *   - completed   : après departureTime + Takeoff
 */
function getStatus(event, nowMins) {
  const f = event.flight;
  if (event.type === 'ARR') {
    const arr = toMins(f.arrivalTime);
    const end = arr + 10; // Landing duration
    if (nowMins < arr)        return { cls: 'st-scheduled',  label: 'Prévu' };
    if (nowMins < end)        return { cls: 'st-inprogress', label: 'Atterrissage' };
    return                           { cls: 'st-completed',  label: 'Terminé' };
  } else {
    const dep      = toMins(f.departureTime);
    const boarding = toMins(f.boardingTime);
    const taxiDep  = dep - 8; // TaxiToRunway
    const end      = dep + 5; // Takeoff duration
    if (nowMins < boarding)   return { cls: 'st-scheduled',  label: 'Prévu' };
    if (nowMins < taxiDep)    return { cls: 'st-boarding',   label: 'Embarquement' };
    if (nowMins < end)        return { cls: 'st-inprogress', label: 'Décollage' };
    return                           { cls: 'st-completed',  label: 'Parti' };
  }
}

// ── Rendu ──────────────────────────────────────────────────────
let currentTypeFilter = 'all';

function renderStats() {
  document.getElementById('stats-bar').innerHTML =
    `<div class="stat"><div class="stat-label">Vols totaux</div><div class="stat-value">${DATA.totalFlights}</div></div>
     <div class="stat"><div class="stat-label">Arrivées</div><div class="stat-value blue">${DATA.totalFlights}</div></div>
     <div class="stat"><div class="stat-label">Départs</div><div class="stat-value green">${DATA.totalFlights}</div></div>
     <div class="stat"><div class="stat-label">Gates</div><div class="stat-value">${DATA.garageCount}</div></div>
     <div class="stat"><div class="stat-label">Pistes</div><div class="stat-value">${DATA.runwayCount}</div></div>
     <div class="stat"><div class="stat-label">Occ. moy.</div><div class="stat-value ${parseFloat(occ)>=60?'green':''}">${occ}%</div></div>
     ${Object.entries(DATA.garageRates).sort().map(([g,r])=>
       `<div class="stat"><div class="stat-label">${g}</div><div class="stat-value ${r>=0.6?'green':''}">${(r*100).toFixed(0)}%</div></div>`
     ).join('')}`;
}

function renderGlobal(events) {
  const now = simNow();
  document.getElementById('global-body').innerHTML = events.map((e, i) => {
    const st = getStatus(e, now);
    return `<tr data-type="${e.type}" style="animation:fadeUp .25s ease ${Math.min(i*.01,.4)}s both">
      <td><span class="mono" style="color:var(--amber)">${e.time}</span></td>
      <td><span class="badge ${e.type==='ARR'?'badge-arr':'badge-dep'}">${e.type}</span></td>
      <td><span class="mono" style="color:var(--blue)">${e.flight.flightId}</span></td>
      <td><span class="badge badge-rwy">${e.runway}</span></td>
      <td><span class="mono mid">${e.flight.gateId}</span></td>
      <td><span class="mono dim">${e.type==='DEP'?e.flight.boardingTime:'—'}</span></td>
      <td><span style="color:var(--text-mid);letter-spacing:.04em">${e.type==='DEP'?e.flight.destination:'—'}</span></td>
      <td><span class="st ${st.cls}">${st.label}</span></td>
    </tr>`;
  }).join('');
}

function filterGlobal() {
  const q = document.getElementById('search-global').value.toLowerCase();
  const filtered = DATA.global.filter(e => {
    const mt = currentTypeFilter === 'all' || e.type === currentTypeFilter;
    const mq = !q || e.flight.flightId.toLowerCase().includes(q) ||
               e.flight.gateId.toLowerCase().includes(q) ||
               (e.flight.destination||'').toLowerCase().includes(q) ||
               e.runway.toLowerCase().includes(q);
    return mt && mq;
  });
  renderGlobal(filtered);
}

function filterType(type, btn) {
  currentTypeFilter = type;
  document.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));
  btn.classList.add('active');
  filterGlobal();
}

function garageOcc(gateId) {
  const fs = DATA.byGarage[gateId] || [];
  const dayMin = toMins(DATA.dayEnd) - toMins(DATA.dayStart);
  let occ = 0;
  fs.forEach(f => {
    // Gate occupée de (arrivalTime + Landing(10) + TaxiToGarage(8))
    // jusqu'à (departureTime - TaxiToRunway(8) - Takeoff(5))
    // = departureTime - 13
    const inn = toMins(f.arrivalTime) + 18;   // arrToGarage = Landing + TaxiToGarage
    const out = toMins(f.departureTime) - 13; // garageToTakeoff = TaxiToRunway + Takeoff
    occ += Math.max(0, out - inn);
  });
  return Math.min(1, occ / dayMin);
}

function flightStatus(f, now) {
  const arr = toMins(f.arrivalTime);
  const dep = toMins(f.departureTime);
  const boarding = toMins(f.boardingTime);
  const taxiDep  = dep - 8;
  if (now < arr)         return { cls: 'st-scheduled',  label: 'Prévu' };
  if (now < arr + 18)    return { cls: 'st-inprogress', label: 'Atterrissage' };
  if (now < boarding)    return { cls: 'st-scheduled',  label: 'Au sol' };
  if (now < taxiDep)     return { cls: 'st-boarding',   label: 'Embarquement' };
  if (now < dep + 5)     return { cls: 'st-inprogress', label: 'Décollage' };
  return                        { cls: 'st-completed',  label: 'Parti' };
}

function renderGates() {
  const now = simNow();
  document.getElementById('gates-grid').innerHTML =
    Object.entries(DATA.byGarage).sort().map(([gid, flights], ci) => {
      const occ = garageOcc(gid);
      const rows = flights.map(f => {
        const st = flightStatus(f, now);
        return `<div class="tl-item">
          <div class="tl-time">${f.arrivalTime}</div>
          <div class="tl-body">
            <div class="tl-id">${f.flightId}</div>
            <div class="tl-dest">→ ${f.destination} &nbsp;|&nbsp; DEP ${f.departureTime}</div>
            <div class="tl-boarding">BOARDING ${f.boardingTime}</div>
          </div>
          <div style="display:flex;flex-direction:column;gap:4px;align-items:flex-end">
            <span class="st ${st.cls}" style="font-size:9px;padding:2px 5px">${st.label}</span>
            <span class="badge badge-arr">${f.arrivalRunway}</span>
            <span class="badge badge-dep">${f.departureRunway}</span>
          </div>
        </div>`;
      }).join('');
      return `<div class="garage-card" style="animation-delay:${ci*.08}s">
        <div class="garage-header">
          <div class="garage-id">${gid}</div>
          <div class="occ-wrap">
            <span class="occ-label">${(occ*100).toFixed(0)}%</span>
            <div class="occ-bar"><div class="occ-fill" style="width:${occ*100}%"></div></div>
          </div>
        </div>
        <div>${rows}</div>
      </div>`;
    }).join('');
}

function renderRunways() {
  const now = simNow();
  document.getElementById('runways-grid').innerHTML =
    Object.entries(DATA.byRunway).sort().map(([rid, _], ci) => {
      const ops = DATA.global.filter(e => e.runway === rid).sort((a,b) => a.time.localeCompare(b.time));
      const arrCount = ops.filter(o => o.type==='ARR').length;
      const depCount = ops.filter(o => o.type==='DEP').length;
      const rows = ops.map(e => {
        const st = getStatus(e, now);
        return `<div class="rwy-row">
          <span class="mono" style="color:var(--amber)">${e.time}</span>
          <span class="badge ${e.type==='ARR'?'badge-arr':'badge-dep'}">${e.type}</span>
          <div>
            <div class="mono" style="color:var(--blue);font-size:12px">${e.flight.flightId}</div>
            <div class="dim" style="font-size:11px">${e.type==='DEP'?'→ '+e.flight.destination:'← '+e.flight.gateId}</div>
          </div>
          <span class="badge badge-rwy">${e.flight.gateId}</span>
          <span class="st ${st.cls}">${st.label}</span>
        </div>`;
      }).join('');
      return `<div class="runway-card" style="animation-delay:${ci*.08}s">
        <div class="runway-header">
          <div class="runway-id">${rid}</div>
          <div style="display:flex;gap:7px">
            <span class="badge badge-arr">${arrCount} ARR</span>
            <span class="badge badge-dep">${depCount} DEP</span>
          </div>
        </div>
        <div>${rows}</div>
      </div>`;
    }).join('');
}

function switchTab(name, el) {
  document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
  document.querySelectorAll('.view').forEach(v => v.classList.remove('active'));
  el.classList.add('active');
  document.getElementById('view-' + name).classList.add('active');
}

function updateClock() {
  const now = new Date();
  document.getElementById('clock').textContent =
    [now.getHours(),now.getMinutes(),now.getSeconds()].map(n=>String(n).padStart(2,'0')).join(':');
}

// Refresh toutes les 30s pour mettre à jour les statuts
function fullRefresh() {
  filterGlobal();
  renderGates();
  renderRunways();
}

renderStats();
renderGlobal(DATA.global);
renderGates();
renderRunways();
updateClock();
setInterval(updateClock, 1000);
setInterval(fullRefresh, 30000);
</script>
</body>
</html>"""
}