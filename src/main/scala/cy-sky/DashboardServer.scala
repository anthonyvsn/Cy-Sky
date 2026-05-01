package cysky

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import cysky.model.{EventType, InjectedEvent, UrgencyLevel}
import cysky.models.{Arrival, Departure, ScheduleGeneratorAlgorithm}
import cysky.petri.PetriScheduleVerifier
import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets
import java.time.{LocalTime}
import java.time.format.DateTimeFormatter
import java.util.UUID

/***
 * Serveur HTTP embarqué qui expose le dashboard de la simulation.
 *
 * Rôle :
 *   Lance un serveur HTTP sur le port 8080 et sert une page web permettant de configurer,
 *   démarrer et visualiser la simulation en temps réel.
 *
 * Routes exposées :
 *   GET  /               -> page HTML du dashboard
 *   POST /configure      -> génère l'emploi du temps avec les paramètres fournis
 *   POST /start          -> démarre la simulation
 *   GET  /schedule       -> retourne le planning et les événements au format JSON
 *   POST /add-event      -> ajoute un événement injecté (ex: atterrissage d'urgence)
 *   GET  /state/libre    -> état courant du mode Libre (JSON)
 *   GET  /state/controle -> état courant du mode Contrôle (JSON)
 *   POST /speed          -> change la vitesse de la simulation
 */
object DashboardServer {

  private val HHmm = DateTimeFormatter.ofPattern("HH:mm")

  /***
   * Démarre le serveur HTTP sur le port 8080 et enregistre toutes les routes.
   */
  def start(): Unit = {
    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    server.createContext("/",                 exchange => handle(exchange, routeRoot))
    server.createContext("/configure",        exchange => handle(exchange, routeConfigure))
    server.createContext("/start",            exchange => handle(exchange, routeStart))
    server.createContext("/schedule",         exchange => handle(exchange, routeSchedule))
    server.createContext("/add-event",        exchange => handle(exchange, routeAddEvent))
    server.createContext("/state/libre",      exchange => handle(exchange, routeStateLibre))
    server.createContext("/state/controle",   exchange => handle(exchange, routeStateControle))
    server.createContext("/speed",            exchange => handle(exchange, routeSpeed))
    server.setExecutor(null)
    server.start()
  }

  /***
   * Exécute une route et renvoie la réponse HTTP au client.
   *
   * Ajoute automatiquement les headers Content-Type et CORS sur chaque réponse.
   *
   * @param ex    l'échange HTTP entrant (requête + réponse).
   * @param route la fonction de traitement qui retourne (code HTTP, content-type, body).
   */
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

  private def routeRoot(ex: HttpExchange): (Int, String, String) =
    (200, "text/html; charset=UTF-8", htmlPage)


  /***
   * Déclenche le démarrage de la simulation.
   *
   * @param ex l'échange HTTP entrant.
   * @return 200 OK avec `{"ok":true}`.
   */
  private def routeStart(ex: HttpExchange): (Int, String, String) = {
    SimState.triggerStart()
    (200, "application/json", """{"ok":true}""")
  }

  private def routeSchedule(ex: HttpExchange): (Int, String, String) =
    (200, "application/json; charset=UTF-8", buildScheduleJson())

  /***
   * Enregistre un événement dynamique (ex: atterrissage d'urgence) dans [[SimState]].
   *
   * Paramètres attendus dans le body (form-urlencoded) :
   *   - `hour`    : heure cible de l'événement (0-23)
   *   - `minute`  : minute cible (0-59)
   *   - `urgency` : niveau d'urgence (Civil, Commercial, Military, Medical, Presidential, Emergency)
   *   - `note`    : description libre (optionnel)
   *
   * @param ex l'échange HTTP entrant.
   * @return 200 OK avec `{"ok":true}`, ou 405 si la méthode n'est pas POST.
   */
  private def routeAddEvent(ex: HttpExchange): (Int, String, String) = {
    if (ex.getRequestMethod != "POST") return (405, "text/plain", "Method Not Allowed")
    val raw    = new String(ex.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
    val params = parseForm(raw)

    val evtType = EventType.EmergencyArrival
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

  /***
   * Génère l'emploi du temps à partir des paramètres de configuration.
   *
   * Valide le planning via [[PetriScheduleVerifier]] avant de l'accepter.
   * Si le planning est invalide (deadlock détecté), retourne une erreur sans modifier [[SimState]].
   *
   * Paramètres attendus (form-urlencoded) :
   *   - `runways`   : nombre de pistes (1-10, défaut 3)
   *   - `garages`   : nombre de garages (1-20, défaut 6)
   *   - `planes`    : nombre d'avions max simultanés (5-50, défaut 15)
   *   - `seed`      : graine aléatoire pour la génération (défaut 42)
   *   - `startHour` / `startMin` : heure de début de la simulation
   *   - `endHour`   / `endMin`   : heure de fin de la simulation
   *
   * @param ex l'échange HTTP entrant.
   * @return JSON avec `{"ok":true,"flights":N,"runways":N,"garages":N}` si valide,
   *         ou `{"ok":false,"error":"..."}` si le planning est rejeté.
   */
  private def routeConfigure(ex: HttpExchange): (Int, String, String) = {
    if (ex.getRequestMethod != "POST") return (405, "text/plain", "Method Not Allowed")
    val raw    = new String(ex.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
    val params = parseForm(raw)

    def int(k: String, d: Int)  = params.get(k).flatMap(s => scala.util.Try(s.toInt).toOption).getOrElse(d)
    def long(k: String, d: Long) = params.get(k).flatMap(s => scala.util.Try(s.toLong).toOption).getOrElse(d)

    val runways  = int("runways",   3).max(1).min(10)
    val garages  = int("garages",   6).max(1).min(20)
    val planes   = int("planes",   15).max(5).min(50)
    val seed     = long("seed",    42L)
    val startH   = int("startHour", 6).max(0).min(23)
    val startM   = int("startMin",  0).max(0).min(59)
    val endH     = int("endHour",  23).max(0).min(23)
    val endM     = int("endMin",   59).max(0).min(59)

    try {
      val schedule = ScheduleGeneratorAlgorithm.generate(
        terminalId   = "T1",
        runwayCount  = runways,
        maxAirplanes = planes,
        seed         = seed,
        startTime    = LocalTime.of(startH, startM),
        endTime      = LocalTime.of(endH,   endM)
      )
      SimState.setSchedule(schedule)
      val flightCount = schedule.values.flatten.count(_.kind == Arrival)

      val verif = PetriScheduleVerifier.verify(schedule, runways, garages)
      println(verif.report)

      if (!verif.valid) {
        val msg = esc(verif.report.replaceAll("[\n\r]", " ").take(300))
        return (200, "application/json; charset=UTF-8",
          s"""{"ok":false,"error":"Planning invalide — essayez d'autres paramètres ou une autre graine","report":"$msg"}""")
      }

      SimState.setConfig(SimConfig(runways, garages, planes, seed, startH, startM, endH, endM))
      (200, "application/json; charset=UTF-8",
        s"""{"ok":true,"flights":$flightCount,"runways":$runways,"garages":$garages}""")

    } catch {
      case e: Exception =>
        val msg = esc(Option(e.getMessage).getOrElse("Erreur inconnue").take(200))
        (200, "application/json; charset=UTF-8",
          s"""{"ok":false,"error":"Erreur lors de la génération : $msg"}""")
    }
  }

  /***
   * Ajuste la vitesse de la simulation en changeant l'intervalle entre chaque tick.
   *
   * @param ex l'échange HTTP entrant. Paramètre attendu : `ms` (intervalle en millisecondes).
   * @return JSON `{"ok":true,"ms":N}` avec la nouvelle valeur appliquée.
   */
  private def routeSpeed(ex: HttpExchange): (Int, String, String) = {
    if (ex.getRequestMethod != "POST") return (405, "text/plain", "Method Not Allowed")
    val raw    = new String(ex.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
    val params = parseForm(raw)
    params.get("ms").flatMap(s => scala.util.Try(s.toLong).toOption).foreach(SimState.setTickInterval)
    (200, "application/json", s"""{"ok":true,"ms":${SimState.getTickInterval}}""")
  }

  private def routeStateLibre(ex: HttpExchange): (Int, String, String) =
    (200, "application/json; charset=UTF-8", buildStateJson(SimState.libre))

  private def routeStateControle(ex: HttpExchange): (Int, String, String) =
    (200, "application/json; charset=UTF-8", buildStateJson(SimState.controle))


  /***
   * Construit le JSON de l'emploi du temps complet (vols + événements injectés).
   *
   * @return JSON de la forme `{"flights":[...],"events":[...]}`.
   */
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

  /***
   * Sérialise un événement injecté en JSON.
   *
   * Calcule automatiquement le `triggerTime` (= targetTime − 30 min) pour affichage.
   *
   * @param e l'événement injecté à sérialiser.
   * @return JSON représentant l'événement.
   */
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

  /***
   * Construit le JSON de l'état courant d'une des 2 simulation (Libre ou Contrôle).
   *
   * Si la simulation n'a pas encore de snapshot, retourne un état vide avec des stats à zéro.
   *
   * @param slot le slot de simulation dont on veut l'état ([[SimState.libre]] ou [[SimState.controle]]).
   * @return JSON avec les stats, la liste des vols et l'heure simulée courante.
   */
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

  /***
   * Décode un body de requête au format `application/x-www-form-urlencoded`.
   *
   * @param body le contenu brut du body HTTP.
   * @return map clé -> valeur des paramètres décodés.
   */
  private def parseForm(body: String): Map[String, String] =
    body.split("&").flatMap { kv =>
      kv.split("=", 2) match {
        case Array(k, v) => Some(URLDecoder.decode(k, "UTF-8") -> URLDecoder.decode(v, "UTF-8"))
        case _           => None
      }
    }.toMap

  /***
   * Échappe les caractères spéciaux pour inclusion sûre dans une chaîne JSON.
   *
   * @param s la chaîne à échapper.
   * @return la chaîne avec `\` et `"` échappés.
   */
  private def esc(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"")


  /***
   * Convertit une chaîne en [[UrgencyLevel]] correspondant.
   *
   * @param s la valeur brute reçue depuis le formulaire HTML.
   * @return Some([[UrgencyLevel]]) si reconnu, None sinon.
   */
  private def parseUrgency(s: String): Option[UrgencyLevel] = s match {
    case "Civil"         => Some(UrgencyLevel.Civil)
    case "Commercial"    => Some(UrgencyLevel.Commercial)
    case "Military"      => Some(UrgencyLevel.Military)
    case "Medical"       => Some(UrgencyLevel.Medical)
    case "Presidential"  => Some(UrgencyLevel.Presidential)
    case "Emergency"     => Some(UrgencyLevel.Emergency)
    case _               => None
  }

  // Page HTML avec css inclus
  private val htmlPage: String = """<!DOCTYPE html>
<html lang="fr">
<head>
<meta charset="UTF-8">
<title>CySky - Simulation</title>
<style>
  body { font-family: Arial, sans-serif; margin: 20px; background: white; color: black; }
  h1 { color: darkblue; }
  h2 { color: darkblue; border-bottom: 2px solid darkblue; padding-bottom: 4px; }
  h3 { color: navy; margin-top: 16px; }
  table { border-collapse: collapse; width: 100%; margin-bottom: 16px; }
  th, td { border: 1px solid black; padding: 5px 10px; text-align: left; }
  th { background-color: #ccccee; }
  tr:nth-child(even) { background-color: #f0f0f0; }
  input, select { padding: 4px; border: 1px solid gray; }
  button { padding: 6px 14px; background: darkblue; color: white; border: none; cursor: pointer; margin: 4px 2px; }
  button:hover { background: navy; }
  button:disabled { background: gray; cursor: default; }
  .err { color: red; font-weight: bold; }
  .ok  { color: green; font-weight: bold; }
  .arr { background: lightblue; font-weight: bold; padding: 1px 5px; }
  .dep { background: lightgreen; font-weight: bold; padding: 1px 5px; }
  .evt { background: orange; font-weight: bold; padding: 1px 5px; }
  .delay { color: darkorange; font-weight: bold; }
  .header-bar { display: flex; justify-content: space-between; align-items: center;
                border-bottom: 2px solid darkblue; padding-bottom: 10px; margin-bottom: 16px; }
  .clock { font-size: 28px; font-weight: bold; color: darkblue; font-family: monospace; }
  #live-sim { display: none; }
  #pre-sim  { display: none; }
  .dual { display: flex; gap: 20px; }
  .dual > div { flex: 1; min-width: 0; overflow-x: auto; }
  .toast-area { position: fixed; top: 10px; right: 10px; width: 280px; z-index: 9999; }
  .toast { padding: 8px 12px; margin-bottom: 6px; border: 2px solid gray;
           background: lightyellow; font-size: 13px; }
  .toast.boom      { background: #ffdddd; border-color: red; color: red; font-weight: bold; }
  .toast.cancelled { background: #eeeeee; border-color: gray; color: #555; }
  .toast.delay     { background: #fff3cd; border-color: darkorange; }
  .status-actif    { color: darkblue; font-weight: bold; }
  .status-parti    { color: gray; }
  .status-annule   { color: #888; text-decoration: line-through; }
  .status-boom     { color: red; font-weight: bold; }
  .row-evt         { background: #fff3e0; }
</style>
</head>
<body>

<div class="header-bar">
  <div>
    <h1>CySky - Simulation Aeroportuaire</h1>
    <span id="live-label">En attente de demarrage...</span>
  </div>
  <div>
    <div class="clock" id="sim-clock">--:--</div>
    <div style="font-size:12px;color:gray">Heure simulee</div>
  </div>
</div>

<div style="margin-bottom:12px">
  Vitesse :
  <input type="range" id="speed-slider" min="0" max="200" step="1" value="100"
    oninput="onSpeedChange(this.value)" onchange="applySpeed(this.value)">
  <span id="speed-label">1.0x</span>
  &nbsp;&nbsp;<small>(Lent &lt;--- ---&gt; Rapide)</small>
</div>

<!-- ECRAN 1 : CONFIGURATION -->
<div id="config-screen">
  <h2>Etape 1 : Configuration de la simulation</h2>
  <form onsubmit="configure(); return false;">
    <table style="width:auto">
      <tr>
        <td><b>Nombre de pistes :</b></td>
        <td><input type="number" id="cfg-runways" min="1" max="10" value="3" style="width:70px"></td>
      </tr>
      <tr>
        <td><b>Nombre de garages :</b></td>
        <td><input type="number" id="cfg-garages" min="1" max="20" value="6" style="width:70px"></td>
      </tr>
      <tr>
        <td><b>Avions simultanes max :</b></td>
        <td><input type="number" id="cfg-planes" min="5" max="50" value="15" style="width:70px"></td>
      </tr>
      <tr>
        <td><b>Heure de debut :</b></td>
        <td>
          <input type="number" id="cfg-start-h" min="0" max="23" value="6" style="width:50px"> h
          <input type="number" id="cfg-start-m" min="0" max="59" value="0" style="width:50px"> min
        </td>
      </tr>
      <tr>
        <td><b>Heure de fin :</b></td>
        <td>
          <input type="number" id="cfg-end-h" min="0" max="23" value="23" style="width:50px"> h
          <input type="number" id="cfg-end-m" min="0" max="59" value="59" style="width:50px"> min
        </td>
      </tr>
      <tr>
        <td><b>Graine aleatoire :</b></td>
        <td>
          <input type="number" id="cfg-seed" value="42" style="width:100px">
          <button type="button" onclick="randomSeed()">Aleatoire</button>
        </td>
      </tr>
    </table>
    <div id="config-result"></div>
    <button type="submit" id="btn-configure">Generer l'emploi du temps</button>
  </form>
</div>

<!-- ECRAN 2 : PRE-SIMULATION -->
<div id="pre-sim">
  <h2>Etape 2 : Emploi du temps et evenements</h2>

  <h3>Ajouter un atterrissage d'urgence</h3>
  <table style="width:auto">
    <tr>
      <td><b>Heure cible :</b></td>
      <td>
        <input type="number" id="evt-hour" min="0" max="23" value="12" style="width:50px"> h
        <input type="number" id="evt-min"  min="0" max="59" value="0"  style="width:50px"> min
      </td>
    </tr>
    <tr>
      <td><b>Urgence :</b></td>
      <td>
        <select id="evt-urgency">
          <option value="Commercial">Commercial</option>
          <option value="Civil">Civil</option>
          <option value="Military">Military</option>
          <option value="Medical">Medical</option>
          <option value="Presidential">Presidential</option>
          <option value="Emergency">Emergency</option>
        </select>
      </td>
    </tr>
    <tr>
      <td><b>Note :</b></td>
      <td><input type="text" id="evt-note" style="width:250px" placeholder="Description (optionnel)"></td>
    </tr>
  </table>
  <button onclick="addEvent()">+ Ajouter l'evenement</button>

  <h3>Evenements injectes</h3>
  <div id="events-list">Aucun evenement ajoute.</div>

  <h3>Emploi du temps genere (valide - aucun deadlock)</h3>
  <table>
    <thead>
      <tr>
        <th>Heure</th><th>Type</th><th>Vol</th><th>Avion</th>
        <th>Piste</th><th>Destination</th><th>Info</th>
      </tr>
    </thead>
    <tbody id="schedule-body">
      <tr><td colspan="7">Chargement...</td></tr>
    </tbody>
  </table>

  <button id="btn-start" onclick="startSim()" style="font-size:16px;padding:10px 30px;background:green">
    Lancer les deux simulations
  </button>
</div>

<!-- ECRAN 3 : SIMULATION LIVE -->
<div id="live-sim">
  <h2>Simulation en cours</h2>
  <div class="dual">

    <!-- Mode Libre -->
    <div>
      <h3 style="color:green">Mode Libre (sans verification Petri)</h3>
      <p>
        Vols total : <b id="stat-libre-total">0</b> |
        En cours : <b id="stat-libre-active">0</b> |
        Partis : <b id="stat-libre-departed">0</b> |
        Pistes libres : <b id="stat-libre-runways">0</b> |
        Gates libres : <b id="stat-libre-garages">0</b>
      </p>
      <table>
        <thead>
          <tr><th>#</th><th>Heure</th><th>Type</th><th>Vol</th><th>Avion</th><th>Piste</th><th>Destination</th><th>Etat</th></tr>
        </thead>
        <tbody id="live-body-libre"></tbody>
      </table>
    </div>

    <!-- Mode Controle -->
    <div>
      <h3 style="color:navy">Mode Controle (verification Petri active)</h3>
      <p>
        Vols total : <b id="stat-controle-total">0</b> |
        En cours : <b id="stat-controle-active">0</b> |
        Partis : <b id="stat-controle-departed">0</b> |
        Pistes libres : <b id="stat-controle-runways">0</b> |
        Gates libres : <b id="stat-controle-garages">0</b>
      </p>
      <table>
        <thead>
          <tr><th>#</th><th>Heure</th><th>Type</th><th>Vol</th><th>Avion</th><th>Piste</th><th>Destination</th><th>Etat</th><th>Retard</th></tr>
        </thead>
        <tbody id="live-body-controle"></tbody>
      </table>
    </div>

  </div>
</div>

<!-- Zone de notifications -->
<div class="toast-area" id="toast-area"></div>

<script>
var scheduleData = {flights:[], events:[]};
var currentTickMs = 112;

function randomSeed() {
  document.getElementById('cfg-seed').value = Math.floor(Math.random() * 999999);
}

function configure() {
  var btn = document.getElementById('btn-configure');
  btn.disabled = true;
  btn.textContent = 'Generation en cours...';
  document.getElementById('config-result').innerHTML = '';

  var body = [
    'runways='   + encodeURIComponent(document.getElementById('cfg-runways').value),
    'garages='   + encodeURIComponent(document.getElementById('cfg-garages').value),
    'planes='    + encodeURIComponent(document.getElementById('cfg-planes').value),
    'seed='      + encodeURIComponent(document.getElementById('cfg-seed').value),
    'startHour=' + encodeURIComponent(document.getElementById('cfg-start-h').value),
    'startMin='  + encodeURIComponent(document.getElementById('cfg-start-m').value),
    'endHour='   + encodeURIComponent(document.getElementById('cfg-end-h').value),
    'endMin='    + encodeURIComponent(document.getElementById('cfg-end-m').value)
  ].join('&');

  fetch('/configure', {method:'POST', headers:{'Content-Type':'application/x-www-form-urlencoded'}, body: body})
    .then(function(r){ return r.json(); })
    .then(function(data) {
      if (data.ok) {
        document.getElementById('config-result').innerHTML =
          '<p class="ok">OK ! Planning genere : ' + data.flights + ' vol(s), ' +
          data.runways + ' piste(s), ' + data.garages + ' garage(s)</p>';
        setTimeout(function() {
          document.getElementById('config-screen').style.display = 'none';
          document.getElementById('pre-sim').style.display = 'block';
          loadSchedule();
        }, 800);
      } else {
        document.getElementById('config-result').innerHTML =
          '<p class="err">Erreur : ' + (data.error || 'Erreur inconnue') + '</p>';
        btn.disabled = false;
        btn.textContent = "Generer l'emploi du temps";
      }
    })
    .catch(function() {
      document.getElementById('config-result').innerHTML =
        '<p class="err">Erreur de connexion au serveur</p>';
      btn.disabled = false;
      btn.textContent = "Generer l'emploi du temps";
    });
}

function sliderToMs(val) {
  return Math.round(112 * Math.pow(2, (100 - parseInt(val)) / 100));
}

function onSpeedChange(val) {
  document.getElementById('speed-label').textContent = (112 / sliderToMs(val)).toFixed(1) + 'x';
}

function applySpeed(val) {
  var ms = sliderToMs(val);
  currentTickMs = ms;
  document.getElementById('speed-label').textContent = (112 / ms).toFixed(1) + 'x';
  fetch('/speed', {method:'POST', headers:{'Content-Type':'application/x-www-form-urlencoded'}, body: 'ms=' + ms});
}

function loadSchedule() {
  fetch('/schedule')
    .then(function(r){ return r.json(); })
    .then(function(data){
      scheduleData = data;
      renderSchedule();
      renderEventList();
    });
}

function renderSchedule() {
  var rows = [];
  scheduleData.flights.forEach(function(f) {
    rows.push({time: f.time, kind: 'flight', data: f});
  });
  scheduleData.events.forEach(function(e) {
    rows.push({time: e.triggerTime, kind: 'event', data: e});
  });
  rows.sort(function(a, b){ return a.time.localeCompare(b.time); });

  var tbody = document.getElementById('schedule-body');
  if (rows.length === 0) {
    tbody.innerHTML = '<tr><td colspan="7">Aucun vol planifie</td></tr>';
    return;
  }
  tbody.innerHTML = rows.map(function(row) {
    if (row.kind === 'flight') {
      var f = row.data;
      var badge = f.kind === 'ARR' ? '<span class="arr">ARR</span>' : '<span class="dep">DEP</span>';
      return '<tr>' +
        '<td>' + f.time + '</td><td>' + badge + '</td>' +
        '<td>' + f.flightId + '</td><td>' + f.airplaneId + '</td>' +
        '<td>' + f.runway + '</td>' +
        '<td>' + (f.kind === 'DEP' ? f.destination : '-') + '</td>' +
        '<td></td></tr>';
    } else {
      var e = row.data;
      return '<tr class="row-evt">' +
        '<td>' + e.triggerTime + '</td><td><span class="evt">EVT</span></td>' +
        '<td colspan="3">' + e.typeLabel + (e.note ? ' - ' + e.note : '') + '</td>' +
        '<td>' + (e.urgency || '') + '</td>' +
        '<td>cible : ' + e.targetTime + '</td></tr>';
    }
  }).join('');
}

function renderEventList() {
  var el = document.getElementById('events-list');
  if (!scheduleData.events || scheduleData.events.length === 0) {
    el.innerHTML = 'Aucun evenement ajoute.';
    return;
  }
  el.innerHTML = '<ul>' + scheduleData.events.map(function(e) {
    return '<li>' + e.typeLabel + ' a ' + e.triggerTime +
           ' (cible : ' + e.targetTime + ') - urgence : ' + e.urgency +
           (e.note ? ' - ' + e.note : '') + '</li>';
  }).join('') + '</ul>';
}

function addEvent() {
  var body = 'type=EmergencyArrival' +
             '&hour='    + encodeURIComponent(document.getElementById('evt-hour').value) +
             '&minute='  + encodeURIComponent(document.getElementById('evt-min').value) +
             '&urgency=' + encodeURIComponent(document.getElementById('evt-urgency').value) +
             '&note='    + encodeURIComponent(document.getElementById('evt-note').value);

  fetch('/add-event', {method:'POST', headers:{'Content-Type':'application/x-www-form-urlencoded'}, body: body})
    .then(function(){ return fetch('/schedule'); })
    .then(function(r){ return r.json(); })
    .then(function(data){
      scheduleData = data;
      renderSchedule();
      renderEventList();
      document.getElementById('evt-note').value = '';
    });
}

function startSim() {
  var btn = document.getElementById('btn-start');
  btn.disabled = true;
  btn.textContent = 'Demarrage...';
  fetch('/start', {method:'POST'}).then(function(){
    document.getElementById('pre-sim').style.display  = 'none';
    document.getElementById('live-sim').style.display = 'block';
    document.getElementById('live-label').textContent = 'Simulation en cours...';
    setTimeout(pollState, 300);
  });
}

var lastBoomLibre    = 0;
var lastBoomControle = 0;
var seenDelays    = {};
var seenCancelled = {};

function showToast(type, title, body, side) {
  var area = document.getElementById('toast-area');
  var t = document.createElement('div');
  t.className = 'toast ' + type;
  t.innerHTML = '<b>' + title + '</b> (' + (side === 'libre' ? 'Mode Libre' : 'Mode Controle') + ')<br>' + body;
  area.appendChild(t);
  setTimeout(function() { if (t.parentNode) t.parentNode.removeChild(t); }, 5000);
}

function statusText(s) {
  if (!s || s === 'Planifie' || s === 'Planifié') return '<span style="color:gray">Planifie</span>';
  if (s === 'BOOM')                                      return '<span class="status-boom">BOOM !</span>';
  if (s === 'Annule' || s === 'Annulé')            return '<span class="status-annule">Annule</span>';
  if (s === 'Parti')                                     return '<span class="status-parti">Parti</span>';
  return '<span class="status-actif">' + s + '</span>';
}

function updatePanel(side, s) {
  document.getElementById('stat-' + side + '-total').textContent    = s.stats.total;
  document.getElementById('stat-' + side + '-active').textContent   = s.stats.active;
  document.getElementById('stat-' + side + '-departed').textContent = s.stats.departed;
  document.getElementById('stat-' + side + '-runways').textContent  = s.stats.freeRunways;
  document.getElementById('stat-' + side + '-garages').textContent  = s.stats.freeGarages;

  var showDelay = (side === 'controle');
  var rows = s.flights.map(function(f, i) {
    var badge = f.kind === 'ARR' ? '<span class="arr">ARR</span>' : '<span class="dep">DEP</span>';
    var delayCell = showDelay
      ? '<td>' + (f.delay > 0 ? '<span class="delay">+' + f.delay + ' min</span>' : '') + '</td>'
      : '';
    return '<tr><td>' + (i+1) + '</td><td>' + f.time + '</td><td>' + badge + '</td>' +
      '<td>' + f.flightId + '</td><td>' + f.airplaneId + '</td>' +
      '<td>' + f.runway + '</td><td>' + f.destination + '</td>' +
      '<td>' + statusText(f.status) + '</td>' + delayCell + '</tr>';
  });
  document.getElementById('live-body-' + side).innerHTML = rows.join('');

  s.flights.forEach(function(f) {
    var dk = side + ':' + f.flightId + ':delay';
    var ck = side + ':' + f.flightId + ':cancel';
    if (f.delay > 0 && !seenDelays[dk]) {
      seenDelays[dk] = true;
      showToast('delay', 'Vol retarde', f.flightId + ' +' + f.delay + ' min', side);
    }
    if ((f.status === 'Annule' || f.status === 'Annulé') && !seenCancelled[ck]) {
      seenCancelled[ck] = true;
      showToast('cancelled', 'Vol annule', f.flightId + ' - ' + f.destination, side);
    }
  });

  if (side === 'libre' && s.boomVersion > lastBoomLibre) {
    lastBoomLibre = s.boomVersion;
    showToast('boom', 'COLLISION DETECTEE !', s.boomMessage, 'libre');
  }
  if (side === 'controle' && s.boomVersion > lastBoomControle) {
    lastBoomControle = s.boomVersion;
    showToast('boom', 'COLLISION DETECTEE !', s.boomMessage, 'controle');
  }
}

function pollState() {
  Promise.all([
    fetch('/state/libre').then(function(r){ return r.json(); }),
    fetch('/state/controle').then(function(r){ return r.json(); })
  ]).then(function(results) {
    var libre    = results[0];
    var controle = results[1];

    var clk = libre.simTime !== '--:--' ? libre.simTime : controle.simTime;
    document.getElementById('sim-clock').textContent = clk;

    updatePanel('libre',    libre);
    updatePanel('controle', controle);

    if (libre.finished && controle.finished) {
      document.getElementById('live-label').textContent = 'Simulations terminees !';
    } else {
      setTimeout(pollState, Math.max(50, Math.min(currentTickMs, 1000)));
    }
  }).catch(function(){ setTimeout(pollState, Math.max(50, Math.min(currentTickMs, 1000))); });
}

loadSchedule();
</script>
</body>
</html>"""
}
