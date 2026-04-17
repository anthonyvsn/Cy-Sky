package cysky

import cysky.actors.TowerControlActor.TowerData
import cysky.model.InjectedEvent
import cysky.models.AircraftFlight
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.CountDownLatch

// ═══════════════════════════════════════════════════════════════
// SimSlot — état isolé pour une simulation (libre ou contrôle).
//
// Chaque simulation a son propre TowerData, son propre état
// finished et ses propres alertes BOOM.
// ═══════════════════════════════════════════════════════════════
class SimSlot(val name: String) {

  private val ref          = new AtomicReference[Option[TowerData]](None)
  private val _finished    = new AtomicReference[Boolean](false)
  private val _boomVersion = new AtomicInteger(0)
  private val _boomMessage = new AtomicReference[String]("")

  def update(data: TowerData): Unit = ref.set(Some(data))
  def snapshot: Option[TowerData]   = ref.get()

  def markFinished(): Unit = _finished.set(true)
  def isFinished: Boolean  = _finished.get()

  def addBoom(message: String): Unit = {
    _boomMessage.set(message)
    _boomVersion.incrementAndGet()
  }
  def boomVersion: Int    = _boomVersion.get()
  def boomMessage: String = _boomMessage.get()
}

// ═══════════════════════════════════════════════════════════════
// SimState — état partagé entre les deux simulations et le
// serveur HTTP.
//
// Partagé : startLatch, isStarted, injectedEvents, scheduleRef
// Par simulation : SimSlot (libre / controle)
// ═══════════════════════════════════════════════════════════════
object SimState {

  // ── Slots par simulation ──────────────────────────────────────
  val libre    = new SimSlot("libre")
  val controle = new SimSlot("controle")

  // ── État partagé ─────────────────────────────────────────────
  private val _started       = new AtomicReference[Boolean](false)
  private val injectedEvents = new AtomicReference[List[InjectedEvent]](List.empty)
  private val scheduleRef    = new AtomicReference[Map[String, List[AircraftFlight]]](Map.empty)

  /** Décompté à 0 quand l'utilisateur clique Start */
  val startLatch = new CountDownLatch(1)

  def triggerStart(): Unit = { _started.set(true); startLatch.countDown() }
  def isStarted: Boolean   = _started.get()

  def updateEventStatus(id: String, status: String): Unit = {
    val updated = injectedEvents.get().map { e =>
      if (e.id == id) e.copy(status = status) else e
    }
    injectedEvents.set(updated)
  }

  def addInjectedEvent(e: InjectedEvent): Unit =
    injectedEvents.getAndUpdate(list => e :: list)

  def injectedEventsSnapshot: List[InjectedEvent] = injectedEvents.get()

  def setSchedule(s: Map[String, List[AircraftFlight]]): Unit = scheduleRef.set(s)
  def scheduleSnapshot: Map[String, List[AircraftFlight]]     = scheduleRef.get()
}
