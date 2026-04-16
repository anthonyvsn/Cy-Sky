package cysky

import cysky.actors.TowerControlActor.TowerData
import cysky.model.InjectedEvent
import cysky.models.AircraftFlight
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.CountDownLatch

// ═══════════════════════════════════════════════════════════════
// SimState — état partagé thread-safe entre le serveur HTTP
// (thread Java) et le système d'acteurs (thread pool Akka).
//
// AtomicReference garantit la visibilité sans synchronized.
// CountDownLatch permet au Main de bloquer jusqu'au clic Start.
// ═══════════════════════════════════════════════════════════════
object SimState {

  private val ref            = new AtomicReference[Option[TowerData]](None)
  private val _started       = new AtomicReference[Boolean](false)
  private val _finished      = new AtomicReference[Boolean](false)
  private val injectedEvents = new AtomicReference[List[InjectedEvent]](List.empty)
  private val scheduleRef    = new AtomicReference[Map[String, List[AircraftFlight]]](Map.empty)

  // ── Alertes BOOM ─────────────────────────────────────────────
  private val _boomVersion = new AtomicInteger(0)
  private val _boomMessage = new AtomicReference[String]("")

  /** Enregistre un nouvel événement BOOM. Thread-safe. */
  def addBoom(message: String): Unit = {
    _boomMessage.set(message)
    _boomVersion.incrementAndGet()
  }

  def boomVersion: Int    = _boomVersion.get()
  def boomMessage: String = _boomMessage.get()

  /** Décompté à 0 quand l'utilisateur clique Start */
  val startLatch = new CountDownLatch(1)

  def update(data: TowerData): Unit = ref.set(Some(data))
  def snapshot: Option[TowerData]   = ref.get()

  def triggerStart(): Unit = { _started.set(true); startLatch.countDown() }
  def isStarted: Boolean   = _started.get()

  def markFinished(): Unit = _finished.set(true)
  def isFinished: Boolean  = _finished.get()

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
