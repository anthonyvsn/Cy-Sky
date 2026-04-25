package cysky

import org.scalatest.funsuite.AnyFunSuite

import akka.Done
import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.actors.{TowerControlActor, ClockActor}
import cysky.models.ScheduleGeneratorAlgorithm
import java.time.{LocalDate, LocalTime}
import scala.concurrent.Await
import scala.concurrent.duration._
import cysky.petri._
import cysky.petri.PetriVisualizer
import cysky.petri.PetriModule
import cysky.petri.PetriComposer._

class Test extends AnyFunSuite {

  // === PARAMÈTRES ===
  val N = 3            // 3 taxiways, 3 pistes
  val nAvions = 5
  val nMaxSystem = 10
  val TICK_INTERVAL: FiniteDuration     = 112.millis
  val SIM_STEP:      java.time.Duration = java.time.Duration.ofMinutes(1)

  // ── TEST PETRI (AVANT SIMULATION) ─────────────────────────────
  test("TEST PETRI") {
  println("\n══════════ TEST PETRI ══════════")

  val petri = PetriModule(
    places = Vector("p1", "p2"),
    transitions = Vector("t1"),
    pre = Vector(
      Vector(1),
      Vector(0)
    ),
    post = Vector(
      Vector(0),
      Vector(1)
    ),
    marking = Vector(1, 0)
  )

  // Visualisation
  PetriVisualizer.saveDot(petri, "test.dot", "Test Petri")
  PetriVisualizer.saveHtml(petri, "test.html", "Test Petri")

  // Analyse complète
  PetriVerifier.analyzeAndReport(petri, "Test Petri")

  // LTL
  val noDeadlock = LTLChecker.noDeadlock(petri)
  val ltlOK = LTLChecker.check(petri, noDeadlock)

  println(s"LTL noDeadlock : $ltlOK")
  println("════════════════════════════════\n")

  assert(!ltlOK)

  }

  // === MODULE GARAGE (T1-T4, Cargo, Maintenance) ===
  val garage = PetriModule(
    places      = Vector("BufferGarage", "Garage", "G_open", "G_close"),
    transitions = Vector("lockGarage", "unlockGarage"),
    pre  = Vector(
      Vector(0, 0),   // BufferGarage
      Vector(1, 0),   // Garage         → lockGarage consomme 1 avion
      Vector(1, 0),   // G_open         → lockGarage consomme le verrou ouvert
      Vector(0, 1)    // G_close        → unlockGarage consomme le verrou fermé
    ),
    post = Vector(
      Vector(0, 1),   // BufferGarage   → unlockGarage libère une place buffer
      Vector(0, 1),   // Garage         → unlockGarage remet l'avion dans Garage
      Vector(0, 1),   // G_open         → unlockGarage rouvre le verrou
      Vector(1, 0)    // G_close        → lockGarage ferme le verrou
    ),
    marking = Vector(nMaxSystem - nAvions, nAvions, 1, 0)
  )

  // === MODULE TAXIWAY (base) ===
  val taxiWayBase = PetriModule(
    places = Vector(
      "BufferTaxiWay",
      "TaxiWay",
      "TW_open",
      "TW_close",
      "Cap_TaxiWay"
    ),

    transitions = Vector(
      "addTaxiWay",
      "lockTaxiWay",
      "unlockTaxiWay"
    ),

    // PRE : chaque ligne = une place, chaque colonne = une transition
    pre = Vector(
      // addTaxiWay, lockTaxiWay, unlockTaxiWay
      Vector(1, 0, 0),  // BufferTaxiWay - addTaxiWay consomme 1 du buffer
      Vector(0, 1, 1),  // TaxiWay - lock et unlock consomment l'avion
      Vector(0, 1, 0),  // TW_open - lock vérifie ouvert
      Vector(0, 0, 1),  // TW_close - unlock vérifie fermé
      Vector(1, 0, 0)   // Cap_TaxiWay - addTaxiWay consomme la capacité
    ),

    // POST
    post = Vector(
      // addTaxiWay, lockTaxiWay, unlockTaxiWay
      Vector(0, 0, 0),  // BufferTaxiWay - capacité consommée
      Vector(1, 1, 1),  // TaxiWay - add produit, lock/unlock maintiennent
      Vector(0, 0, 1),  // TW_open - unlock rouvre
      Vector(0, 1, 0),  // TW_close - lock ferme
      Vector(0, 0, 1)   // Cap_TaxiWay - unlock RESTAURE la capacité ← FIX ICI
    ),

    marking = Vector(
      nMaxSystem - nAvions,  // BufferTaxiWay
      0,                     // TaxiWay
      1,                     // TW_open
      0,                     // TW_close
      1                      // Cap_TaxiWay = 1 capacité disponible
    )
  )

  // === MODULE TRACK (base) ===
  val trackBase = PetriModule(
    places      = Vector("BufferTrack", "Track", "TR_open", "TR_close"),
    transitions = Vector("landing", "lockTrack", "unlockTrack", "takeoff", "addOnTrack"),
    pre  = Vector(
      Vector(0, 0, 1, 0, 0),  // BufferTrack
      Vector(0, 1, 1, 1, 0),  // Track
      Vector(1, 1, 0, 1, 1),  // TR_open
      Vector(0, 0, 1, 0, 0)   // TR_close
    ),
    post = Vector(
      Vector(0, 0, 1, 1, 0),  // BufferTrack
      Vector(1, 1, 1, 0, 1),  // Track
      Vector(1, 0, 1, 1, 1),  // TR_open
      Vector(0, 1, 0, 0, 0)   // TR_close
    ),
    marking = Vector(nMaxSystem - nAvions, 0, 1, 0)
  )

  // === RÉPLICATION & ASSEMBLAGE DIAGONAL ===
  val allTaxiWays = replicateModule(taxiWayBase, N)
  val allTracks   = replicateModule(trackBase, N)
  val base        = blockDiag(garage, blockDiag(allTaxiWays, allTracks))

  // === LIEN GARAGE → TAXIWAY ===
  val garageToTwPairs = (1 to N).map(j => j)

  val withGarageTw = garageToTwPairs.foldLeft(base) { (sys, j) =>
    val s1 = addLinkTransition(sys, s"garage_to_tw$j", "Garage", s"TaxiWay_$j")
    val s2 = addArc(s1, s"BufferTaxiWay_$j", s"garage_to_tw$j", 1, 0)
    val s3 = addArc(s2, "BufferGarage",      s"garage_to_tw$j", 0, 1)
    val s4 = addArc(s3, "G_open",            s"garage_to_tw$j", 1, 1)
    val s5 = addArc(s4, s"TW_open_$j",       s"garage_to_tw$j", 1, 1)
    // AJOUTER : consommer Cap_TaxiWay_j
    addArc(s5, s"Cap_TaxiWay_$j",            s"garage_to_tw$j", 1, 0)
  }

  // === LIEN TW → TRACK (N×N = 9 transitions) ===
  val twToTrPairs = for (i <- 1 to N; j <- 1 to N) yield (i, j)

  val withTwTr = twToTrPairs.foldLeft(withGarageTw) { case (sys, (i, j)) =>
    val s1 = addLinkTransition(sys, s"tw${i}_to_tr${j}", s"TaxiWay_$i", s"Track_$j")
    val s2 = addArc(s1, s"BufferTrack_$j",   s"tw${i}_to_tr${j}", 1, 0)
    val s3 = addArc(s2, s"BufferTaxiWay_$i", s"tw${i}_to_tr${j}", 0, 1)
    val s4 = addArc(s3, s"TW_open_$i",       s"tw${i}_to_tr${j}", 1, 1)
    val s5 = addArc(s4, s"TR_open_$j",       s"tw${i}_to_tr${j}", 1, 1)
    // AJOUTER : restaurer Cap_TaxiWay_i quand on quitte le taxiway
    addArc(s5, s"Cap_TaxiWay_$i",            s"tw${i}_to_tr${j}", 0, 1)
  }

  // === REDIRECTIONS TW ↔ TW ===
  val redirectPairs = for (i <- 1 to N; j <- 1 to N; if i != j) yield (i, j)

  val withRedirects = redirectPairs.foldLeft(withTwTr) { case (sys, (i, j)) =>
    val s1 = addLinkTransition(sys, s"redirect_tw${i}_to_tw${j}",
      s"TaxiWay_$i", s"TaxiWay_$j")
    // Vérifier que TW_i est ouvert
    val s2 = addArc(s1, s"TW_open_$i", s"redirect_tw${i}_to_tw${j}", 1, 1)
    // Vérifier que TW_j est ouvert
    val s3 = addArc(s2, s"TW_open_$j", s"redirect_tw${i}_to_tw${j}", 1, 1)
    // AJOUTER : restaurer Cap_TaxiWay_i (on quitte)
    val s4 = addArc(s3, s"Cap_TaxiWay_$i", s"redirect_tw${i}_to_tw${j}", 0, 1)
    // AJOUTER : consommer Cap_TaxiWay_j (on arrive)
    addArc(s4, s"Cap_TaxiWay_$j", s"redirect_tw${i}_to_tw${j}", 1, 0)
  }

  // === countPlanes GLOBAL ===
  val withCount = addPlace(
    addPlace(withRedirects, "countPlanes", nAvions),
    "BufferCountPlanes", nMaxSystem - nAvions
  )

  val system = (1 to N).foldLeft(withCount) { (sys, i) =>
    val s1 = addArc(sys, "countPlanes",       s"takeoff_$i",     1, 0)
    val s2 = addArc(s1,  "BufferCountPlanes", s"takeoff_$i",     0, 1)
    val s3 = addArc(s2,  "BufferCountPlanes", s"landing_$i",     1, 0)
    val s4 = addArc(s3,  "countPlanes",       s"landing_$i",     0, 1)
    val s5 = addArc(s4,  "BufferCountPlanes", s"addOnTrack_$i",  1, 0)
    addArc(s5,           "countPlanes",       s"addOnTrack_$i",  0, 1)
  }

  // === AFFICHAGE ===
  println(s"=== PLACES (${system.places.length}) ===")
  system.places.zipWithIndex.foreach { case (p, i) =>
    println(f"  P$i%2d : $p  [M0 = ${system.marking(i)}]")
  }

  println(s"\n=== TRANSITIONS (${system.transitions.length}) ===")
  system.transitions.zipWithIndex.foreach { case (t, j) =>
    println(f"  T$j%2d : $t")
  }

  val header = "".padTo(25, ' ') +
    system.transitions.indices.map(j => "%4s".format(s"T$j")).mkString

  println(s"\n=== MATRICE PRE ===")
  println(header)
  system.pre.zipWithIndex.foreach { case (row, i) =>
    val label = "P%-2d %-20s".format(i, system.places(i))
    println(label + row.map(v => "%4d".format(v)).mkString)
  }

  println(s"\n=== MATRICE POST ===")
  println(header)
  system.post.zipWithIndex.foreach { case (row, i) =>
    val label = "P%-2d %-20s".format(i, system.places(i))
    println(label + row.map(v => "%4d".format(v)).mkString)
  }

  println(s"\n=== MATRICE C = POST - PRE ===")
  println(header)
  system.pre.zip(system.post).zipWithIndex.foreach { case ((preRow, postRow), i) =>
    val label = "P%-2d %-20s".format(i, system.places(i))
    val incRow = preRow.zip(postRow).map { case (a, b) => b - a }
    println(label + incRow.map(v => "%4d".format(v)).mkString)
  }

  def verify(m: PetriModule): (Boolean, List[String]) = {
  val errors = scala.collection.mutable.ListBuffer[String]()

  m.marking.zipWithIndex.foreach { case (tokens, i) =>
    if (tokens < 0)
      errors += s"Jeton négatif : ${m.places(i)} = $tokens"
  }

  val iCount  = m.places.indexOf("countPlanes")
  val iBuf    = m.places.indexOf("BufferCountPlanes")

  if (iCount >= 0 && iBuf >= 0) {
    val total = m.marking(iCount) + m.marking(iBuf)
    if (total != nMaxSystem)
      errors += s"Conservation violée : $total ≠ $nMaxSystem"
  }

  def checkLock(openName: String, closeName: String): Unit = {
    val iO = m.places.indexOf(openName)
    val iC = m.places.indexOf(closeName)
    if (iO >= 0 && iC >= 0) {
      val sum = m.marking(iO) + m.marking(iC)
      if (sum != 1)
        errors += s"Verrou incohérent : $openName + $closeName = $sum"
    }
  }

  checkLock("G_open", "G_close")
  (1 to N).foreach { i =>
    checkLock(s"TW_open_$i", s"TW_close_$i")
    checkLock(s"TR_open_$i", s"TR_close_$i")
  }

  val ok = errors.isEmpty
  (ok, errors.toList)
}

  test("TEST VÉRIFICATION MARQUAGE INITIAL") {
  val (ok, errs) = verify(system)

  errs.foreach(e => println(s"[ERREUR] $e"))

  assert(ok)
}

  test("TEST ALÉATOIRE") {

  def randomTest(initial: PetriModule, steps: Int = 1000): Boolean = {
    import cysky.petri.PetriComposer.{enabledTransitions, fireTransition}
    val rng     = new scala.util.Random(42)
    var state   = initial
    var fired   = 0
    var deadlockCount = 0
    var errorCount    = 0

    println(s"\n=== TEST ALÉATOIRE ($steps transitions) ===")

    for (step <- 1 to steps) {
      val enabled = enabledTransitions(state)
      if (enabled.isEmpty) {
        println(s"  [DEADLOCK] Étape $step — aucune transition franchissable, réinitialisation")
        deadlockCount += 1
        state = initial
      } else {
        val tIdx  = enabled(rng.nextInt(enabled.length))
        val tName = state.transitions(tIdx)
        state = fireTransition(state, tIdx)
        fired += 1

        val (ok, errs) = verify(state)
        
        if (!ok) {
          println(s"  [ERREUR] Étape $step après T$tIdx ($tName) :")
          errs.foreach(e => println(s"           • $e"))
          errorCount += 1
        }
        
      }
    }

    println(s"  Transitions franchies    : $fired")
    println(s"  Deadlocks rencontrés     : $deadlockCount")
    println(s"  Violations d'invariants  : $errorCount")
    if (errorCount == 0 && deadlockCount == 0) {
      println(s"  => Test REUSSI : aucune erreur sur $steps transitions");
      true;
    }
    else {
      println(s"  => Test ECHOUE");
      false;
    }
    
  }

  val ok = randomTest(system)
  assert(ok)

  }

  test("TEST LTL SUR SYSTÈME AÉRIEN") {

  println("\n=== TEST LTL SUR SYSTÈME AÉRIEN ===")

  (1 to N).foreach { i =>
    val name = s"TaxiWay_$i"
    val idx = system.places.indexOf(name)

    if (idx == -1) {
      println(s"$name capacity SKIPPED (not found)")
    } else {
      val formula = LTLChecker.noOverflow(idx, 1)
      println(s"$name capacity OK : " + LTLChecker.check(system, formula))
      assert(LTLChecker.check(system, formula))
    }
  }

  val req = system.places.indexOf("BufferTrack_1")
  val landed = system.places.indexOf("Track_1")

  if (req != -1 && landed != -1) {
    val serviceOK = LTLChecker.requestEventuallyServed(req, landed)
    println("Service eventual : " + LTLChecker.check(system, serviceOK))
    assert(LTLChecker.check(system, serviceOK))
  }

  println("No deadlock : " + LTLChecker.check(system, LTLChecker.noDeadlock(system)))
  assert(LTLChecker.check(system, LTLChecker.noDeadlock(system)))

  def lockOK(open: Int, close: Int) =
    LTLChecker.Always(
      LTLChecker.Atomic(m => m.tokens(open) + m.tokens(close) == 1)
    )

  (1 to N).foreach { i =>
    val o = system.places.indexOf(s"TR_open_$i")
    val c = system.places.indexOf(s"TR_close_$i")
    if (o != -1 && c != -1) {
      println(s"Lock TR_$i OK : " + LTLChecker.check(system, lockOK(o, c)))
      assert(LTLChecker.check(system, lockOK(o, c)))
    }
  }

  val iCount = system.places.indexOf("countPlanes")
  val iBuf   = system.places.indexOf("BufferCountPlanes")

  if (iCount != -1 && iBuf != -1) {
    val conservation = LTLChecker.Always(
      LTLChecker.Atomic(m => m.tokens(iCount) + m.tokens(iBuf) == nMaxSystem)
    )
    println("Conservation avions : " + LTLChecker.check(system, conservation))
    assert(LTLChecker.check(system, conservation))
  }
    println("\n=== FIN DU TEST ===")
  }
}