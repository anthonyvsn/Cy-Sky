package cysky

import cysky.petri.PetriVisualizer
import cysky.petri.PetriModule
import cysky.petri.PetriComposer._
import cysky.petri.{LTLChecker, PetriVerifier}

// ═══════════════════════════════════════════════════════════════
// PetriNetworkApp — construction, visualisation, vérification LTL
// et test aléatoire du réseau de Pétri CySky (standalone).
//
// Lance avec : sbt "runMain cysky.PetriNetworkApp"
// ═══════════════════════════════════════════════════════════════
object PetriNetworkApp extends App {

  val N          = 3   // 3 taxiways, 3 pistes
  val nAvions    = 5
  val nMaxSystem = 10

  // === MODULE GARAGE (T1-T4, Cargo, Maintenance) ===
  val garage = PetriModule(
    places      = Vector("BufferGarage", "Garage", "G_open", "G_close"),
    transitions = Vector("lockGarage", "unlockGarage"),
    pre  = Vector(
      Vector(0, 0),
      Vector(1, 0),
      Vector(1, 0),
      Vector(0, 1)
    ),
    post = Vector(
      Vector(0, 1),
      Vector(0, 1),
      Vector(0, 1),
      Vector(1, 0)
    ),
    marking = Vector(nMaxSystem - nAvions, nAvions, 1, 0)
  )

  // === MODULE TAXIWAY — avec Cap_TaxiWay (fix Marjorie) ===
  val taxiWayBase = PetriModule(
    places      = Vector("BufferTaxiWay", "TaxiWay", "TW_open", "TW_close", "Cap_TaxiWay"),
    transitions = Vector("addTaxiWay", "lockTaxiWay", "unlockTaxiWay"),
    pre  = Vector(
      Vector(1, 0, 0),  // BufferTaxiWay
      Vector(0, 1, 1),  // TaxiWay
      Vector(0, 1, 0),  // TW_open
      Vector(0, 0, 1),  // TW_close
      Vector(1, 0, 0)   // Cap_TaxiWay
    ),
    post = Vector(
      Vector(0, 0, 0),  // BufferTaxiWay
      Vector(1, 1, 1),  // TaxiWay
      Vector(0, 0, 1),  // TW_open
      Vector(0, 1, 0),  // TW_close
      Vector(0, 0, 1)   // Cap_TaxiWay — restaurée par unlockTaxiWay
    ),
    marking = Vector(nMaxSystem - nAvions, 0, 1, 0, 1)
  )

  // === MODULE TRACK (base) ===
  val trackBase = PetriModule(
    places      = Vector("BufferTrack", "Track", "TR_open", "TR_close"),
    transitions = Vector("landing", "lockTrack", "unlockTrack", "takeoff", "addOnTrack"),
    pre  = Vector(
      Vector(0, 0, 1, 0, 0),
      Vector(0, 1, 1, 1, 0),
      Vector(1, 1, 0, 1, 1),
      Vector(0, 0, 1, 0, 0)
    ),
    post = Vector(
      Vector(0, 0, 1, 1, 0),
      Vector(1, 1, 1, 0, 1),
      Vector(1, 0, 1, 1, 1),
      Vector(0, 1, 0, 0, 0)
    ),
    marking = Vector(nMaxSystem - nAvions, 0, 1, 0)
  )

  // === RÉPLICATION & ASSEMBLAGE DIAGONAL ===
  val allTaxiWays = replicateModule(taxiWayBase, N)
  val allTracks   = replicateModule(trackBase, N)
  val base        = blockDiag(garage, blockDiag(allTaxiWays, allTracks))

  // === LIEN GARAGE → TAXIWAY ===
  val withGarageTw = (1 to N).foldLeft(base) { (sys, j) =>
    val s1 = addLinkTransition(sys, s"garage_to_tw$j", "Garage", s"TaxiWay_$j")
    val s2 = addArc(s1, s"BufferTaxiWay_$j", s"garage_to_tw$j", 1, 0)
    val s3 = addArc(s2, "BufferGarage",      s"garage_to_tw$j", 0, 1)
    val s4 = addArc(s3, "G_open",            s"garage_to_tw$j", 1, 1)
    val s5 = addArc(s4, s"TW_open_$j",       s"garage_to_tw$j", 1, 1)
    addArc(s5, s"Cap_TaxiWay_$j",            s"garage_to_tw$j", 1, 0)
  }

  // === LIEN TW → TRACK (N×N) avec restauration Cap_TaxiWay ===
  val twToTrPairs = for (i <- 1 to N; j <- 1 to N) yield (i, j)

  val withTwTr = twToTrPairs.foldLeft(withGarageTw) { case (sys, (i, j)) =>
    val s1 = addLinkTransition(sys, s"tw${i}_to_tr${j}", s"TaxiWay_$i", s"Track_$j")
    val s2 = addArc(s1, s"BufferTrack_$j",   s"tw${i}_to_tr${j}", 1, 0)
    val s3 = addArc(s2, s"BufferTaxiWay_$i", s"tw${i}_to_tr${j}", 0, 1)
    val s4 = addArc(s3, s"TW_open_$i",       s"tw${i}_to_tr${j}", 1, 1)
    val s5 = addArc(s4, s"TR_open_$j",       s"tw${i}_to_tr${j}", 1, 1)
    addArc(s5, s"Cap_TaxiWay_$i",            s"tw${i}_to_tr${j}", 0, 1)
  }

  // === REDIRECTIONS TW ↔ TW avec gestion capacité ===
  val redirectPairs = for (i <- 1 to N; j <- 1 to N; if i != j) yield (i, j)

  val withRedirects = redirectPairs.foldLeft(withTwTr) { case (sys, (i, j)) =>
    val s1 = addLinkTransition(sys, s"redirect_tw${i}_to_tw${j}", s"TaxiWay_$i", s"TaxiWay_$j")
    val s2 = addArc(s1, s"TW_open_$i",      s"redirect_tw${i}_to_tw${j}", 1, 1)
    val s3 = addArc(s2, s"TW_open_$j",      s"redirect_tw${i}_to_tw${j}", 1, 1)
    val s4 = addArc(s3, s"Cap_TaxiWay_$i",  s"redirect_tw${i}_to_tw${j}", 0, 1)
    addArc(s4, s"Cap_TaxiWay_$j",           s"redirect_tw${i}_to_tw${j}", 1, 0)
  }

  // === countPlanes GLOBAL ===
  val withCount = addPlace(
    addPlace(withRedirects, "countPlanes", nAvions),
    "BufferCountPlanes", nMaxSystem - nAvions
  )

  val petriNet = (1 to N).foldLeft(withCount) { (sys, i) =>
    val s1 = addArc(sys, "countPlanes",       s"takeoff_$i",     1, 0)
    val s2 = addArc(s1,  "BufferCountPlanes", s"takeoff_$i",     0, 1)
    val s3 = addArc(s2,  "BufferCountPlanes", s"landing_$i",     1, 0)
    val s4 = addArc(s3,  "countPlanes",       s"landing_$i",     0, 1)
    val s5 = addArc(s4,  "BufferCountPlanes", s"addOnTrack_$i",  1, 0)
    addArc(s5,           "countPlanes",       s"addOnTrack_$i",  0, 1)
  }

  // === AFFICHAGE ===
  println(s"=== PLACES (${petriNet.places.length}) ===")
  petriNet.places.zipWithIndex.foreach { case (p, i) =>
    println(f"  P$i%2d : $p  [M0 = ${petriNet.marking(i)}]")
  }
  println(s"\n=== TRANSITIONS (${petriNet.transitions.length}) ===")
  petriNet.transitions.zipWithIndex.foreach { case (t, j) =>
    println(f"  T$j%2d : $t")
  }

  // === GÉNÉRATION DES SCHÉMAS ===
  PetriVisualizer.saveHtml(petriNet, "petri_schema.html", "CY-SKY Orly")
  PetriVisualizer.saveDot(petriNet, "petri_schema.dot", "CY-SKY Orly")

  // === ANALYSE PÉTRI (Marjorie) ===
  PetriVerifier.analyzeAndReport(petriNet, "CY-SKY Orly")

  // === VÉRIFICATION LTL (Marjorie) ===
  println("\n=== VÉRIFICATION LTL ===")

  println("No deadlock : " + LTLChecker.check(petriNet, LTLChecker.noDeadlock(petriNet)))

  (1 to N).foreach { i =>
    val idx = petriNet.places.indexOf(s"TaxiWay_$i")
    if (idx != -1) {
      val formula = LTLChecker.noOverflow(idx, 1)
      println(s"TaxiWay_$i capacité ≤ 1 : " + LTLChecker.check(petriNet, formula))
    }
  }

  val req    = petriNet.places.indexOf("BufferTrack_1")
  val landed = petriNet.places.indexOf("Track_1")
  if (req != -1 && landed != -1)
    println("Service éventuel Track_1 : " + LTLChecker.check(petriNet, LTLChecker.requestEventuallyServed(req, landed)))

  def lockOK(open: Int, close: Int) =
    LTLChecker.Always(LTLChecker.Atomic(m => m.tokens(open) + m.tokens(close) == 1))

  (1 to N).foreach { i =>
    val o = petriNet.places.indexOf(s"TR_open_$i")
    val c = petriNet.places.indexOf(s"TR_close_$i")
    if (o != -1 && c != -1)
      println(s"Verrou TR_$i : " + LTLChecker.check(petriNet, lockOK(o, c)))
  }

  val iCount = petriNet.places.indexOf("countPlanes")
  val iBuf   = petriNet.places.indexOf("BufferCountPlanes")
  if (iCount != -1 && iBuf != -1) {
    val conservation = LTLChecker.Always(
      LTLChecker.Atomic(m => m.tokens(iCount) + m.tokens(iBuf) == nMaxSystem)
    )
    println("Conservation avions : " + LTLChecker.check(petriNet, conservation))
  }

  // === TEST ALÉATOIRE ===
  def verify(m: PetriModule): (Boolean, List[String]) = {
    val errors = scala.collection.mutable.ListBuffer[String]()
    m.marking.zipWithIndex.foreach { case (tokens, i) =>
      if (tokens < 0) errors += s"Jeton négatif : ${m.places(i)} = $tokens"
    }
    val iC = m.places.indexOf("countPlanes")
    val iB = m.places.indexOf("BufferCountPlanes")
    if (iC >= 0 && iB >= 0) {
      val total = m.marking(iC) + m.marking(iB)
      if (total != nMaxSystem)
        errors += s"Conservation violée : $total ≠ $nMaxSystem"
    }
    def checkLock(openName: String, closeName: String): Unit = {
      val iO = m.places.indexOf(openName)
      val iCl = m.places.indexOf(closeName)
      if (iO >= 0 && iCl >= 0 && m.marking(iO) + m.marking(iCl) != 1)
        errors += s"Verrou incohérent : $openName + $closeName ≠ 1"
    }
    checkLock("G_open", "G_close")
    (1 to N).foreach { i =>
      checkLock(s"TW_open_$i", s"TW_close_$i")
      checkLock(s"TR_open_$i", s"TR_close_$i")
    }
    (errors.isEmpty, errors.toList)
  }

  def randomTest(initial: PetriModule, steps: Int = 1000): Unit = {
    import cysky.petri.PetriComposer.{enabledTransitions, fireTransition}
    val rng = new scala.util.Random(42)
    var state = initial
    var fired = 0; var deadlockCount = 0; var errorCount = 0
    println(s"\n=== TEST ALÉATOIRE ($steps transitions) ===")
    for (step <- 1 to steps) {
      val enabled = enabledTransitions(state)
      if (enabled.isEmpty) {
        println(s"  [DEADLOCK] Étape $step — réinitialisation")
        deadlockCount += 1; state = initial
      } else {
        val tIdx = enabled(rng.nextInt(enabled.length))
        state = fireTransition(state, tIdx)
        fired += 1
        val (ok, errs) = verify(state)
        if (!ok) { errs.foreach(e => println(s"  [ERREUR] $e")); errorCount += 1 }
      }
    }
    println(s"  Franchies: $fired  Deadlocks: $deadlockCount  Erreurs: $errorCount")
    println(if (errorCount == 0 && deadlockCount == 0) "  => Test REUSSI" else "  => Test ECHOUE")
  }

  randomTest(petriNet)
}
