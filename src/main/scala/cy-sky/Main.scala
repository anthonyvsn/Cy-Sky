package cysky


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

object Main extends App {

  // === PARAMÈTRES ===
  val N = 3            // 3 taxiways, 3 pistes
  val nAvions = 5
  val nMaxSystem = 10
  val TICK_INTERVAL: FiniteDuration     = 112.millis
  val SIM_STEP:      java.time.Duration = java.time.Duration.ofMinutes(1)

  // ── TEST PETRI (AVANT SIMULATION) ─────────────────────────────
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
  val ltlOK = LTLChecker.verify(petri, noDeadlock)

  println(s"LTL noDeadlock : $ltlOK")
  println("════════════════════════════════\n")

  // ── Génération du planning ────────────────────────────────────
  val schedule = ScheduleGeneratorAlgorithm.generate(
    terminalId   = "T1",
    runwayCount  = RUNWAY_COUNT,
    maxAirplanes = MAX_AIRPLANES,
    seed         = SCHEDULE_SEED,
    startTime    = SIM_START,
    endTime      = SIM_END
  )

  // === MODULE GARAGE (T1-T4, Cargo, Maintenance) ===
  val garage = PetriModule(
    places      = Vector("BufferGarage", "Garage", "G_open", "G_close"),
    transitions = Vector("lockGarage", "unlockGarage"),
    pre  = Vector(
      Vector(0, 0),   // BufferGarage
      Vector(1, 0),   // Garage         → lockGarage consomme 1 avion
      Vector(1, 0),   // G_open         → lockGarage doit consommer open (sinon accumulation)
      Vector(0, 1)    // G_close        → unlockGarage consomme le verrou
    ),
    post = Vector(
      Vector(0, 1),   // BufferGarage   → unlockGarage libère une place
      Vector(0, 0),   // Garage
      Vector(0, 1),   // G_open         → unlockGarage rouvre
      Vector(1, 0)    // G_close        → lockGarage verrouille
    ),
    marking = Vector(nMaxSystem - nAvions, nAvions, 1, 0)

  )

  // === MODULE TAXIWAY (base) ===
  val taxiWayBase = PetriModule(
    places      = Vector("BufferTaxiWay", "TaxiWay", "TW_open", "TW_close"),
    transitions = Vector("addTaxiWay", "lockTaxiWay", "unlockTaxiWay"),
    pre  = Vector(
      Vector(0, 0, 0),
      Vector(0, 1, 0),
      Vector(1, 1, 0),   // TW_open → lockTaxiWay doit consommer open
      Vector(0, 0, 1)
    ),
    post = Vector(
      Vector(0, 0, 1),
      Vector(1, 0, 0),
      Vector(1, 0, 1),
      Vector(0, 1, 0)
    ),
    marking = Vector(nMaxSystem - nAvions, 0, 1, 0)
  )

  // === MODULE TRACK (base) ===
  // Colonnes : landing | lockTrack | unlockTrack | takeoff | addOnTrack
  val trackBase = PetriModule(
    places      = Vector("BufferTrack", "Track", "TR_open", "TR_close"),
    transitions = Vector("landing", "lockTrack", "unlockTrack", "takeoff", "addOnTrack"),
    pre  = Vector(
      Vector(0, 0, 0, 0, 0),  // BufferTrack
      Vector(0, 1, 0, 0, 0),  // Track        → lockTrack consomme
      Vector(1, 1, 0, 1, 1),  // TR_open      → lockTrack doit consommer open + landing/takeoff/addOnTrack
      Vector(0, 0, 1, 0, 0)   // TR_close     → unlockTrack consomme le verrou
    ),
    post = Vector(
      Vector(0, 0, 1, 0, 0),  // BufferTrack  → unlockTrack libère
      Vector(1, 0, 0, 0, 1),  // Track        → landing & addOnTrack placent un avion
      Vector(1, 0, 1, 1, 1),  // TR_open      → self-loop pour landing/unlockTrack/takeoff/addOnTrack
      Vector(0, 1, 0, 0, 0)   // TR_close     → lockTrack verrouille
    ),
    marking = Vector(nMaxSystem - nAvions, 0, 1, 0)
  )

  // === RÉPLICATION & ASSEMBLAGE DIAGONAL ===
  val allTaxiWays = replicateModule(taxiWayBase, N)
  val allTracks   = replicateModule(trackBase, N)
  val base        = blockDiag(garage, blockDiag(allTaxiWays, allTracks))

  // === LIEN GARAGE → TAXIWAY (fonctionnel, pas de boucle) ===
  val garageToTwPairs = (1 to N).map(j => j)

  val withGarageTw = garageToTwPairs.foldLeft(base) { (sys, j) =>
    val s1 = addLinkTransition(sys, s"garage_to_tw$j",
      "Garage", s"TaxiWay_$j")
    val s2 = addArc(s1, s"BufferTaxiWay_$j", s"garage_to_tw$j", 1, 0)
    val s3 = addArc(s2, "BufferGarage",      s"garage_to_tw$j", 0, 1)
    val s4 = addArc(s3, "G_open",            s"garage_to_tw$j", 1, 1)
    addArc(s4, s"TW_open_$j",               s"garage_to_tw$j", 1, 1)
  }

  // === LIEN TW → TRACK (N×N = 9 transitions) ===
  val twToTrPairs = for (i <- 1 to N; j <- 1 to N) yield (i, j)

  val withTwTr = twToTrPairs.foldLeft(withGarageTw) { case (sys, (i, j)) =>
    val s1 = addLinkTransition(sys, s"tw${i}_to_tr${j}",
      s"TaxiWay_$i", s"Track_$j")
    val s2 = addArc(s1, s"BufferTrack_$j",   s"tw${i}_to_tr${j}", 1, 0)
    val s3 = addArc(s2, s"BufferTaxiWay_$i", s"tw${i}_to_tr${j}", 0, 1)
    addArc(s3, s"TR_open_$j",               s"tw${i}_to_tr${j}", 1, 1)
  }

  // === REDIRECTIONS TW ↔ TW ===
  val redirectPairs = for (i <- 1 to N; j <- 1 to N; if i != j) yield (i, j)

  val withRedirects = redirectPairs.foldLeft(withTwTr) { case (sys, (i, j)) =>
    val s1 = addLinkTransition(sys, s"redirect_tw${i}_to_tw${j}",
      s"TaxiWay_$i", s"TaxiWay_$j")
    addArc(s1, s"TW_open_$j", s"redirect_tw${i}_to_tw${j}", 1, 1)
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

  println(s"\n=== MARQUAGE INITIAL ===")
  println(system.marking.mkString("[", ", ", "]"))

  // === GÉNÉRATION DES SCHÉMAS ===
  PetriVisualizer.saveHtml(system, "petri_schema.html", "CY-SKY Orly")
  PetriVisualizer.saveDot(system, "petri_schema.dot", "CY-SKY Orly")

  // =========================================================================
  // === FONCTION DE VÉRIFICATION ============================================
  // =========================================================================
  // Vérifie les invariants structurels du réseau sur un marquage donné :
  //   1. Aucun jeton négatif
  //   2. Conservation globale : countPlanes + BufferCountPlanes == nMaxSystem
  //   3. Exclusion mutuelle des verrous (open + close == 1 pour chaque module)
  def verify(m: PetriModule): (Boolean, List[String]) = {
    val errors = scala.collection.mutable.ListBuffer[String]()

    // 1. Aucun jeton négatif
    m.marking.zipWithIndex.foreach { case (tokens, i) =>
      if (tokens < 0)
        errors += s"Jeton négatif : ${m.places(i)} = $tokens"
    }

    // 2. Conservation : countPlanes + BufferCountPlanes == nMaxSystem
    val iCount  = m.places.indexOf("countPlanes")
    val iBuf    = m.places.indexOf("BufferCountPlanes")
    if (iCount >= 0 && iBuf >= 0) {
      val total = m.marking(iCount) + m.marking(iBuf)
      if (total != nMaxSystem)
        errors += s"Conservation violée : countPlanes(${m.marking(iCount)}) + BufferCountPlanes(${m.marking(iBuf)}) = $total ≠ $nMaxSystem"
    }

    // 3. Exclusion mutuelle des verrous (open XOR close == 1)
    def checkLock(openName: String, closeName: String): Unit = {
      val iO = m.places.indexOf(openName)
      val iC = m.places.indexOf(closeName)
      if (iO >= 0 && iC >= 0) {
        val sum = m.marking(iO) + m.marking(iC)
        if (sum != 1)
          errors += s"Verrou incohérent : $openName(${m.marking(iO)}) + $closeName(${m.marking(iC)}) = $sum ≠ 1"
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

  // =========================================================================
  // === TEST ALÉATOIRE (1000 transitions) ===================================
  // =========================================================================
  // Tire aléatoirement une transition franchissable à chaque étape.
  // Vérifie les invariants après chaque franchissement.
  // En cas de deadlock : affiche un avertissement et réinitialise l'état.
  def randomTest(initial: PetriModule, steps: Int = 1000): Unit = {
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
    if (errorCount == 0 && deadlockCount == 0)
      println(s"  => Test REUSSI : aucune erreur sur $steps transitions")
    else
      println(s"  => Test ECHOUE")
  }

  // Lancement du test
  randomTest(system)
}