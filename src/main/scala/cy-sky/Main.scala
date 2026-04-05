package cysky

import cysky.petri.PetriVisualizer
import cysky.petri.PetriModule
import cysky.petri.PetriComposer._

object Main extends App {

  // === PARAMÈTRES ===
  val N = 3            // 3 taxiways, 3 pistes
  val nAvions = 5
  val nMaxSystem = 10

  // === MODULE GARAGE (T1-T4, Cargo, Maintenance) ===
  val garage = PetriModule(
    places      = Vector("BufferGarage", "Garage", "G_open", "G_close"),
    transitions = Vector("lockGarage", "unlockGarage"),
    pre  = Vector(
      Vector(0, 0),   // BufferGarage
      Vector(1, 0),   // Garage         → lockGarage consomme 1 avion
      Vector(0, 0),   // G_open
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
      Vector(1, 0, 0),
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
  val trackBase = PetriModule(
    places      = Vector("BufferTrack", "Track", "TR_open", "TR_close"),
    transitions = Vector("landing", "lockTrack", "unlockTrack", "takeoff"),
    pre  = Vector(
      Vector(0, 0, 0, 0),
      Vector(0, 1, 0, 0),
      Vector(1, 0, 0, 1),
      Vector(0, 0, 1, 0)
    ),
    post = Vector(
      Vector(0, 0, 1, 0),
      Vector(1, 0, 0, 0),
      Vector(1, 0, 1, 1),
      Vector(0, 1, 0, 0)
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
    val s1 = addArc(sys, "countPlanes",       s"takeoff_$i",  1, 0)
    val s2 = addArc(s1,  "BufferCountPlanes", s"takeoff_$i",  0, 1)
    val s3 = addArc(s2,  "BufferCountPlanes", s"landing_$i",  1, 0)
    addArc(s3, "countPlanes",                 s"landing_$i",  0, 1)
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
}