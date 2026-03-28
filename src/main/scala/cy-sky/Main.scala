package cysky

import cysky.petri.PetriVisualizer
import cysky.petri.PetriModule
import cysky.petri.PetriComposer._
import java.time.LocalTime
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object Main extends App {

    
  val N = 3
  val nAvions = 5
  val nMaxSystem = 10

  // --- Définition des modules de base ---

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


  // --- Créer les N modules ---
  val allTaxiWays = replicateModule(taxiWayBase, N)
  val allTracks   = replicateModule(trackBase, N)

  // --- Bloc diagonal global ---
  var system = blockDiag(allTaxiWays, allTracks)

  // === LIEN A : TaxiWay_i → Track_j (N × N transitions) ===
  for (i <- 1 to N; j <- 1 to N) {
    system = addLinkTransition(
      system,
      name      = s"tw${i}_to_tr${j}",
      fromPlace = s"TaxiWay_$i",      // avion quitte le taxiway
      toPlace   = s"Track_$j"         // avion arrive sur la piste
    )
    // Le buffer track doit aussi être consommé (capacité)
    system = addArc(system, s"BufferTrack_$j", s"tw${i}_to_tr${j}",
                    preCost = 1, postGain = 0)
    // Le buffer taxiway est libéré
    system = addArc(system, s"BufferTaxiWay_$i", s"tw${i}_to_tr${j}",
                    preCost = 0, postGain = 1)
    // Vérifier que la piste est ouverte (self-loop sur open)
    system = addArc(system, s"TR_open_$j", s"tw${i}_to_tr${j}",
                    preCost = 1, postGain = 1)
  }

  // === LIEN B : Redirection TaxiWay_i → TaxiWay_j ===
  for (i <- 1 to N; j <- 1 to N; if i != j) {
    system = addLinkTransition(
      system,
      name      = s"redirect_tw${i}_to_tw${j}",
      fromPlace = s"TaxiWay_$i",
      toPlace   = s"TaxiWay_$j"
    )
    // Vérifier que le taxiway destination est ouvert
    system = addArc(system, s"TW_open_$j", s"redirect_tw${i}_to_tw${j}",
                    preCost = 1, postGain = 1)
  }

  // === LIEN C : countPlanes global (partagé par tous les tracks) ===
  system = addPlace(system, "countPlanes", nAvions)
  system = addPlace(system, "BufferCountPlanes", nMaxSystem - nAvions)

  for (i <- 1 to N) {
    // Takeoff : un avion sort du système → countPlanes diminue
    system = addArc(system, "countPlanes",       s"takeoff_$i",
                    preCost = 1, postGain = 0)
    system = addArc(system, "BufferCountPlanes", s"takeoff_$i",
                    preCost = 0, postGain = 1)

    // Landing : un avion entre → countPlanes augmente
    system = addArc(system, "BufferCountPlanes", s"landing_$i",
                    preCost = 1, postGain = 0)
    system = addArc(system, "countPlanes",       s"landing_$i",
                    preCost = 0, postGain = 1)
  }
  // === AFFICHAGE DES MATRICES ===

  println(s"=== PLACES (${system.places.length}) ===")
  system.places.zipWithIndex.foreach { case (p, i) =>
    println(f"  P$i%2d : $p  [M0 = ${system.marking(i)}]")
  }

  println(s"\n=== TRANSITIONS (${system.transitions.length}) ===")
  system.transitions.zipWithIndex.foreach { case (t, j) =>
    println(f"  T$j%2d : $t")
  }

  val header = "".padTo(25, ' ') + system.transitions.indices.map(j => "%4s".format(s"T$j")).mkString

  println(s"\n=== MATRICE PRE (${system.pre.length} x ${system.pre.head.length}) ===")
  println(header)
  system.pre.zipWithIndex.foreach { case (row, i) =>
    val label = "P%-2d %-20s".format(i, system.places(i))
    println(label + row.map(v => "%4d".format(v)).mkString)
  }

  println(s"\n=== MATRICE POST (${system.post.length} x ${system.post.head.length}) ===")
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

  // === GÉNÉRATION DU SCHÉMA ===
  PetriVisualizer.saveHtml(system, "petri_schema.html", "CY-SKY Aeroport")
  PetriVisualizer.saveDot(system, "petri_schema.dot", "CY-SKY Aeroport")
}