package cysky.petri

object PetriAnalyzer {
  import StateSpaceExplorer._
  
  // Détecte les états de blocage (deadlock)
  def findDeadlocks(petri: PetriModule): Set[Marking] = {
    val graph = buildReachabilityGraph(petri)
    graph.states.filter { marking =>
      enabledTransitions(petri, marking).isEmpty
    }
  }
  
  // Vérifie si le réseau est borné (k-bounded)
  def isBounded(petri: PetriModule, k: Int): Boolean = {
    val graph = buildReachabilityGraph(petri)
    graph.states.forall { marking =>
      marking.tokens.forall(_ <= k)
    }
  }
  
  // Trouve la borne maximale pour chaque place
  def computeBounds(petri: PetriModule): Vector[Int] = {
    val graph = buildReachabilityGraph(petri)
    (0 until petri.places.length).map { placeIdx =>
      graph.states.map(_.tokens(placeIdx)).max
    }.toVector
  }
  
  // Détecte les transitions mortes (jamais franchissables) 
  def findDeadTransitions(petri: PetriModule): Set[String] = {
    val graph = buildReachabilityGraph(petri)
    petri.transitions.zipWithIndex.filter { case (_, tIdx) =>
      !graph.states.exists { marking =>
        enabledTransitions(petri, marking).contains(tIdx)
      }
    }.map(_._1).toSet
  }
  
  // Vérifie la vivacité (toute transition reste franchissable)
  def isLive(petri: PetriModule): Boolean = {
    findDeadTransitions(petri).isEmpty
  }
  
  // Calcule la matrice d'incidence C = Post - Pre
  def incidenceMatrix(petri: PetriModule): Vector[Vector[Int]] = {
    petri.pre.zip(petri.post).map { case (preRow, postRow) =>
      preRow.zip(postRow).map { case (p, q) => q - p }
    }
  }
  
  /* Calcule les P-invariants (vecteurs x tels que C^T · x = 0) 
   *  Méthode simplifiée - pour un vrai projet, utilisez une bibliothèque d'algèbre linéaire
   */
  def computePInvariants(petri: PetriModule): Vector[Vector[Int]] = {
    // TODO: implémenter résolution système homogène
    // Pour l'instant, retourne une liste vide
    // Nécessite algorithme de Gauss ou bibliothèque comme Breeze
    Vector.empty
  }
  
  // Vérifie la réversibilité (retour au marquage initial possible) 
  def isReversible(petri: PetriModule): Boolean = {
    val graph = buildReachabilityGraph(petri)
    val initial = Marking(petri.marking)
    
    graph.states.forall { state =>
      // Depuis chaque état, on doit pouvoir revenir à l'état initial
      canReach(graph, state, initial)
    }
  }
  
  // Vérifie si on peut atteindre 'target' depuis 'start'
  private def canReach(graph: ReachabilityGraph, start: Marking, target: Marking): Boolean = {
    if (start == target) return true
    
    val visited = scala.collection.mutable.Set[Marking]()
    val queue = scala.collection.mutable.Queue[Marking](start)
    
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (current == target) return true
      
      if (!visited.contains(current)) {
        visited += current
        graph.transitions.get(current).foreach { successors =>
          successors.values.flatten.foreach(queue.enqueue)
        }
      }
    }
    false
  }
}