package cysky.petri

import scala.collection.mutable

object StateSpaceExplorer {
  
  case class Marking(tokens: Vector[Int]) {
    override def toString: String = tokens.mkString("[", ",", "]")
  }
  
  case class ReachabilityGraph(
    states: Set[Marking],
    transitions: Map[Marking, Map[String, Set[Marking]]]
  )
  
  // Trouve les transitions franchissables pour un marquage donné
  def enabledTransitions(petri: PetriModule, marking: Marking): Set[Int] = {
    (0 until petri.transitions.length).filter { j =>
      petri.pre.indices.forall { i =>
        marking.tokens(i) >= petri.pre(i)(j)
      }
    }.toSet
  }
  
  // Franchit une transition et retourne le nouveau marquage
  def fireTransition(petri: PetriModule, marking: Marking, transIdx: Int): Marking = {
    val newTokens = marking.tokens.zipWithIndex.map { case (tok, i) =>
      tok - petri.pre(i)(transIdx) + petri.post(i)(transIdx)
    }
    Marking(newTokens)
  }
  
  // Construit le graphe d'accessibilité complet (BFS)
  def buildReachabilityGraph(petri: PetriModule): ReachabilityGraph = {
    val initial = Marking(petri.marking)
    val visited = mutable.Set[Marking]()
    val queue = mutable.Queue[Marking](initial)
    val edges = mutable.Map[Marking, Map[String, Set[Marking]]]()
    
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      
      if (!visited.contains(current)) {
        visited += current
        val successors = mutable.Map[String, Set[Marking]]()
        
        enabledTransitions(petri, current).foreach { tIdx =>
          val transName = petri.transitions(tIdx)
          val nextMarking = fireTransition(petri, current, tIdx)
          
          successors(transName) = successors.getOrElse(transName, Set.empty) + nextMarking
          
          if (!visited.contains(nextMarking)) {
            queue.enqueue(nextMarking)
          }
        }
        
        edges(current) = successors.toMap
      }
    }
    
    ReachabilityGraph(visited.toSet, edges.toMap)
  }
  
  // Vérifie si un marquage cible est atteignable
  def isReachable(petri: PetriModule, target: Vector[Int]): Boolean = {
    val graph = buildReachabilityGraph(petri)
    graph.states.exists(_.tokens == target)
  }
}