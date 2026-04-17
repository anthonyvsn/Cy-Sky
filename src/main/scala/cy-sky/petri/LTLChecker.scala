package cysky.petri

object LTLChecker {
  import StateSpaceExplorer._
  
  // Formules LTL
  sealed trait LTLFormula
  case class Atomic(predicate: Marking => Boolean) extends LTLFormula
  case class Not(f: LTLFormula) extends LTLFormula
  case class And(f1: LTLFormula, f2: LTLFormula) extends LTLFormula
  case class Or(f1: LTLFormula, f2: LTLFormula) extends LTLFormula
  case class Implies(f1: LTLFormula, f2: LTLFormula) extends LTLFormula
  case class Next(f: LTLFormula) extends LTLFormula              // X φ
  case class Always(f: LTLFormula) extends LTLFormula            // G φ
  case class Eventually(f: LTLFormula) extends LTLFormula        // F φ
  case class Until(f1: LTLFormula, f2: LTLFormula) extends LTLFormula  // φ U ψ
  
  type Path = List[Marking]
  
  // Évalue une formule sur un chemin infini (simplifié : chemins finis)
  def evaluateOnPath(path: Path, formula: LTLFormula): Boolean = {
    if (path.isEmpty) return false
    
    formula match {
      case Atomic(pred) => pred(path.head)
      
      case Not(f) => !evaluateOnPath(path, f)
      
      case And(f1, f2) => 
        evaluateOnPath(path, f1) && evaluateOnPath(path, f2)
      
      case Or(f1, f2) => 
        evaluateOnPath(path, f1) || evaluateOnPath(path, f2)
      
      case Implies(f1, f2) => 
        !evaluateOnPath(path, f1) || evaluateOnPath(path, f2)
      
      case Next(f) => 
        if (path.tail.isEmpty) false 
        else evaluateOnPath(path.tail, f)
      
      case Always(f) => 
        path.forall(m => evaluateOnPath(List(m), f))
      
      case Eventually(f) => 
        path.exists(m => evaluateOnPath(List(m), f))
      
      case Until(f1, f2) =>
        path.indices.exists { i =>
          evaluateOnPath(List(path(i)), f2) &&
          (0 until i).forall(j => evaluateOnPath(List(path(j)), f1))
        }
    }
  }
  
  // Génère tous les chemins du graphe (jusqu'à une profondeur max)
  def generatePaths(graph: ReachabilityGraph, maxDepth: Int = 10): Set[Path] = {
    val initial = graph.states.headOption.getOrElse(return Set.empty)
    
    def explore(current: Marking, depth: Int, visited: Set[Marking]): Set[Path] = {
      if (depth >= maxDepth) return Set(List(current))
      
      val successors = graph.transitions.getOrElse(current, Map.empty).values.flatten.toSet
      
      if (successors.isEmpty) {
        Set(List(current))
      } else {
        successors.flatMap { next =>
          if (visited.contains(next)) {
            Set(List(current, next)) // Cycle détecté
          } else {
            explore(next, depth + 1, visited + next).map(current :: _)
          }
        }
      }
    }
    
    explore(initial, 0, Set(initial))
  }
  
  // Vérifie une formule sur TOUS les chemins du graphe
  def verify(petri: PetriModule, formula: LTLFormula): Boolean = {
    val graph = buildReachabilityGraph(petri)
    val paths = generatePaths(graph)
    
    paths.forall { path =>
      evaluateOnPath(path, formula)
    }
  }
  
  // ═══════════════════════════════════════════════════════════
  // EXEMPLES DE PROPRIÉTÉS POUR UN SYSTÈME AÉRIEN
  // ═══════════════════════════════════════════════════════════
  
  // Exemple : "Jamais plus de N avions sur une piste"
  def noOverflow(placeIdx: Int, maxTokens: Int): LTLFormula = 
    Always(Atomic(_.tokens(placeIdx) <= maxTokens))
  
  // Exemple : "Si un avion demande un atterrissage, il finit par atterrir"
  def requestEventuallyServed(requestPlace: Int, landedPlace: Int): LTLFormula = 
    Always(
      Implies(
        Atomic(_.tokens(requestPlace) > 0),
        Eventually(Atomic(_.tokens(landedPlace) > 0))
      )
    )
  
  // Exemple : "Le système ne reste jamais bloqué indéfiniment"
  def noDeadlock(petri: PetriModule): LTLFormula = 
    Always(Atomic { m =>
      enabledTransitions(petri, m).nonEmpty
    })
}