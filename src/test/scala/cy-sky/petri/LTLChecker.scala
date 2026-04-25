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
  def eval(m: Marking,
           f: LTLFormula,
           graph: ReachabilityGraph,
           seen: Set[Marking] = Set()
          ): Boolean = {

    f match {

      case Atomic(pred) =>
        pred(m)

      case Not(f1) =>
        !eval(m, f1, graph, seen)

      case And(f1, f2) =>
        eval(m, f1, graph, seen) &&
        eval(m, f2, graph, seen)

      case Or(f1, f2) =>
        eval(m, f1, graph, seen) ||
        eval(m, f2, graph, seen)

      case Next(f1) =>
        graph.transitions.getOrElse(m, Map.empty)
          .values.flatten
          .forall(m2 => eval(m2, f1, graph, seen + m))

      case Eventually(f1) =>
        eval(m, f1, graph, seen) ||
        graph.transitions.getOrElse(m, Map.empty)
          .values.flatten
          .exists(m2 =>
            !seen.contains(m2) &&
            eval(m2, f1, graph, seen + m)
          )

      case Always(f1) =>
        eval(m, f1, graph, seen) &&
        graph.transitions.getOrElse(m, Map.empty)
          .values.flatten
          .forall(m2 =>
            !seen.contains(m2) &&
            eval(m2, f1, graph, seen + m)
          )

      case Until(f1, f2) =>
        if (eval(m, f2, graph, seen)) true
        else if (!eval(m, f1, graph, seen)) false
        else
          graph.transitions.getOrElse(m, Map.empty)
            .values.flatten
            .forall(m2 =>
              !seen.contains(m2) &&
              eval(m2, f1, graph, seen + m)
            )
      case Implies(f1, f2) =>
        !eval(m, f1, graph, seen) ||
        eval(m, f2, graph, seen)
    }
  }

  def check(petri: PetriModule, formula: LTLFormula): Boolean = {
    val graph = StateSpaceExplorer.buildReachabilityGraph(petri)
    val init  = graph.states.head
    eval(init, formula, graph)
  }
  
  // Génère tous les chemins du graphe (jusqu'à une profondeur max)
  def generatePaths(graph: ReachabilityGraph, maxDepth: Int = 5): Set[Path] = {
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