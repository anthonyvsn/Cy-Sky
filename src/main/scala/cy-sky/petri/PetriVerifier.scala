package cysky.petri

object PetriVerifier {
  
  def analyzeAndReport(petri: PetriModule, name: String): Unit = {
    println(s"\n═══════════════════════════════════════════════")
    println(s"ANALYSE FORMELLE : $name")
    println(s"═══════════════════════════════════════════════")
    
    // 1. Espace d'états
    val graph = StateSpaceExplorer.buildReachabilityGraph(petri)
    println(s"✓ États accessibles : ${graph.states.size}")
    println(s"✓ Transitions totales dans le graphe : ${graph.transitions.values.flatMap(_.values).map(_.size).sum}")
    
    // 2. Propriétés structurelles
    val deadlocks = PetriAnalyzer.findDeadlocks(petri)
    println(s"✓ Deadlocks détectés : ${deadlocks.size}")
    if (deadlocks.nonEmpty) {
      deadlocks.take(3).foreach(d => println(s"  → $d"))
    }
    
    val bounds = PetriAnalyzer.computeBounds(petri)
    println(s"✓ Bornes par place : ${bounds.mkString(", ")}")
    println(s"✓ Réseau borné (≤10) : ${PetriAnalyzer.isBounded(petri, 10)}")
    
    val deadTrans = PetriAnalyzer.findDeadTransitions(petri)
    println(s"✓ Transitions mortes : ${deadTrans.mkString(", ")}")
    println(s"✓ Réseau vivant : ${PetriAnalyzer.isLive(petri)}")
    
    // 3. Vérification LTL (exemples)
    // TODO: adapter les indices selon votre modèle
    // val safetyOK = LTLChecker.verify(petri, LTLChecker.noOverflow(0, 2))
    // println(s"✓ Propriété sûreté : $safetyOK")
  }
}