# Cy Sky

Modélisation et vérification d'une application critique distribuée d'aéroport avec Scala et Réseaux de Pétri.

## Lancer le projet
`sbt clean`

`sbt compile`

- Lancer l'application :  `sbt run`
- Lancer les tests : `sbt test`

Vérifier le réseau de Pétri : `sbt "runMain cysky.PetriNetworkApp"`
  
Créer une image PNG pour visualiser les fichiers dot : `dot -Tpng graph.dot -p graph.png`
