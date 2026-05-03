# Cy-Sky

Simulation d'aéroport en Scala avec Akka, où deux tours de contrôle s'affrontent en temps réel : l'une qui fait confiance au hasard, l'autre qui vérifie formellement chaque décision avec un réseau de Pétri.

## C'est quoi exactement ?

Le projet simule une journée complète d'aéroport — avions en approche, atterrissages, stationnements en gate, décollages — en pilotant tout ça avec des acteurs Akka qui se passent des messages.

La partie intéressante : deux simulations tournent **en parallèle** sur le même planning de vols, mais avec deux approches opposées.

**Mode Libre** — le ScheduleManager insère les événements directement. Si deux avions se retrouvent sur la même piste au même moment, c'est le **BOOM** (collision détectée, vols marqués comme détruits).

**Mode Contrôle** — avant chaque insertion, le planning est vérifié formellement par un réseau de Pétri. Si un conflit est détecté, le SM essaie de décaler les vols gênants, de changer de piste, ou en dernier recours d'annuler. Les urgences, elles, ne sont jamais annulées — le SM force leur placement quoi qu'il arrive.

## Comment lancer

```bash
sbt clean compile
sbt run
```

Ensuite ouvrir **http://localhost:8080**, configurer le nombre de pistes, de gates, d'avions, choisir une graine aléatoire, et cliquer Start. Les deux simulations démarrent ensemble.

```bash
# Tests
sbt test

# Vérification du réseau de Pétri en standalone
sbt "runMain cysky.PetriNetworkApp"

# Visualiser un fichier .dot généré par PetriNetworkApp
dot -Tpng graph.dot -o graph.png
```

## Ce qu'on peut faire depuis le dashboard

En cours de simulation, on peut injecter un **atterrissage d'urgence** à une heure cible. L'événement se déclenche 30 minutes avant pour laisser le temps aux deux ScheduleManagers de trouver (ou forcer) un créneau libre.

On peut aussi ajuster la vitesse de la simulation en direct.

## Architecture en deux mots

Chaque simulation tourne avec sa propre `TowerControlActor` qui spawne et orchestre tous les acteurs enfants : les pistes (`RunwayActor`), les gates (`GarageActor`), les avions (`AirplaneActor`), et le gestionnaire de planning (`ScheduleManagerActor`). Un `ClockActor` envoie des ticks toutes les 112 ms, chaque tick représentant une minute simulée.

Les pistes sont des machines à états strictes : un seul avion à la fois, dans l'ordre `Free → Landing → TaxiToGarage → Free` (ou `Free → TakeoffInProgress → Free`). Même logique pour les avions : `InFlight → Landing → Taxiing → Parked → TaxiOut → Takeoff`.

## Stack

- **Scala 2.13** + **Akka Actor Typed 2.7.0**
- Serveur HTTP embarqué Java (pas de framework externe) pour le dashboard
- Logback pour les logs, ScalaTest pour les tests
