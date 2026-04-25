package cysky

import akka.Done
import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import cysky.actors.{TowerControlActor, ClockActor}
import cysky.models.ScheduleGeneratorAlgorithm
import java.time.{LocalDate, LocalTime}
import scala.concurrent.Await
import scala.concurrent.duration._

object Main extends App {

  // === PARAMÈTRES ===
  
  println("\nExécution de l'app...")
}