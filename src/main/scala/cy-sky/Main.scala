package cysky

import akka.actor.typed.{ ActorSystem, Behavior }
import akka.actor.typed.scaladsl.Behaviors
import cysky.actors.scheduleGenerator.ScheduleGeneratorActor
import cysky.models.ScheduleGeneratorProtocol._
import java.time.LocalTime
import java.nio.file.{ Files, Paths }
import java.awt.Desktop

object Main extends App {

  val rootBehavior: Behavior[ScheduleGenerated] = Behaviors.setup { context =>

    val generator = context.spawn(ScheduleGeneratorActor(), "schedule-generator")

    generator ! GenerateSchedule(
      terminalId   = "T1",
      runwayCount  = 2,
      maxAirplanes = 5,
      seed         = 42L,
      startTime    = LocalTime.of(6, 0),
      endTime      = LocalTime.of(23, 59),
      replyTo      = context.self
    )

    Behaviors.receiveMessage { case ScheduleGenerated(terminalId, schedule) =>
      val html    = HtmlReport.generate(terminalId, schedule)
      val outPath = Paths.get("schedule_report.html")
      Files.writeString(outPath, html)
      println(s"Rapport généré : ${outPath.toAbsolutePath}")
      if (Desktop.isDesktopSupported) Desktop.getDesktop.browse(outPath.toUri)
      Behaviors.stopped
    }
  }

  ActorSystem(rootBehavior, "aerosim-test")
}