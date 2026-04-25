ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.12"

val AkkaVersion = "2.7.0"

lazy val root = (project in file("."))
  .settings(
    name := "CY_airsim",
    Compile / mainClass := Some("cysky.Main"),
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor-typed"         % AkkaVersion,
      "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
      "org.scalatest"     %% "scalatest"                % "3.2.17"    % Test,
      "ch.qos.logback"     % "logback-classic"          % "1.4.11"
    )
  )
