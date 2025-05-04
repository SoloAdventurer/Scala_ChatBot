name         := "chatbot"
version      := "0.1"
scalaVersion := "3.6.1"

val circeVersion = "0.14.13"

libraryDependencies ++= Seq(
  // Circe dependencies
  "io.circe"                      %% "circe-core"               % circeVersion,
  "io.circe"                      %% "circe-generic"            % circeVersion,
  "io.circe"                      %% "circe-parser"             % circeVersion,
  "io.spray"                      %% "spray-json"               % "1.3.6",
  "org.scala-lang.modules"        %% "scala-parser-combinators" % "2.4.0",
  "org.jsoup"                      % "jsoup"                    % "1.18.1",
  "com.typesafe.akka"             %% "akka-http"                % "10.5.3",
  "com.typesafe.akka"             %% "akka-actor-typed"         % "2.8.5",
  "com.typesafe.akka"             %% "akka-stream"              % "2.8.5",
  "com.typesafe.akka"             %% "akka-slf4j"               % "2.8.5",
  "org.scalatest"                 %% "scalatest"                % "3.2.18" % Test,
  "com.lihaoyi"                   %% "upickle"                  % "3.3.1",
  "com.typesafe"                   % "config"                   % "1.4.3",
  "com.github.tototoshi"          %% "scala-csv"                % "1.3.10",
  "com.softwaremill.sttp.client3" %% "core"                     % "3.10.0",
  "org.slf4j"                      % "slf4j-api"                % "2.0.16",
  "ch.qos.logback"                 % "logback-classic"          % "1.5.8",
  "com.lihaoyi"                   %% "os-lib"                   % "0.11.4"
)

ThisBuild / scalafmtOnCompile := true
