name         := "chatbot"
version      := "0.1"
scalaVersion := "3.6.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.jsoup"               % "jsoup"                    % "1.18.1",
  "com.typesafe.akka"      %% "akka-http"                % "10.5.3", // Added this line
  "com.typesafe.akka"      %% "akka-http-core"           % "10.5.3", // Use core for Scala 3
  "com.typesafe.akka"      %% "akka-actor-typed"         % "2.8.5", // Compatible with Scala 3
  "com.typesafe.akka"      %% "akka-stream"              % "2.6.20",
  "org.scalatest"          %% "scalatest"                % "3.2.18" % Test,
  "com.lihaoyi"            %% "upickle"                  % "4.0.0",
  "com.typesafe"            % "config"                   % "1.4.3",
  "com.github.tototoshi"   %% "scala-csv"                % "1.3.10"
)

ThisBuild / scalafmtOnCompile := true

// TODO (Your Name): Add dependency for Typesafe Config if used in Config.scala.
// TODO (Your Name): Add logging library (e.g., Logback) if needed.
// TODO (Your Name): Verify Akka versions for Render compatibility.
