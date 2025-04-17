package chatbot.server

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import scala.io.StdIn
import scala.util.{Failure, Success}

object WebServer {
  def main(args: Array[String]): Unit = {
    implicit val system           = ActorSystem(Behaviors.empty, "ChatbotSystem")
    implicit val executionContext = system.executionContext

    val route =
      path("chat") {
        get {
          parameter("input") { input =>
            complete(s"Received input: $input")
            // TODO (Member C): Integrate InputParser to parse input (e.g., "list planets").
            // TODO (Member C): Call Responder to generate responses.
            // TODO (Member C): Handle parse errors (e.g., return 400 for invalid input).
          }
        }
      } ~
        pathEndOrSingleSlash {
          get {
            complete("Welcome to the Astronomy Chatbot! Use /chat?input=your_command")
            // TODO (Member C): Add CSS for starry theme (e.g., serve static resources).
          }
        }

    val bindingFuture = Http().newServerAt("localhost", 8080).bind(route)
    bindingFuture.onComplete {
      case Success(_) =>
        println("Server online at http://localhost:8080/\nPress ENTER to stop...")
      case Failure(e) =>
        println(s"Binding failed: ${e.getMessage}")
        system.terminate()
    }
    // TODO (Member C): Use Config.serverPort instead of hardcoded 8080.
    // TODO (Member C): Add routes for quiz interactions (e.g., /quiz?answer=).
    // TODO (Member C): Test for Render deployment (sbt stage, Procfile).

    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}
