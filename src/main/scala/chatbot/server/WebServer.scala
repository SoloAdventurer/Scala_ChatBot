package chatbot.server

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Directives._
import chatbot.config.Config
import chatbot.parser.InputParser
import chatbot.parser.AST.Command
import chatbot.responder.Responder
import chatbot.quiz.QuizManager
import chatbot.data.{AstronomyData, PlanetApiClient}
import chatbot.analytics.Analytics
import scala.io.StdIn
import scala.util.{Failure, Success}

object WebServer {
  def main(args: Array[String]): Unit = {
    implicit val system           = ActorSystem(Behaviors.empty, "ChatbotSystem")
    implicit val executionContext = system.executionContext

    val config      = Config.load
    val dataSource  = new AstronomyData(config)
    val apiClient   = new PlanetApiClient()
    val analytics   = new Analytics()
    val inputParser = new InputParser()
    val responder   = new Responder(dataSource, apiClient, analytics)
    val quizManager = new QuizManager()

    // CSS for starry theme
    val starryThemeCSS = """
      body {
        background-color: #0a0e2a;
        color: #ffffff;
        font-family: Arial, sans-serif;
        margin: 0;
        padding: 20px;
        background-image: 
          radial-gradient(white, rgba(255,255,255,.2) 2px, transparent 10px),
          radial-gradient(white, rgba(255,255,255,.15) 1px, transparent 5px),
          radial-gradient(white, rgba(255,255,255,.1) 2px, transparent 10px);
        background-size: 550px 550px, 350px 350px, 250px 250px;
        background-position: 0 0, 40px 60px, 130px 270px;
      }
      .container {
        max-width: 800px;
        margin: 0 auto;
        padding: 20px;
        background-color: rgba(13, 20, 55, 0.8);
        border-radius: 10px;
        box-shadow: 0 0 20px rgba(100, 150, 255, 0.5);
      }
      h1 {
        color: #64a0ff;
        text-align: center;
      }
      a {
        color: #64a0ff;
      }
      code {
        background-color: #1c2452;
        padding: 2px 5px;
        border-radius: 3px;
      }
      .footer {
        margin-top: 40px;
        text-align: center;
        font-size: 0.9em;
        color: #64a0ff;
      }
      pre {
        white-space: pre-wrap;
        font-family: monospace;
      }
    """

    val welcomeHTML = s"""
      <!DOCTYPE html>
      <html>
        <head>
          <title>Astronomy Chatbot</title>
          <style>$starryThemeCSS</style>
        </head>
        <body>
          <div class="container">
            <pre style="color: cyan;">=======================================</pre>
            <pre style="color: yellow;">    CHATURN Astronomy Chatbot</pre>
            <pre style="color: cyan;">=======================================</pre>
            <pre style="color: yellow;">Team:</pre><pre style="color: white;"> Chaturn</pre>
            <pre style="color: yellow;">Members:</pre><pre style="color: white;"> Mohamed, Dania, Maroska, Jana</pre>
            <pre style="color: cyan;">
                                                                    ..;===+.
                                                                .:=iiiiii=+=
                                                             .=i))=;::+)i=+,
                                                          ,=i);)I)))I):=i=;
                                                       .=i==))))ii)))I:i++
                                                     +)+))iiiiiiii))I=i+:''
                                .,:;;++++++;:,.       )iii+:::;iii))+i='
                             .:;++=iiiiiiiiii=++;.    =::,,,:::=i));=+''
                           ,;+==ii)))))))))))ii==+;,      ,,,:=i))+=:
                         ,;+=ii))))))IIIIII))))ii===;.    ,,:=i)=i+
                        ;+=ii)))IIIIITIIIIII))))iiii=+,   ,:=));=,
                      ,+=i))IIIIIITTTTTITIIIIII)))I)i=+,,:+i)=i+
                     ,+i))IIIIIITTTTTTTTTTTTI))IIII))i=::i))i='
                    ,=i))IIIIITLLTTTTTTTTTTIITTTTIII)+;+i)+i`
                    =i))IIITTLTLTTTTTTTTTIITTLLTTTII+:i)ii:''
                   +i))IITTTLLLTTTTTTTTTTTTLLLTTTT+:i)))=,
                   =))ITTTTTTTTTTTLTTTTTTLLLLLLTi:=)IIiii;
                  .i)IIITTTTTTTTLTTTITLLLLLLLT);=)I)))))i;
                  :))IIITTTTTLTTTTTTLLHLLLLL);=)II)IIIIi=:
                  :i)IIITTTTTTTTTLLLHLLHLL)+=)II)ITTTI)i=
                  .i)IIITTTTITTLLLHHLLLL);=)II)ITTTTII)i+
                  =i)IIIIIITTLLLLLLHLL=:i)II)TTTTTTIII)i''
                +i)i)))IITTLLLLLLLLT=:i)II)TTTTLTTIII)i;
              +ii)i:)IITTLLTLLLLT=;+i)I)ITTTTLTTTII))i;
             =;)i=:,=)ITTTTLTTI=:i))I)TTTLLLTTTTTII)i;
           +i)ii::,  +)IIITI+:+i)I))TTTTLLTTTTTII))=,
         :=;)i=:,,    ,i++::i))I)ITTTTTTTTTTIIII)=+''
       .+ii)i=::,,   ,,::=i)))iIITTTTTTTTIIIII)=+
      ,==)ii=;:,,,,:::=ii)i)iIIIITIIITIIII))i+:'
     +=:))i==;:::;=iii)+)=  `:i)))IIIII)ii+'
   .+=:))iiiiiiii)))+ii;
  .+=;))iiiiii)));ii+
 .+=i:)))))))=+ii+
.;==i+::::=)i=;
,+==iiiiii+,
`+=+++;`
            </pre>
            <h1>🔭 Astronomy Chatbot 🪐</h1>
            <p>Welcome to the Astronomy Chatbot! Ask questions about planets, stars, galaxies, and more.</p>
            
            <h2>How to use:</h2>
            <ul>
              <li>Ask a question: <code>/chat?input=ask about Mars</code></li>
              <li>List planets: <code>/chat?input=list planets</code></li>
              <li>Get facts: <code>/chat?input=fact about black holes</code></li>
              <li>Take a quiz: <code>/quiz?start=true</code></li>
            </ul>
            
            <h2>Try a query:</h2>
            <form action="/chat" method="get">
              <input type="text" name="input" placeholder="Ask something about astronomy..." style="width: 80%; padding: 8px;">
              <button type="submit" style="padding: 8px 15px; background-color: #3d5afe; color: white; border: none; border-radius: 4px;">Ask</button>
            </form>
            
            <div class="footer">
              <p>Astronomy Chatbot - Educational Project</p>
            </div>
          </div>
        </body>
      </html>
    """

    val route =
      path("chat") {
        get {
          parameter("input") { input =>
            inputParser.parseInput(input) match {
              case Right(command) =>
                analytics.logInteraction(command)
                val response = responder.respond(command).getOrElse("message", "No response available.")
                complete(
                  HttpEntity(
                    ContentTypes.`text/html(UTF-8)`,
                    s"""
                    <!DOCTYPE html>
                    <html>
                      <head>
                        <title>Astronomy Chatbot</title>
                        <style>$starryThemeCSS</style>
                      </head>
                      <body>
                        <div class="container">
                          <h1>🔭 Astronomy Chatbot 🪐</h1>
                          <p><strong>Your query:</strong> $input</p>
                          <p><strong>Response:</strong> $response</p>
                          <p><a href="/">Back to home</a></p>
                        </div>
                      </body>
                    </html>
                    """
                  )
                )
              case Left(error) =>
                complete(
                  StatusCodes.BadRequest,
                  HttpEntity(
                    ContentTypes.`text/html(UTF-8)`,
                    s"""
                      <!DOCTYPE html>
                      <html>
                        <head>
                          <title>Astronomy Chatbot - Error</title>
                          <style>$starryThemeCSS</style>
                        </head>
                        <body>
                          <div class="container">
                            <h1>🔭 Astronomy Chatbot 🪐</h1>
                            <p>Error: $error</p>
                            <p>Please try again with a valid query.</p>
                            <p><a href="/">Back to home</a></p>
                          </div>
                        </body>
                      </html>
                    """
                  )
                )
            }
          }
        }
      } ~
        path("quiz") {
          get {
            parameter("start".as[Boolean].optional) { startQuiz =>
              if (startQuiz.getOrElse(false)) {
                val question = quizManager.startQuiz()
                complete(
                  HttpEntity(
                    ContentTypes.`text/html(UTF-8)`,
                    s"""
                  <!DOCTYPE html>
                  <html>
                    <head>
                      <title>Astronomy Quiz</title>
                      <style>$starryThemeCSS</style>
                    </head>
                    <body>
                      <div class="container">
                        <h1>🔭 Astronomy Quiz 🪐</h1>
                        <p>${question.text}</p>
                        <form action="/quiz" method="get">
                          <input type="hidden" name="questionId" value="${question.id}">
                          <input type="text" name="answer" placeholder="Type your answer..." style="width: 80%; padding: 8px;">
                          <button type="submit" style="padding: 8px 15px; background-color: #3d5afe; color: white; border: none; border-radius: 4px;">Submit</button>
                        </form>
                        <p><a href="/">Back to home</a></p>
                      </div>
                    </body>
                  </html>
                  """
                  )
                )
              } else {
                parameters("questionId", "answer") { (questionId, answer) =>
                  val result = quizManager.checkAnswer(questionId, answer)
                  complete(
                    HttpEntity(
                      ContentTypes.`text/html(UTF-8)`,
                      s"""
                    <!DOCTYPE html>
                    <html>
                      <head>
                        <title>Astronomy Quiz</title>
                        <style>$starryThemeCSS</style>
                      </head>
                      <body>
                        <div class="container">
                          <h1>🔭 Astronomy Quiz 🪐</h1>
                          <p>${result.feedback}</p>
                          <p>${if (result.correct) "Correct!" else "Incorrect. Try again!"}</p>
                          <p><a href="/quiz?start=true">Next question</a></p>
                          <p><a href="/">Back to home</a></p>
                        </div>
                      </body>
                    </html>
                    """
                    )
                  )
                }
              }
            }
          }
        } ~
        pathEndOrSingleSlash {
          get {
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, welcomeHTML))
          }
        } ~
        path("static" / Remaining) { file =>
          getFromResourceDirectory("static")
        }

    val serverPort    = config.serverPort
    val bindingFuture = Http().newServerAt("0.0.0.0", serverPort).bind(route)

    bindingFuture.onComplete {
      case Success(_) =>
        println(s"Server online at http://localhost:$serverPort/\nPress ENTER to stop...")
      case Failure(e) =>
        println(s"Binding failed: ${e.getMessage}")
        system.terminate()
    }

    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}
