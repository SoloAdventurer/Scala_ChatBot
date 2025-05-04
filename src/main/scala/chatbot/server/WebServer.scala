package chatbot.server

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Directives._
import chatbot.config.Config
import chatbot.parser.InputParser
import chatbot.responder.Responder
import chatbot.quiz.{QuizManager, QuizHandler}
import chatbot.data.AstronomyData
import chatbot.analytics.Analytics
import scala.io.StdIn
import scala.util.{Failure, Success}

object WebServer {
  private var chatHistory: List[(String, String)]                     = List()
  private var currentQuestion: Option[chatbot.quiz.data.QuizQuestion] = None
  private var quizActive: Boolean                                     = false
  private var userName: Option[String]                                = None

  private def addToHistory(query: String, response: String): Unit = {
    chatHistory = (query, response) :: chatHistory.take(4)
  }

  private def sanitizeInput(input: String): String = {
    input
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("\"", "&quot;")
      .replaceAll("'", "&#x27;")
  }

  private def log(message: String): Unit = {
    println(s"[WebServer] $message")
  }

  private def renderHtml(content: String): HttpEntity.Strict = {
    HttpEntity(ContentTypes.`text/html(UTF-8)`, content)
  }

  private val starryThemeCSS = """
    body {
      background-color: #0a0e2a;
      color: #ffffff;
      font-family: 'Montserrat', Arial, sans-serif;
      margin: 0;
      padding: 0;
      background-image: 
        radial-gradient(white, rgba(255,255,255,.2) 2px, transparent 10px),
        radial-gradient(white, rgba(255,255,255,.15) 1px, transparent 5px),
        radial-gradient(white, rgba(255,255,255,.1) 2px, transparent 10px),
        linear-gradient(to right, rgba(15, 32, 93, 0.4), rgba(80, 15, 93, 0.4));
      background-size: 550px 550px, 350px 350px, 250px 250px, 100% 100%;
      background-position: 0 0, 40px 60px, 130px 270px, 0 0;
      background-attachment: fixed;
      position: relative;
      min-height: 100vh;
    }
    body::before {
      content: "";
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: radial-gradient(circle at center, transparent 0%, rgba(10, 14, 42, 0.8) 80%);
      z-index: -1;
    }
    @keyframes floatingPlanet {
      0% { transform: translateY(0) rotate(0deg); }
      50% { transform: translateY(-10px) rotate(5deg); }
      100% { transform: translateY(0) rotate(0deg); }
    }
    .container {
      max-width: 800px;
      margin: 20px auto;
      padding: 25px;
      background-color: rgba(13, 20, 55, 0.8);
      border-radius: 15px;
      box-shadow: 0 0 30px rgba(100, 150, 255, 0.5);
      backdrop-filter: blur(5px);
      position: relative;
      overflow: hidden;
      border: 1px solid rgba(100, 150, 255, 0.2);
    }
    .container::after {
      content: '';
      position: absolute;
      top: -50%;
      left: -50%;
      right: -50%;
      bottom: -50%;
      background: radial-gradient(circle at center, rgba(100, 150, 255, 0.1) 0%, transparent 70%);
      z-index: -1;
    }
    h1 {
      color: #64a0ff;
      text-align: center;
      text-shadow: 0 0 15px rgba(100, 160, 255, 0.8);
      font-size: 2.5em;
      margin-bottom: 20px;
    }
    h2 {
      color: #a09be7;
      border-bottom: 1px solid rgba(100, 150, 255, 0.3);
      padding-bottom: 10px;
      margin-top: 30px;
    }
    input[type="text"] {
      background-color: rgba(13, 20, 55, 0.6);
      border: 1px solid rgba(100, 150, 255, 0.3);
      color: white;
      border-radius: 5px;
      padding: 12px;
      width: calc(100% - 24px);
      margin-bottom: 10px;
      font-family: 'Montserrat', Arial, sans-serif;
    }
    input[type="text"]:focus {
      outline: none;
      border-color: rgba(100, 150, 255, 0.8);
    }
    button {
      transition: all 0.3s ease;
      border-radius: 5px;
      box-shadow: 0 0 15px rgba(61, 90, 254, 0.4);
      font-weight: bold;
      cursor: pointer;
      font-family: 'Montserrat', Arial, sans-serif;
      padding: 12px 20px;
      background-color: #3d5afe;
      color: white;
      border: none;
      margin-right: 10px;
      margin-bottom: 10px;
    }
    button:hover {
      background-color: #536dfe;
      box-shadow: 0 0 15px rgba(83, 109, 254, 0.7);
      transform: translateY(-2px);
    }
    .response-container {
      background-color: rgba(28, 36, 82, 0.6);
      padding: 15px;
      border-radius: 10px;
      margin: 20px 0;
      border-left: 3px solid #64a0ff;
    }
    .query {
      display: inline-block;
      margin-bottom: 10px;
      padding: 5px 10px;
      background-color: rgba(28, 36, 82, 0.8);
      border-radius: 5px;
      border-left: 3px solid #a09be7;
    }
    .history-entry {
      background-color: rgba(28, 36, 82, 0.6);
      padding: 10px;
      border-radius: 8px;
      margin: 10px 0;
      border-left: 3px solid #a09be7;
    }
    .history-entry p {
      margin: 5px 0;
    }
    .analytics-dashboard {
      background-color: rgba(28, 36, 82, 0.6);
      padding: 15px;
      border-radius: 10px;
      margin: 20px 0;
      border-left: 3px solid #a09be7;
    }
    .analytics-dashboard h3 {
      color: #64a0ff;
      margin-bottom: 15px;
    }
    .analytics-dashboard p {
      margin: 5px 0;
      font-size: 1em;
    }
    a {
      color: #64a0ff;
      text-decoration: none;
      transition: all 0.3s ease;
    }
    a:hover {
      color: #a0c8ff;
      text-shadow: 0 0 8px rgba(100, 160, 255, 0.8);
    }
    ul {
      list-style-type: none;
      padding-left: 20px;
    }
    ul li {
      position: relative;
      padding: 5px 0 5px 25px;
    }
    ul li::before {
      content: '✨';
      position: absolute;
      left: 0;
      color: #64a0ff;
    }
    .footer {
      margin-top: 40px;
      text-align: center;
      font-size: 0.9em;
      color: #64a0ff;
      padding: 15px;
      border-top: 1px solid rgba(100, 150, 255, 0.3);
    }
    pre {
      white-space: pre-wrap;
      font-family: 'Fira Code', monospace;
      margin: 0;
    }
    .answer-buttons {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
    }
  """

  def renderPage(
    lastQuery: Option[String] = None,
    response: Option[String] = None,
    isQuizActive: Boolean = false,
    quizSummary: Option[String] = None,
    question: Option[chatbot.quiz.data.QuizQuestion] = None,
    analyticsDashboard: Option[String] = None
  ): String = {
    val greeting = userName match {
      case Some(name) => s"Hello, $name! Welcome to the Astronomy Chatbot!"
      case None       => "Welcome to the Astronomy Chatbot! What's your name?"
    }
    val nameForm = if (userName.isEmpty) {
      s"""
        <form method="POST" action="/set-name">
          <input type="text" name="name" placeholder="Enter your name..." required />
          <button type="submit">Submit Name</button>
        </form>
      """
    } else {
      ""
    }
    val responseSection = response
      .map { msg =>
        val personalizedMsg = userName match {
          case Some(name) => s"$name, $msg"
          case None       => msg
        }
        val sanitizedMsg   = sanitizeInput(personalizedMsg)
        val summarySection = quizSummary.map(summary => s"<p>${sanitizeInput(summary)}</p>").getOrElse("")
        val questionSection = question
          .map { q =>
            if (!msg.contains("Question:") || quizSummary.isDefined) {
              s"""
            <p><strong>Question:</strong> ${sanitizeInput(q.text)}</p>
            <form method="POST" action="/answer-quiz">
              <input type="hidden" name="questionId" value="${q.id}" />
              <div class="answer-buttons">
                ${q.options
                  .map(opt =>
                    s"""<button type="submit" name="answer" value="${sanitizeInput(opt)}">${sanitizeInput(
                        opt
                      )}</button>"""
                  )
                  .mkString}
              </div>
            </form>
          """
            } else {
              ""
            }
          }
          .getOrElse("")
        s"""
        <div class="response-container">
          ${lastQuery.map(q => s"<div class='query'>Your query: ${sanitizeInput(q)}</div>").getOrElse("")}
          <p>$sanitizedMsg</p>
          $questionSection
          $summarySection
          ${if (isQuizActive && question.isDefined && questionSection.nonEmpty) """
            <form method="POST" action="/continue-quiz">
              <button type="submit" name="action" value="next">Next Question</button>
              <button type="submit" name="action" value="skip">Skip</button>
              <button type="submit" name="action" value="end">End Quiz</button>
            </form>
          """ else ""}
        </div>
      """
      }
      .getOrElse("")

    val analyticsSection = analyticsDashboard
      .map { dashboard =>
        // Parse the dashboard string into key-value pairs
        val lines = dashboard.split("\n").filter(_.contains(":")).map(_.trim)
        val stats = lines.map { line =>
          val Array(key, value) = line.split(":").map(_.trim)
          s"<p><strong>$key:</strong> $value</p>"
        }.mkString
        s"""
        <div class="analytics-dashboard">
          <h3>📊 Analytics Dashboard</h3>
          $stats
        </div>
      """
      }
      .getOrElse("")

    s"""
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
            <h1>🔭 Astronomy Chatbot 🪐</h1>
            <p>$greeting</p>
            $nameForm
            <form method="POST" action="/chat">
              <input type="text" name="input" placeholder="Ask something about astronomy..." required />
              <button type="submit">Ask</button>
            </form>
            <p><a href="/history">View Chat History</a> | <a href="/analytics">View Analytics</a></p>
            $responseSection
            $analyticsSection
            <h2>Example queries:</h2>
            <ul>
              <li>Tell me about Mars</li>
              <li>List all planets</li>
              <li>Give me a fact about black holes</li>
              <li>Start an astronomy quiz</li>
            </ul>
            <div class="footer">
              <p>Astronomy Chatbot - Educational Project</p>
            </div>
          </div>
        </body>
      </html>
    """
  }

  def renderHistoryPage(): String = {
    val historySection = if (chatHistory.isEmpty) {
      "<p>No chat history available.</p>"
    } else {
      chatHistory.map { case (query, response) =>
        s"""
          <div class="history-entry">
            <p><strong>Query:</strong> ${sanitizeInput(query)}</p>
            <p><strong>Response:</strong> ${sanitizeInput(response)}</p>
          </div>
        """
      }.mkString
    }

    s"""
      <!DOCTYPE html>
      <html>
        <head>
          <title>Astronomy Chatbot - Chat History</title>
          <style>$starryThemeCSS</style>
        </head>
        <body>
          <div class="container">
            <pre style="color: cyan;">=======================================</pre>
            <pre style="color: yellow;">    CHATURN Astronomy Chatbot</pre>
            <pre style="color: cyan;">=======================================</pre>
            <pre style="color: yellow;">Team:</pre><pre style="color: white;"> Chaturn</pre>
            <pre style="color: yellow;">Members:</pre><pre style="color: white;"> Mohamed, Dania, Maroska, Jana</pre>
            <h1>🔭 Chat History 🪐</h1>
            <p>View your last 5 interactions with the Astronomy Chatbot.</p>
            $historySection
            <p><a href="/">Back to Chat</a></p>
            <div class="footer">
              <p>Astronomy Chatbot - Educational Project</p>
            </div>
          </div>
        </body>
      </html>
    """
  }

  def main(args: Array[String]): Unit = {
    implicit val system           = ActorSystem(Behaviors.empty, "ChatbotSystem")
    implicit val executionContext = system.executionContext

    val config      = Config.load
    val dataSource  = new AstronomyData(config)
    val analytics   = new Analytics()
    val inputParser = new InputParser()
    val quizManager = new QuizManager()
    val responder   = new Responder(dataSource, analytics, quizManager)
    val quizHandler = new QuizHandler()

    val route =
      path("set-name") {
        post {
          formField("name") { name =>
            val sanitizedName = sanitizeInput(name.trim.take(50))
            if (sanitizedName.isEmpty) {
              log("Empty or invalid name provided")
              complete(StatusCodes.BadRequest, renderHtml(renderPage(response = Some("Please provide a valid name."))))
            } else {
              userName = Some(sanitizedName)
              log(s"User name set to: $sanitizedName")
              complete(
                renderHtml(
                  renderPage(response = Some(s"Welcome, $sanitizedName! How can I help you explore the cosmos?"))
                )
              )
            }
          }
        }
      } ~
        path("chat") {
          post {
            formField("input") { input =>
              val sanitizedInput = sanitizeInput(input.trim)
              if (sanitizedInput.isEmpty) {
                log("Empty chat input received")
                complete(
                  StatusCodes.BadRequest,
                  renderHtml(renderPage(response = Some("Please provide a valid input.")))
                )
              } else {
                quizActive = quizHandler.isQuizActive()
                val command = inputParser.parseInput(sanitizedInput, quizActive)
                log(s"Parsed command: $command")

                val response =
                  if (command.startsWith("startquiz") || command.startsWith("answerquiz_") || command == "exit") {
                    if (command == "startquiz") {
                      analytics.logQuizStart() // Log quiz start
                      quizActive = true
                    } else if (command == "exit") {
                      quizActive = false
                    }
                    quizHandler.handleMessage(sanitizedInput)
                  } else {
                    responder.respond(command).getOrElse("message", "No response available.")
                  }

                addToHistory(sanitizedInput, response)
                quizActive = quizHandler.isQuizActive()
                currentQuestion = if (quizActive) quizManager.getCurrentQuestion() else None
                log(s"Quiz active: $quizActive, Current question: ${currentQuestion.map(_.text).getOrElse("None")}")

                complete(
                  renderHtml(renderPage(Some(sanitizedInput), Some(response), quizActive, None, currentQuestion))
                )
              }
            }
          }
        } ~
        path("answer-quiz") {
          post {
            formFields("questionId", "answer") { (questionId, answer) =>
              val sanitizedAnswer = sanitizeInput(answer.trim)
              if (sanitizedAnswer.isEmpty) {
                log("Empty quiz answer received")
                complete(
                  StatusCodes.BadRequest,
                  renderHtml(renderPage(response = Some("Please provide a valid answer.")))
                )
              } else {
                // Assume QuizHandler returns a response indicating correctness (e.g., "Correct!" or "Incorrect...")
                val response  = quizHandler.handleMessage(sanitizedAnswer)
                val isCorrect = response.toLowerCase.contains("correct")
                analytics.logQuizAnswer(isCorrect) // Log quiz answer

                quizActive = quizHandler.isQuizActive()
                currentQuestion = if (quizActive) quizManager.getCurrentQuestion() else None
                addToHistory(s"Quiz answer: $sanitizedAnswer", response)
                log(s"Quiz answer processed: $sanitizedAnswer, Response: $response, Correct: $isCorrect")
                complete(
                  renderHtml(
                    renderPage(Some(s"Answer: $sanitizedAnswer"), Some(response), quizActive, None, currentQuestion)
                  )
                )
              }
            }
          }
        } ~
        path("continue-quiz") {
          post {
            formField("action") { action =>
              if (!quizHandler.isQuizActive()) {
                log("Attempted quiz action without active quiz")
                complete(StatusCodes.BadRequest, renderHtml(renderPage(response = Some("No active quiz to continue."))))
              } else {
                val (message, questionOpt, summaryOpt) = action match {
                  case "next" =>
                    val nextQuestion = quizManager.nextQuestion()
                    (
                      nextQuestion.map(q => s"Next question: ${sanitizeInput(q.text)}").getOrElse("No more questions."),
                      nextQuestion,
                      None
                    )
                  case "skip" =>
                    val nextQuestion = quizManager.nextQuestion()
                    (
                      nextQuestion.map(q => s"Skipped to: ${sanitizeInput(q.text)}").getOrElse("No more questions."),
                      nextQuestion,
                      None
                    )
                  case "end" =>
                    val summary = quizManager.getQuizSummary()
                    quizManager.resetQuiz()
                    quizHandler.resetQuizState()
                    (
                      s"Quiz ended. ${sanitizeInput(summary)}",
                      None,
                      Some(summary)
                    )
                  case _ =>
                    log(s"Unknown quiz action: $action")
                    (
                      "Unknown action.",
                      currentQuestion,
                      None
                    )
                }
                quizActive = quizHandler.isQuizActive()
                currentQuestion = questionOpt
                addToHistory(action, message)
                log(s"Quiz action: $action, Message: $message")
                complete(renderHtml(renderPage(None, Some(message), quizActive, summaryOpt, questionOpt)))
              }
            }
          }
        } ~
        path("history") {
          get {
            log("Rendering chat history")
            complete(renderHtml(renderHistoryPage()))
          }
        } ~
        path("analytics") {
          get {
            log("Rendering analytics dashboard")
            complete(renderHtml(renderPage(analyticsDashboard = Some(analytics.getDashboard))))
          }
        } ~
        pathEndOrSingleSlash {
          get {
            quizActive = quizHandler.isQuizActive()
            log("Rendering main page")
            complete(renderHtml(renderPage()))
          }
        } ~
        path("static" / Remaining) { file =>
          getFromResourceDirectory("static")
        }

    val serverPort    = config.serverPort
    val bindingFuture = Http().newServerAt("0.0.0.0", serverPort).bind(route)

    bindingFuture.onComplete {
      case Success(_) =>
        log(s"Server online at http://localhost:$serverPort/")
        println(s"Server online at http://localhost:$serverPort/\nPress ENTER to stop...")
      case Failure(e) =>
        log(s"Binding failed: ${e.getMessage}")
        system.terminate()
    }

    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}
