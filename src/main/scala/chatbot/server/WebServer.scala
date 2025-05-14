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
import chatbot.quiz.data.QuizQuestion
import chatbot.data.AstronomyData
import chatbot.analytics.Analytics
import scala.io.StdIn
import scala.util.{Failure, Success}

object WebServer {
  private var chatHistory: List[(String, String)]   = List()
  private var currentQuestion: Option[QuizQuestion] = None
  private var quizActive: Boolean                   = false
  private var userName: Option[String]              = None
  private var quizManager: QuizManager              = new QuizManager()
  private val totalQuizQuestions: Int               = 5

  private def addToHistory(query: String, response: String): Unit = {
    chatHistory = (query, response) :: chatHistory.take(4)
  }

  private def sanitizeInput(input: String): String = {
    input.replaceAll("[<>\"&']", "")
  }

  private def log(message: String): Unit = {
    println(s"[WebServer] $message")
  }

  private def renderHtml(content: String): HttpEntity.Strict = {
    HttpEntity(ContentTypes.`text/html(UTF-8)`, content)
  }

  private val starryThemeCSS = """
    body {
      background: #0a0e2a;
      color: #fff;
      font-family: Arial, sans-serif;
      margin: 0;
      padding: 0;
      background-image: radial-gradient(white, rgba(255,255,255,.2) 2px, transparent 10px);
      background-size: 550px 550px;
      min-height: 100vh;
      overflow-x: hidden;
    }
    .container {
      max-width: 800px;
      margin: 20px auto;
      padding: 20px;
      background: rgba(13, 20, 55, 0.8);
      border-radius: 10px;
      box-shadow: 0 0 20px rgba(100, 150, 255, 0.5);
    }
    h1 {
      color: #64a0ff;
      text-align: center;
      font-size: 2em;
    }
    h2 {
      color: #a09be7;
      border-bottom: 1px solid rgba(100, 150, 255, 0.3);
    }
    input[type="text"] {
      background: rgba(13, 20, 55, 0.6);
      border: 1px solid rgba(100, 150, 255, 0.3);
      color: #fff;
      border-radius: 5px;
      padding: 10px;
      width: calc(100% - 20px);
      margin-bottom: 10px;
    }
    button {
      background: #3d5afe;
      color: #fff;
      border: none;
      padding: 10px 15px;
      border-radius: 5px;
      cursor: pointer;
      margin: 8px;
      box-shadow: 0 0 8px rgba(100, 150, 255, 0.5);
      animation: radiate 2s ease-in-out infinite;
    }
    button:hover {
      background: #536dfe;
      box-shadow: 0 0 12px rgba(100, 150, 255, 0.8);
    }
    @keyframes radiate {
      0%, 100% { box-shadow: 0 0 8px rgba(100, 150, 255, 0.5); }
      50% { box-shadow: 0 0 12px rgba(100, 150, 255, 0.8); }
    }
    .response-container {
      background: rgba(28, 36, 82, 0.6);
      padding: 10px;
      border-radius: 5px;
      margin: 15px 0;
      border-left: 3px solid #64a0ff;
    }
    .query {
      background: rgba(28, 36, 82, 0.8);
      padding: 5px 10px;
      border-radius: 5px;
      margin-bottom: 10px;
    }
    .history-entry {
      background: rgba(28, 36, 82, 0.6);
      padding: 10px;
      border-radius: 5px;
      margin: 10px 0;
      border-left: 3px solid #a09be7;
    }
    .analytics-dashboard {
      background: rgba(28, 36, 82, 0.6);
      padding: 10px;
      border-radius: 5px;
      margin: 15px 0;
      border-left: 3px solid #a09be7;
    }
    a {
      color: #64a0ff;
      text-decoration: none;
    }
    a:hover {
      color: #a0c8ff;
    }
    ul {
      list-style: none;
      padding-left: 20px;
    }
    ul li::before {
      content: '✨';
      color: #64a0ff;
      position: absolute;
      left: 0;
    }
    .footer {
      text-align: center;
      color: #64a0ff;
      padding: 10px;
      border-top: 1px solid rgba(100, 150, 255, 0.3);
    }
    .answer-buttons {
      display: flex;
      gap: 15px;
      flex-wrap: wrap;
    }
    .progress-meter {
      background: #1c2452;
      border-radius: 5px;
      margin: 10px 0;
    }
    .progress-bar {
      height: 20px;
      background: #64a0ff;
      color: #fff;
      text-align: center;
      transition: width 0.3s;
    }
    .quiz-container {
      background: rgba(28, 36, 82, 0.8);
      padding: 10px;
      border-radius: 5px;
      margin-top: 10px;
      border-left: 3px solid #a09be7;
    }
    .shooting-star {
      position: fixed;
      width: 2px;
      height: 2px;
      background: #fff;
      border-radius: 50%;
      box-shadow: 0 0 10px #fff;
      opacity: 0;
      z-index: -1;
    }
    .twinkle-star {
      position: fixed;
      width: 2px;
      height: 2px;
      background: #fff;
      border-radius: 50%;
      z-index: -1;
    }
    .audio-player {
      position: fixed;
      bottom: 10px;
      right: 10px;
      opacity: 0.8;
    }
    @keyframes twinkle {
      0%, 100% { opacity: 0.2; }
      50% { opacity: 1; }
    }
  """

  private val animationScript = """
    <script>
      function createStars() {
        const body = document.body;
        for (let i = 0; i < 15; i++) {
          const star = document.createElement('div');
          star.className = 'shooting-star';
          body.appendChild(star);
          animateShootingStar(star);
          setInterval(() => animateShootingStar(star), 10000 + Math.random() * 10000);
        }
        for (let i = 0; i < 150; i++) {
          const star = document.createElement('div');
          star.className = 'twinkle-star';
          star.style.left = Math.random() * 100 + '%';
          star.style.top = Math.random() * 100 + '%';
          star.style.width = (Math.random() * 2 + 1) + 'px';
          star.style.height = star.style.width;
          star.style.animation = `twinkle ${Math.random() * 3 + 2}s infinite`;
          star.style.animationDelay = Math.random() * 5 + 's';
          body.appendChild(star);
        }
      }
      function animateShootingStar(star) {
        const startX = Math.random() * window.innerWidth;
        const startY = -10;
        const angle = Math.random() * 30 + 30;
        const distance = window.innerHeight * 1.5;
        const endX = startX + distance * Math.cos(angle * Math.PI / 180);
        const endY = startY + distance * Math.sin(angle * Math.PI / 180);
        star.style.left = startX + 'px';
        star.style.top = startY + 'px';
        star.style.opacity = '0';
        star.animate([
          { left: startX + 'px', top: startY + 'px', opacity: 0 },
          { opacity: 1, offset: 0.1 },
          { left: endX + 'px', top: endY + 'px', opacity: 0 }
        ], {
          duration: (Math.random() * 1 + 0.5) * 1000,
          easing: 'linear',
          fill: 'forwards'
        });
      }
      window.addEventListener('load', createStars);
    </script>
  """

  def renderPage(
    lastQuery: Option[String] = None,
    response: Option[String] = None,
    isQuizActive: Boolean = false,
    quizSummary: Option[String] = None,
    question: Option[QuizQuestion] = None,
    analyticsDashboard: Option[String] = None
  ): String = {
    val greeting = userName.map(n => s"Hello, $n!").getOrElse("What's your name?")
    val nameForm = if (userName.isEmpty) {
      """<form method="POST" action="/set-name">
           <input type="text" name="name" placeholder="Enter name" required />
           <button type="submit">Submit</button>
         </form>"""
    } else {
      ""
    }

    val quizTopicSection = response.exists(_.toLowerCase.contains("quiz topics")) match {
      case true =>
        """<div class="quiz-container">
             <h3>Quiz Topics:</h3>
             <form method="POST" action="/start-quiz">
               <button type="submit" name="topic" value="traditional">Traditional</button>
               <button type="submit" name="topic" value="personal">Personal</button>
             </form>
           </div>"""
      case false => ""
    }

    val responseSection = response
      .map { msg =>
        val personalizedMsg = userName.map(n => s"$n, $msg").getOrElse(msg)
        val sanitizedMsg    = sanitizeInput(personalizedMsg)
        val summarySection  = quizSummary.map(s => s"<p>${sanitizeInput(s)}</p>").getOrElse("")

        val questionSection = if (isQuizActive && question.isDefined) {
          question
            .map { q =>
              val idx      = quizManager.getCurrentQuestionIndex() + 1
              val total    = quizManager.getTotalQuestions()
              val progress = if (total > 0) (idx.toDouble / total * 100).toInt else 0
              s"""<div class="quiz-container">
                <p><strong>Question:</strong> ${sanitizeInput(q.text)}</p>
                <div class="progress-meter">
                  <div class="progress-bar" style="width: ${progress}%">${idx} / ${total}</div>
                </div>
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
                <form method="POST" action="/continue-quiz">
                  <button type="submit" name="action" value="skip">Skip</button>
                  <button type="submit" name="action" value="end">End</button>
                </form>
              </div>"""
            }
            .getOrElse("")
        } else {
          ""
        }

        s"""<div class="response-container">
            ${lastQuery.map(q => s"<div class='query'>Query: ${sanitizeInput(q)}</div>").getOrElse("")}
            <p>$sanitizedMsg</p>
            $questionSection
            $summarySection
            $quizTopicSection
          </div>"""
      }
      .getOrElse("")

    val analyticsSection = analyticsDashboard
      .map { d =>
        val stats = d
          .split("\n")
          .filter(_.contains(":"))
          .map { l =>
            val Array(k, v) = l.split(":").map(_.trim)
            s"<p><strong>$k:</strong> $v</p>"
          }
          .mkString
        s"""<div class="analytics-dashboard">
            <h3>Analytics</h3>
            $stats
          </div>"""
      }
      .getOrElse("")

    s"""<!DOCTYPE html>
        <html>
          <head>
            <title>Astronomy Chatbot</title>
            <style>$starryThemeCSS</style>
            $animationScript
          </head>
          <body>
            <audio class="audio-player" autoplay loop controls volume="0.3">
              <source src="/music/pirate_moozik.mp3" type="audio/mpeg">
              <!-- Replace 'path/to/your/audio.mp3' with the path to your audio file (e.g., 'resources/music/pirate_moozik.mp3') -->
              Your browser does not support the audio element.
            </audio>
            <div class="container">
              <h1>🔭 Astronomy Chatbot 🪐</h1>
              <p>$greeting</p>
              $nameForm
              <form method="POST" action="/chat">
                <input type="text" name="input" placeholder="Ask about astronomy" required />
                <button type="submit">Ask</button>
              </form>
              <p><a href="/history">History</a> | <a href="/analytics">Analytics</a></p>
              $responseSection
              $analyticsSection
              <h2>Try these:</h2>
              <ul>
                <li>Tell me about Mars</li>
                <li>Compare Earth and Venus</li>
                <li>How big is Earth?</li>
                <li>Start quiz</li>
              </ul>
              <div class="footer">
                <p>Astronomy Chatbot</p>
              </div>
            </div>
          </body>
        </html>"""
  }

  def renderHistoryPage(): String = {
    val historySection = if (chatHistory.isEmpty) {
      "<p>No history.</p>"
    } else {
      chatHistory.map { case (q, r) =>
        s"""<div class="history-entry">
              <p><strong>Query:</strong> ${sanitizeInput(q)}</p>
              <p><strong>Response:</strong> ${sanitizeInput(r)}</p>
            </div>"""
      }.mkString
    }

    s"""<!DOCTYPE html>
        <html>
          <head>
            <title>Chat History</title>
            <style>$starryThemeCSS</style>
            $animationScript
          </head>
          <body>
            <audio class="audio-player" autoplay loop controls volume="0.3">
              <source src="/music/pirate_moozik.mp3" type="audio/mpeg">
              <!-- Replace 'path/to/your/audio.mp3' with the path to your audio file (e.g., 'resources/music/space-ambient.mp3') -->
              Your browser does not support the audio element.
            </audio>
            <div class="container">
              <h1>🔭 Chat History 🪐</h1>
              <p>Last 5 interactions.</p>
              $historySection
              <p><a href="/">Back</a></p>
              <div class="footer">
                <p>Astronomy Chatbot</p>
              </div>
            </div>
          </body>
        </html>"""
  }

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem(Behaviors.empty, "ChatbotSystem")
    implicit val ec     = system.executionContext

    val config     = Config.load
    val dataSource = new AstronomyData(config)
    val analytics  = new Analytics()
    val parser     = new InputParser()
    quizManager = new QuizManager()
    val responder   = new Responder(dataSource, analytics, quizManager)
    val quizHandler = new QuizHandler()

    val route =
      pathPrefix("music") {
        getFromResourceDirectory("music")
      } ~
        path("set-name") {
          post {
            formField("name") { name =>
              val sanitizedName = sanitizeInput(name.trim.take(50))
              if (sanitizedName.isEmpty) {
                log("Invalid name")
                complete(StatusCodes.BadRequest, renderHtml(renderPage(response = Some("Invalid name."))))
              } else {
                userName = Some(sanitizedName)
                log(s"Name set: $sanitizedName")
                complete(renderHtml(renderPage(response = Some(s"Welcome, $sanitizedName!"))))
              }
            }
          }
        } ~
        path("chat") {
          post {
            formField("input") { input =>
              val sanitizedInput = sanitizeInput(input.trim)
              if (sanitizedInput.isEmpty) {
                log("Empty input")
                complete(StatusCodes.BadRequest, renderHtml(renderPage(response = Some("Invalid input."))))
              } else {
                quizActive = quizHandler.isQuizActive()
                val command = parser.parseInput(sanitizedInput, quizActive)
                log(s"Command: $command")

                val response = if (command.startsWith("startquiz") || command == "exit") {
                  if (command == "startquiz") {
                    if (sanitizedInput.toLowerCase.contains("traditional")) {
                      quizActive = true
                      quizManager.startQuiz("traditional", totalQuizQuestions)
                      "Starting traditional quiz!"
                    } else if (sanitizedInput.toLowerCase.contains("personal")) {
                      quizActive = true
                      quizManager.startQuiz("personal", totalQuizQuestions)
                      "Starting personal quiz!"
                    } else {
                      "Quiz topics:\n- Traditional\n- Personal"
                    }
                  } else {
                    quizActive = false
                    quizHandler.resetQuizState()
                    "Quiz ended."
                  }
                } else {
                  responder.respond(command).getOrElse("message", "No response.")
                }

                addToHistory(sanitizedInput, response)
                quizActive = quizHandler.isQuizActive() || sanitizedInput.toLowerCase.contains("quiz")
                currentQuestion = if (quizActive) quizManager.getCurrentQuestion() else None
                log(s"Quiz: $quizActive, Question: ${currentQuestion.map(_.text).getOrElse("None")}")

                complete(
                  renderHtml(renderPage(Some(sanitizedInput), Some(response), quizActive, None, currentQuestion))
                )
              }
            }
          }
        } ~
        path("start-quiz") {
          post {
            formField("topic") { topic =>
              quizHandler.resetQuizState()
              quizActive = true
              val question = quizManager.startQuiz(topic, totalQuizQuestions)
              val response =
                question.map(q => s"Starting ${topic} Quiz! Question: ${q.text}").getOrElse(s"No ${topic} quiz.")
              currentQuestion = question
              addToHistory(s"Start ${topic} quiz", response)
              log(s"Quiz started: $topic")
              complete(
                renderHtml(renderPage(Some(s"Start ${topic} quiz"), Some(response), quizActive, None, currentQuestion))
              )
            }
          }
        } ~
        path("answer-quiz") {
          post {
            formFields("questionId", "answer") { (questionId, answer) =>
              val sanitizedAnswer = sanitizeInput(answer.trim)
              if (sanitizedAnswer.isEmpty) {
                log("Empty answer")
                complete(StatusCodes.BadRequest, renderHtml(renderPage(response = Some("Invalid answer."))))
              } else {
                val response  = quizManager.answerCurrentQuestion(sanitizedAnswer).getOrElse("No response.")
                val isCorrect = response.toLowerCase.contains("correct")
                analytics.logInteraction(s"answerquiz_$sanitizedAnswer", isCorrect)

                val nextQuestion = quizManager.nextQuestion()
                quizActive = nextQuestion.isDefined
                currentQuestion = nextQuestion

                val fullResponse = nextQuestion.map(q => s"$response\n\nNext: ${q.text}").getOrElse {
                  val summary = quizManager.getQuizSummary()
                  quizManager.resetQuiz()
                  quizHandler.resetQuizState()
                  s"$response\n\n$summary"
                }

                addToHistory(s"Answer: $sanitizedAnswer", fullResponse)
                log(s"Answer: $sanitizedAnswer, Correct: $isCorrect, Next: ${nextQuestion.isDefined}")

                complete(
                  renderHtml(
                    renderPage(Some(s"Answer: $sanitizedAnswer"), Some(fullResponse), quizActive, None, currentQuestion)
                  )
                )
              }
            }
          }
        } ~
        path("continue-quiz") {
          post {
            formField("action") { action =>
              if (!quizActive) {
                log("No active quiz")
                complete(StatusCodes.BadRequest, renderHtml(renderPage(response = Some("No quiz."))))
              } else {
                val (message, questionOpt, summaryOpt) = action match {
                  case "skip" =>
                    val next = quizManager.nextQuestion()
                    quizActive = next.isDefined
                    currentQuestion = next
                    (
                      next.map(q => s"Skipped: ${q.text}").getOrElse("Quiz done!"),
                      next,
                      if (next.isEmpty) Some(quizManager.getQuizSummary()) else None
                    )
                  case "end" =>
                    val summary = quizManager.getQuizSummary()
                    quizManager.resetQuiz()
                    quizHandler.resetQuizState()
                    quizActive = false
                    currentQuestion = None
                    (s"Quiz ended. $summary", None, Some(summary))
                  case _ =>
                    log(s"Unknown action: $action")
                    ("Unknown action.", currentQuestion, None)
                }

                addToHistory(action, message)
                log(s"Action: $action, Quiz: $quizActive")

                complete(renderHtml(renderPage(None, Some(message), quizActive, summaryOpt, questionOpt)))
              }
            }
          }
        } ~
        path("history") {
          get {
            log("Rendering history")
            complete(renderHtml(renderHistoryPage()))
          }
        } ~
        path("analytics") {
          get {
            log("Rendering analytics")
            complete(renderHtml(renderPage(analyticsDashboard = Some(analytics.getDashboard))))
          }
        } ~
        pathEndOrSingleSlash {
          get {
            quizActive = quizHandler.isQuizActive()
            log("Rendering main")
            complete(renderHtml(renderPage()))
          }
        }

    val serverPort    = config.serverPort
    val bindingFuture = Http().newServerAt("0.0.0.0", serverPort).bind(route)

    bindingFuture.onComplete {
      case Success(_) =>
        log(s"Server at http://localhost:$serverPort/")
        println(s"Server at http://localhost:$serverPort/\nPress ENTER to stop...")
      case Failure(e) =>
        log(s"Failed: ${e.getMessage}")
        system.terminate()
    }

    StdIn.readLine()
    bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
  }
}
