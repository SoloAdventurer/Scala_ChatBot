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
    @keyframes twinkling {
      0% { opacity: 0.3; }
      50% { opacity: 1; }
      100% { opacity: 0.3; }
    }
    @keyframes shooting-star {
      0% { transform: translateX(0) translateY(0) rotate(45deg); opacity: 0; }
      10% { opacity: 1; }
      40% { opacity: 1; }
      60% { opacity: 0; }
      100% { transform: translateX(300px) translateY(300px) rotate(45deg); opacity: 0; }
    }
    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(10px); }
      to { opacity: 1; transform: translateY(0); }
    }
    @keyframes glow {
      0% { box-shadow: 0 0 5px rgba(100, 160, 255, 0.6); }
      50% { box-shadow: 0 0 15px rgba(100, 160, 255, 0.9), 0 0 25px rgba(100, 160, 255, 0.6); }
      100% { box-shadow: 0 0 5px rgba(100, 160, 255, 0.6); }
    }
    @keyframes dots {
      0% { content: '.'; }
      33% { content: '..'; }
      66% { content: '...'; }
      100% { content: '.'; }
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
      animation: glow 3s ease-in-out infinite;
    }
    h2 {
      color: #a09be7;
      border-bottom: 1px solid rgba(100, 150, 255, 0.3);
      padding-bottom: 10px;
      margin-top: 30px;
    }
    a {
      color: #64a0ff;
      text-decoration: none;
      transition: all 0.3s ease;
      position: relative;
    }
    a:hover {
      color: #a0c8ff;
      text-shadow: 0 0 8px rgba(100, 160, 255, 0.8);
    }
    a::after {
      content: '';
      position: absolute;
      width: 0;
      height: 1px;
      bottom: -2px;
      left: 0;
      background-color: #a0c8ff;
      transition: width 0.3s ease;
    }
    a:hover::after {
      width: 100%;
    }
    code {
      background-color: #1c2452;
      padding: 4px 8px;
      border-radius: 4px;
      box-shadow: 0 0 5px rgba(28, 36, 82, 0.5);
      font-family: 'Fira Code', monospace;
      letter-spacing: -0.5px;
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
    .chat-input {
      display: flex;
      gap: 10px;
      margin-top: 20px;
    }
    input[type="text"] {
      background-color: rgba(13, 20, 55, 0.6);
      border: 1px solid rgba(100, 150, 255, 0.3);
      color: white;
      border-radius: 5px;
      padding: 12px;
      flex: 1;
      transition: all 0.3s ease;
      font-family: 'Montserrat', Arial, sans-serif;
    }
    input[type="text"]:focus {
      outline: none;
      border-color: rgba(100, 150, 255, 0.8);
      animation: glow 2s ease-in-out infinite;
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
    }
    button:hover {
      background-color: #536dfe;
      box-shadow: 0 0 15px rgba(83, 109, 254, 0.7);
      transform: translateY(-2px);
    }
    .shooting-star {
      position: absolute;
      width: 100px;
      height: 2px;
      background: linear-gradient(to right, transparent, #fff, transparent);
      animation: shooting-star 3s linear infinite;
      top: 20%;
      left: 10%;
    }
    .shooting-star:nth-child(2) {
      top: 60%;
      left: 80%;
      animation-delay: 1.5s;
    }
    .planet {
      position: absolute;
      opacity: 0.2;
      z-index: -1;
      animation: floatingPlanet 8s ease-in-out infinite;
    }
    .star {
      position: absolute;
      width: 2px;
      height: 2px;
      background: white;
      border-radius: 50%;
      animation: twinkling 3s ease-in-out infinite;
    }
    .chat-messages {
      margin-top: 20px;
      padding: 10px;
      border-radius: 8px;
      min-height: 100px;
      max-height: 400px;
      overflow-y: auto;
    }
    .message {
      margin: 10px 0;
      padding: 10px 15px;
      border-radius: 8px;
      max-width: 80%;
      animation: fadeIn 0.5s ease-in;
    }
    .user-message {
      background-color: #3d5afe;
      margin-left: auto;
      color: white;
    }
    .bot-message {
      background-color: #1c2452;
      margin-right: auto;
      color: #e0e7ff;
    }
    .loading {
      display: none;
      color: #64a0ff;
      padding: 10px;
    }
    .loading.active {
      display: block;
    }
    .loading::after {
      content: '...';
      animation: dots 1.5s steps(3, end) infinite;
    }
    .response-container {
      background-color: rgba(28, 36, 82, 0.6);
      padding: 15px;
      border-radius: 10px;
      margin: 20px 0;
      border-left: 3px solid #64a0ff;
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
    .query {
      display: inline-block;
      margin-bottom: 10px;
      padding: 5px 10px;
      background-color: rgba(28, 36, 82, 0.8);
      border-radius: 5px;
      border-left: 3px solid #a09be7;
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

    val interactiveJS = """
    document.addEventListener('DOMContentLoaded', function() {
      const container = document.querySelector('body');
      for (let i = 0; i < 50; i++) {
        const star = document.createElement('div');
        star.classList.add('star');
        star.style.left = Math.random() * 100 + 'vw';
        star.style.top = Math.random() * 100 + 'vh';
        star.style.animationDelay = Math.random() * 3 + 's';
        container.appendChild(star);
      }
      const shootingStar1 = document.createElement('div');
      shootingStar1.classList.add('shooting-star');
      container.appendChild(shootingStar1);
      const shootingStar2 = document.createElement('div');
      shootingStar2.classList.add('shooting-star');
      container.appendChild(shootingStar2);
      const planet = document.createElement('div');
      planet.classList.add('planet');
      planet.style.right = '-50px';
      planet.style.top = '100px';
      planet.style.width = '200px';
      planet.style.height = '200px';
      planet.style.borderRadius = '50%';
      planet.style.background = 'radial-gradient(circle at 40% 40%, rgba(100, 160, 255, 0.3), rgba(10, 14, 42, 0) 70%)';
      container.appendChild(planet);

      const input = document.getElementById('chat-input');
      const sendButton = document.getElementById('send-button');
      const messagesDiv = document.getElementById('chat-messages');
      const loading = document.getElementById('loading');
      let currentQuestionId = null;

      function addMessage(text, isUser) {
        const message = document.createElement('div');
        message.className = `message ${isUser ? 'user-message' : 'bot-message'}`;
        message.textContent = text;
        messagesDiv.appendChild(message);
        messagesDiv.scrollTop = messagesDiv.scrollHeight;
      }

      async function sendMessage() {
        const message = input.value.trim();
        if (!message) return;

        addMessage(message, true);
        input.value = '';
        loading.classList.add('active');

        try {
          if (message.toLowerCase().includes('quiz') || currentQuestionId) {
            if (currentQuestionId) {
              const response = await fetch(`/api/quiz?questionId=${encodeURIComponent(currentQuestionId)}&answer=${encodeURIComponent(message)}`);
              const data = await response.json();
              addMessage(`${data.feedback} ${data.correct ? 'Correct!' : 'Incorrect.'}`, false);
              currentQuestionId = null;
            }
            const response = await fetch('/api/quiz?start=true');
            const data = await response.json();
            addMessage(`${data.text} Options: ${data.options.join(', ')}`, false);
            currentQuestionId = data.id;
          } else {
            const response = await fetch(`/api/chat?input=${encodeURIComponent(message)}`);
            const data = await response.json();
            addMessage(data.response || data.error || 'Something went wrong.', false);
          }
        } catch (error) {
          addMessage('Error connecting to the cosmos. Try again!', false);
        } finally {
          loading.classList.remove('active');
          messagesDiv.scrollTop = messagesDiv.scrollHeight;
        }
      }

      input.addEventListener('keypress', (e) => {
        if (e.key === 'Enter') sendMessage();
      });

      sendButton.addEventListener('click', sendMessage);
    });
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
