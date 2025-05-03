package chatbot.main

import chatbot.parser.InputParser
import chatbot.parser.AST.Command
import chatbot.parser.AST.Command._
import chatbot.responder.Responder
import chatbot.data.{AstronomyData, PlanetApiClient}
import chatbot.analytics.Analytics
import chatbot.config.Config
import chatbot.quiz.QuizManager
import chatbot.quiz.data.QuizQuestion
import scala.io.StdIn
import chatbot.quiz.QuizHandler

object Main {
  private val Cyan   = "\u001b[36m"
  private val Yellow = "\u001b[33m"
  private val Green  = "\u001b[32m"
  private val Red    = "\u001b[31m"
  private val Reset  = "\u001b[0m"
  private val Blink  = "\u001b[5m"

  def greetUser(name: String): String = {
    s"Hello, $name! I'm CHATURN, your astronomy chatbot. Ask me about planets, stars, or try 'help' for commands!"
  }

  def main(args: Array[String]): Unit = {
    val config      = Config.load
    val dataSource  = new AstronomyData(config)
    val apiClient   = new PlanetApiClient()
    val analytics   = new Analytics()
    val quizManager = new QuizManager()
    val responder   = new Responder(dataSource, apiClient, analytics, quizManager)
    val parser      = new InputParser()
    val quizHandler = new QuizHandler()

    print(s"${Green}Please enter your name: ${Reset}")
    val userName = StdIn.readLine().trim.take(50) match {
      case name if name.nonEmpty => name
      case _                     => "Space Explorer"
    }

    println(s"""
               |${Cyan}=======================================${Reset}
               |${Yellow}${Blink}    CHATURN Astronomy Chatbot${Reset}
               |${Cyan}=======================================${Reset}
               |${Yellow}Team:${Reset} Chaturn
               |${Yellow}Members:${Reset} Mohamed, Dania, Maroska, Jana
               |${Cyan}
               |                                                                    ..;===+.
               |                                                                .:=iiiiii=+=
               |                                                             .=i))=;::+)i=+,
               |                                                          ,=i);)I)))I):=i=;
               |                                                       .=i==))))ii)))I:i++
               |                                                     +)+))iiiiiiii))I=i+:''
               |                                .,:;;++++++;:,.       )iii+:::;iii))+i='
               |                             .:;++=iiiiiiiiii=++;.    =::,,,:::=i));=+''
               |                           ,;+==ii)))))))))))ii==+;,      ,,,:=i))+=:
               |                         ,;+=ii))))))IIIIII))))ii===;.    ,,:=i)=i+
               |                        ;+=ii)))IIIIITIIIIII))))iiii=+,   ,:=));=,
               |                      ,+=i))IIIIIITTTTTITIIIIII)))I)i=+,,:+i)=i+
               |                     ,+i))IIIIIITTTTTTTTTTTTI))IIII))i=::i))i='
               |                    ,=i))IIIIITLLTTTTTTTTTTIITTTTIII)+;+i)+i`
               |                    =i))IIITTLTLTTTTTTTTTIITTLLTTTII+:i)ii:''
               |                   +i))IITTTLLLTTTTTTTTTTTTLLLTTTT+:i)))=,
               |                   =))ITTTTTTTTTTTLTTTTTTLLLLLLTi:=)IIiii;
               |                  .i)IIITTTTTTTTLTTTITLLLLLLLT);=)I)))))i;
               |                  :))IIITTTTTLTTTTTTLLHLLLLL);=)II)IIIIi=:
               |                  :i)IIITTTTTTTTTLLLHLLHLL)+=)II)ITTTI)i=
               |                  .i)IIITTTTITTLLLHHLLLL);=)II)ITTTTII)i+
               |                  =i)IIIIIITTLLLLLLHLL=:i)II)TTTTTTIII)i''
               |                +i)i)))IITTLLLLLLLLT=:i)II)TTTTLTTIII)i;
               |              +ii)i:)IITTLLTLLLLT=;+i)I)ITTTTLTTTII))i;
               |             =;)i=:,=)ITTTTLTTI=:i))I)TTTLLLTTTTTII)i;
               |           +i)ii::,  +)IIITI+:+i)I))TTTTLLTTTTTII))=,
               |         :=;)i=:,,    ,i++::i))I)ITTTTTTTTTTIIII)=+''
               |       .+ii)i=::,,   ,,::=i)))iIITTTTTTTTIIIII)=+
               |      ,==)ii=;:,,,,:::=ii)i)iIIIITIIITIIII))i+:'
               |     +=:))i==;:::;=iii)+)=  `:i)))IIIII)ii+'
               |   .+=:))iiiiiiii)))+ii;
               |  .+=;))iiiiii)));ii+
               | .+=i:)))))))=+ii+
               |.+==i+::::=)i=;
               |,+==iiiiii+,
               |`+=+++;`
               |${Reset}
               |${greetUser("Space Explorer")}
    """.stripMargin)

    var running    = true
    var isQuizMode = false // Flag to indicate if we're in quiz mode

    while (running) {
      val prompt = if (isQuizMode) {
        s"${Green}Quiz> ${Reset}"
      } else {
        s"${Green}CHATURN> ${Reset}"
      }

      print(prompt)
      val input = StdIn.readLine()

      if (input == null || input.trim.toLowerCase == "exit") {
        if (isQuizMode) {
          // Exit quiz mode but continue the chatbot
          val response = quizHandler.handleMessage("end quiz")
          println(s"${Yellow}$response${Reset}")
          isQuizMode = false
        } else {
          // Exit the entire program
          running = false
          println(s"${Yellow}Goodbye! Come back to explore the cosmos!${Reset}")
        }
      } else if (input.trim.isEmpty) {
        println(s"${Red}Please type something!${Reset}")
      } else {
        // Check for quiz mode transitions
        input.trim.toLowerCase match {
          case cmd if cmd.startsWith("quiz") || cmd.startsWith("start quiz") =>
            if (!isQuizMode) {
              isQuizMode = true
              val response = quizHandler.handleMessage(input)
              println(formatQuizResponse(response))
            } else {
              val response = quizHandler.handleMessage(input)
              println(formatQuizResponse(response))
            }

          case "end quiz" | "stop quiz" | "exit quiz" | "exit" | "quit" if isQuizMode =>
            val response = quizHandler.handleMessage(input)
            println(formatQuizResponse(response))
            isQuizMode = false

          case _ if isQuizMode =>
            // We're in quiz mode, handle all input via quiz handler
            val response = quizHandler.handleMessage(input)
            println(formatQuizResponse(response))

            // Check if we should exit quiz mode based on response
            if (
              response.contains("The quiz is now complete") ||
              response.contains("No active quiz")
            ) {
              isQuizMode = false
            }

          case _ =>
            // Normal chatbot flow - parse command and respond
            val command = parser.parseInput(input, false) match {
              case Right(cmd) => cmd
              case Left(error) =>
                println(s"${Red}Error:${Reset} $error")
                Unknown(input)
            }

            analytics.logInteraction(command)
            val response = responder.respond(command)
            println(s"${Cyan}Response:${Reset} $userName, ${response.getOrElse("message", "Error!")}")
        }
      }
    }
  }

  // Format quiz responses with appropriate colors
  private def formatQuizResponse(response: String): String = {
    if (response.contains("Correct!")) {
      s"${Green}$response${Reset}"
    } else if (response.contains("The correct answer is")) {
      s"${Red}$response${Reset}"
    } else if (response.startsWith("Question:") || response.contains("Question:")) {
      // Highlight question and options
      val parts = response.split("Question:", 2)
      if (parts.length > 1) {
        val intro        = parts(0)
        val questionPart = parts(1)

        // Handle options formatting if present
        if (questionPart.contains("\n\n")) {
          val questionSplit = questionPart.split("\n\n", 2)
          s"$intro${Cyan}Question:${Reset}${questionSplit(0)}\n\n${Yellow}${questionSplit(1)}${Reset}"
        } else {
          s"$intro${Cyan}Question:${Reset}$questionPart"
        }
      } else {
        s"${Cyan}$response${Reset}"
      }
    } else if (response.contains("Quiz Summary") || response.contains("Your Personalized Space Profile")) {
      s"${Yellow}$response${Reset}"
    } else {
      s"${Cyan}$response${Reset}"
    }
  }
}
