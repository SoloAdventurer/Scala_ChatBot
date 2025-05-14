package chatbot.main

import chatbot.parser.InputParser
import chatbot.responder.Responder
import chatbot.data.{AstronomyData}
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

  // Format quiz responses with appropriate colors
  private def formatQuizResponse(response: String): String = {
    response match {
      case r if r.contains("Correct!")                                         => s"${Green}$r${Reset}"
      case r if r.contains("Incorrect") || r.contains("The correct answer is") => s"${Red}$r${Reset}"
      case r if r.startsWith("Question:") || r.contains("Question:") =>
        val parts = r.split("Question:", 2)
        if (parts.length > 1) {
          val intro        = parts(0).trim
          val questionPart = parts(1).trim
          val (questionText, options) = if (questionPart.contains("\n\n")) {
            val split = questionPart.split("\n\n", 2)
            (split(0), split(1))
          } else {
            (questionPart, "")
          }
          s"${intro}${Cyan}Question:${Reset} $questionText\n\n${Yellow}$options${Reset}"
        } else {
          s"${Cyan}$r${Reset}"
        }
      case r if r.contains("Quiz Summary") || r.contains("Your Personalized Space Profile") => s"${Yellow}$r${Reset}"
      case r if r.trim.isEmpty => s"${Cyan}No response available.${Reset}"
      case r                   => s"${Cyan}$r${Reset}"
    }
  }

  def greetUser(name: String): String = {
    s"Hello, $name! I'm CHATURN, your astronomy chatbot. Ask me about planets, stars, or try 'help' for commands!"
  }

  def main(args: Array[String]): Unit = {
    val config      = Config.load
    val dataSource  = new AstronomyData(config)
    val analytics   = new Analytics()
    val quizManager = new QuizManager()
    val responder   = new Responder(dataSource, analytics, quizManager)
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
      val prompt = if (isQuizMode) s"${Green}Quiz> ${Reset}" else s"${Green}CHATURN> ${Reset}"
      print(prompt)
      val input = StdIn.readLine()

      if (input == null || input.trim.toLowerCase == "exit") {
        running = false // Exit the program in all cases
        val message = if (isQuizMode) {
          val response = quizHandler.handleMessage("end quiz")
          quizHandler.resetQuizState()
          s"${Yellow}$response${Reset}"
        } else {
          s"${Yellow}Goodbye! Come back to explore the cosmos!${Reset}"
        }
        println(message)
      } else if (input.trim.isEmpty) {
        println(s"${Red}Please type something!${Reset}")
      } else {
        val normalizedInput = input.trim.toLowerCase
        val response = normalizedInput match {
          case cmd
              if (cmd.startsWith("start quiz") || cmd == "quiz" || cmd.startsWith(
                "start an astronomy quiz"
              )) && !isQuizMode =>
            isQuizMode = true
            quizHandler.handleMessage(input)

          case cmd if List("end quiz", "stop quiz", "exit quiz", "quit").contains(cmd) && isQuizMode =>
            val resp = quizHandler.handleMessage(input)
            isQuizMode = false
            quizHandler.resetQuizState()
            resp

          case _ if isQuizMode =>
            val resp = quizHandler.handleMessage(input)
            // Sync with quizHandler state and check for completion
            isQuizMode = quizHandler.isQuizActive()
            if (!isQuizMode || resp.contains("Quiz ended") || resp.contains("No active quiz")) {
              quizHandler.resetQuizState()
            }
            resp

          case _ =>
            val command = parser.parseInput(input, isQuizMode)
            analytics.logInteraction(command)
            responder.respond(command).getOrElse("message", "I didnt understand. Try 'help'!")
        }

        // Output response based on context
        val output = if (isQuizMode || normalizedInput.startsWith("start quiz")) {
          formatQuizResponse(response)
        } else {
          s"${Cyan}CHATURN>${Reset} $userName, $response"
        }
        println(output)
      }
    }

  }
}
