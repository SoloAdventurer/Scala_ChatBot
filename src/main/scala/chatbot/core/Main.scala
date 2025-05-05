package chatbot.main

import scala.io.StdIn
import java.time.LocalDateTime

/** Main entry point with pure functional approach */
object Main {
  // ANSI color codes
  val Cyan: String   = "\u001b[36m"
  val Yellow: String = "\u001b[33m"
  val Green: String  = "\u001b[32m"
  val Red: String    = "\u001b[31m"
  val Reset: String  = "\u001b[0m"
  val Blink: String  = "\u001b[5m"

  // Program state type definition
  type ProgramState = (
    String,                                    // userName
    Boolean,                                   // running
    chatbot.quiz.QuizState,                    // quizState
    chatbot.analytics.Analytics.AnalyticsState // analyticsState
  )

  /** Creates initial program state
    * @param userName
    *   The user's name
    * @return
    *   Initial program state
    */
  def initialState(userName: String): ProgramState = (
    userName,
    true, // running
    chatbot.quiz.Quiz.initialState,
    chatbot.analytics.Analytics.initialState
  )

  /** Formats quiz responses with appropriate colors
    * @param response
    *   Response to format
    * @return
    *   Formatted response
    */
  def formatQuizResponse(response: String): String = {
    response match {
      case r if r.contains("Correct!") =>
        s"${Green}$r${Reset}"

      case r if r.contains("Incorrect") || r.contains("The correct answer is") =>
        s"${Red}$r${Reset}"

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

      case r if r.contains("Quiz Summary") || r.contains("Your Personalized Space Profile") =>
        s"${Yellow}$r${Reset}"

      case r if r.trim.isEmpty =>
        s"${Cyan}No response available.${Reset}"

      case r =>
        s"${Cyan}$r${Reset}"
    }
  }

  /** Greets the user
    * @param name
    *   User's name
    * @return
    *   Greeting message
    */
  def greetUser(name: String): String = {
    s"Hello, $name! I'm CHATURN, your astronomy chatbot. Ask me about planets, stars, or try 'help' for commands!"
  }

  /** Processes user input, returning new state and output
    * @param state
    *   Current program state
    * @param input
    *   User input
    * @return
    *   (new state, output message)
    */
  def processInput(state: ProgramState, input: String): (ProgramState, String) = {
    val (userName, running, quizState, analyticsState) = state
    val isQuizMode                                     = chatbot.quiz.Quiz.isActive(quizState)

    // Handle exit command
    if (input == null || input.trim.toLowerCase == "exit") {
      if (isQuizMode) {
        val (response, newQuizState) = chatbot.quiz.Quiz.endQuiz(quizState)
        val updatedAnalytics         = chatbot.analytics.Analytics.logInteraction(analyticsState, "exit", false)
        ((userName, false, newQuizState, updatedAnalytics), s"${Yellow}$response${Reset}")
      } else {
        val updatedAnalytics = chatbot.analytics.Analytics.logInteraction(analyticsState, "exit", false)
        ((userName, false, quizState, updatedAnalytics), s"${Yellow}Goodbye! Come back to explore the cosmos!${Reset}")
      }
    }
    // Handle empty input
    else if (input.trim.isEmpty) {
      (state, s"${Red}Please type something!${Reset}")
    }
    // Process meaningful input
    else {
      val normalizedInput = input.trim.toLowerCase

      val (newQuizState, analyticsUpdate, response) = normalizedInput match {
        // Starting a quiz
        case cmd
            if (cmd
              .startsWith("start quiz") || cmd == "quiz" || cmd.startsWith("start an astronomy quiz")) && !isQuizMode =>
          val (quizResponse, updatedQuizState) = chatbot.quiz.Quiz.startQuiz(quizState, input)
          val loggedAnalytics = chatbot.analytics.Analytics.logInteraction(analyticsState, "startquiz", false)
          (updatedQuizState, loggedAnalytics, quizResponse)

        // Ending a quiz
        case cmd if List("end quiz", "stop quiz", "exit quiz", "quit").contains(cmd) && isQuizMode =>
          val (quizResponse, updatedQuizState) = chatbot.quiz.Quiz.endQuiz(quizState)
          val loggedAnalytics = chatbot.analytics.Analytics.logInteraction(analyticsState, "endquiz", false)
          (updatedQuizState, loggedAnalytics, quizResponse)

        // In quiz mode
        case _ if isQuizMode =>
          val (quizResponse, updatedQuizState) = chatbot.quiz.Quiz.handleMessage(quizState, input)
          // For analytics, determine if answer was correct (simplistic implementation)
          val isCorrect = quizResponse.contains("Correct!")
          val loggedAnalytics = chatbot.analytics.Analytics.logInteraction(
            analyticsState,
            s"answerquiz_${input.take(10)}",
            isCorrect
          )
          (updatedQuizState, loggedAnalytics, quizResponse)

        // Normal chat mode - would typically call parser and responder
        case _ =>
          // In a full implementation, we would:
          // val command = InputParser.parseInput(input)
          // val response = Responder.respond(command)
          val loggedAnalytics =
            chatbot.analytics.Analytics.logInteraction(analyticsState, s"chat_${input.take(10)}", false)
          // Simplified placeholder for the refactoring
          val placeholderResponse = s"In a full implementation, I would respond to: $input"
          (quizState, loggedAnalytics, placeholderResponse)
      }

      // Format the output based on context
      val output = if (chatbot.quiz.Quiz.isActive(newQuizState) || normalizedInput.startsWith("start quiz")) {
        formatQuizResponse(response)
      } else {
        s"${Cyan}CHATURN>${Reset} $userName, $response"
      }

      ((userName, running, newQuizState, analyticsUpdate), output)
    }
  }

  /** Main execution loop
    * @param state
    *   Current program state
    */
  def runLoop(state: ProgramState): Unit = {
    val (userName, running, quizState, _) = state

    if (!running) {
      return // Exit the loop
    }

    val isQuizMode = chatbot.quiz.Quiz.isActive(quizState)
    val prompt     = if (isQuizMode) s"${Green}Quiz> ${Reset}" else s"${Green}CHATURN> ${Reset}"

    print(prompt)
    val input = StdIn.readLine()

    val (newState, output) = processInput(state, input)
    println(output)

    runLoop(newState) // Recursive call instead of while loop
  }

  /** Main entry point
    * @param args
    *   Command line arguments
    */
  def main(args: Array[String]): Unit = {
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
               |${greetUser(userName)}
    """.stripMargin)

    // Start main execution loop with initial state
    runLoop(initialState(userName))

    // Final analytics could be shown here
    // println(Analytics.getDashboard(finalAnalyticsState))
  }
}
