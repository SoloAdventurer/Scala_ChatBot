package chatbot.quiz

/** Models a quiz question with options and optional correct answer */
case class QuizQuestion(
  id: String,
  text: String,
  options: List[String],
  correctAnswer: Option[String]
)

/** Models a quiz session including questions and state */
type QuizState = (
  List[QuizQuestion], // questions
  Int,                // currentIndex
  Int,                // correctAnswers
  Int,                // totalQuestions
  String              // currentTopic
)

/** Pure functional quiz implementation */
object Quiz {
  // Initial state
  def initialState: QuizState = (List.empty, 0, 0, 0, "")

  // Checks if a quiz is active based on state
  def isActive(state: QuizState): Boolean = {
    val (questions, currentIndex, _, _, _) = state
    questions.nonEmpty && currentIndex < questions.length
  }

  /** Handles a user message, returning a response and new state
    * @param state
    *   Current quiz state
    * @param message
    *   User message
    * @return
    *   (response, new state)
    */
  def handleMessage(state: QuizState, message: String): (String, QuizState) = {
    val (questions, currentIndex, correctAnswers, totalQuestions, currentTopic) = state

    if (isActive(state) && currentIndex < questions.length) {
      handleQuizAnswer(state, message)
    } else {
      // Pattern matching for quiz commands
      message.toLowerCase.trim match {
        case m if m.startsWith("start quiz") || m.startsWith("quiz me") || m == "quiz" =>
          startQuiz(state, m)

        case "quiz topics" | "show topics" | "available quizzes" =>
          ("Available quiz topics: traditional, personal", state)

        case "quiz results" | "show results" | "quiz summary" =>
          (getQuizSummary(state), state)

        case "reset quiz" | "restart quiz" =>
          ("Quiz reset. Start a new quiz with 'start quiz'.", initialState)

        case "end quiz" | "stop quiz" | "exit quiz" =>
          endQuiz(state)

        case "help" | "quiz help" =>
          ("Quiz commands: start quiz [topic], quiz topics, quiz results, reset quiz, end quiz", state)

        case _ =>
          if (isActive(state)) {
            ("Please answer the current question or type 'end quiz' to stop.", state)
          } else {
            ("Try 'start quiz' to begin or 'help' for commands.", state)
          }
      }
    }
  }

  /** Starts a new quiz
    * @param state
    *   Current quiz state
    * @param message
    *   User message containing potential topic
    * @return
    *   (response, new state)
    */
  def startQuiz(state: QuizState, message: String): (String, QuizState) = {
    // Extract topic using pattern matching instead of regex
    val topic = message.toLowerCase match {
      case m if m.contains("about") =>
        val parts = m.split("about")
        if (parts.length > 1) parts(1).trim else "traditional"
      case m if m.contains("on") =>
        val parts = m.split("on")
        if (parts.length > 1) parts(1).trim else "traditional"
      case _ => "traditional"
    }

    val questionCount = 5
    val questions     = selectQuizQuestions(topic)

    if (questions.nonEmpty && questions.head.text != "The topic is not found") {
      val newState = (questions.take(questionCount), 0, 0, questionCount, topic)
      (formatQuestion(questions.head, isFirst = true, topic), newState)
    } else {
      (s"Sorry, I couldn't start a quiz on '$topic'. Available topics are: traditional, personal", state)
    }
  }

  /** Handles a user's answer to a quiz question
    * @param state
    *   Current quiz state
    * @param answer
    *   User's answer
    * @return
    *   (response, new state)
    */
  def handleQuizAnswer(state: QuizState, answer: String): (String, QuizState) = {
    val (questions, currentIndex, correctAnswers, totalQuestions, currentTopic) = state

    if (currentIndex >= questions.length) {
      ("No active question. Quiz ended.", initialState)
    } else {
      val question = questions(currentIndex)

      val (responseMsg, updatedCorrectAnswers) = question.correctAnswer match {
        case Some(correct) =>
          if (evaluateQuizAnswer(answer, correct)) {
            ("Correct!", correctAnswers + 1)
          } else {
            (s"Incorrect. The correct answer is: $correct", correctAnswers)
          }
        case None =>
          ("Thanks for your answer!", correctAnswers)
      }

      val nextIndex = currentIndex + 1

      if (nextIndex < questions.length) {
        val nextQuestion = questions(nextIndex)
        val newState     = (questions, nextIndex, updatedCorrectAnswers, totalQuestions, currentTopic)
        (s"$responseMsg\n\n${formatQuestion(nextQuestion, isFirst = false, currentTopic)}", newState)
      } else {
        val finalState = (questions, nextIndex, updatedCorrectAnswers, totalQuestions, currentTopic)
        (s"$responseMsg\n\n${getQuizSummary(finalState)}", initialState)
      }
    }
  }

  /** Ends the current quiz
    * @param state
    *   Current quiz state
    * @return
    *   (response, new state)
    */
  def endQuiz(state: QuizState): (String, QuizState) = {
    val summary = getQuizSummary(state)
    (s"Quiz ended. $summary", initialState)
  }

  /** Formats a question for display
    * @param question
    *   Question to format
    * @param isFirst
    *   Whether this is the first question
    * @param topic
    *   Quiz topic
    * @return
    *   Formatted question string
    */
  def formatQuestion(question: QuizQuestion, isFirst: Boolean, topic: String): String = {
    val intro = if (isFirst) s"Starting $topic quiz!\n\n" else ""
    s"${intro}Question: ${question.text}\n\nOptions:\n${question.options.map(opt => s"- $opt").mkString("\n")}"
  }

  /** Gets a summary of the current quiz
    * @param state
    *   Current quiz state
    * @return
    *   Summary string
    */
  def getQuizSummary(state: QuizState): String = {
    val (_, _, correctAnswers, totalQuestions, _) = state

    if (totalQuestions == 0) {
      "No quiz was taken."
    } else {
      s"Quiz Summary: You got $correctAnswers out of $totalQuestions correct!"
    }
  }

  /** Helper functions */

  def selectQuizQuestions(topic: String): List[QuizQuestion] = {
    // Define question banks
    val traditional = List(
      QuizQuestion(
        "q1",
        "What is the name of our galaxy?",
        List("Butterfly Galaxy", "Milky Way Galaxy", "Spiral Galaxy"),
        Some("Milky Way Galaxy")
      ),
      QuizQuestion(
        "q2",
        "What is the smallest planet in our solar system?",
        List("Mercury", "Mars", "Saturn"),
        Some("Mercury")
      ),
      QuizQuestion(
        "q3",
        "Which planet is known as the Red Planet?",
        List("Jupiter", "Mars", "Saturn"),
        Some("Mars")
      ),
      QuizQuestion(
        "q4",
        "What is the largest planet in our solar system?",
        List("Neptune", "Jupiter", "Saturn"),
        Some("Jupiter")
      ),
      QuizQuestion(
        "q5",
        "How does it rain on Venus?",
        List("Diamonds", "Methane", "Sulfuric Acid"),
        Some("Sulfuric Acid")
      )
    )

    val personal = List(
      QuizQuestion(
        "p1",
        "What is your favourite planet in our solar system?",
        List("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"),
        None
      ),
      QuizQuestion(
        "p2",
        "Which planet of the following would you want to live on?",
        List("Saturn", "Jupiter", "Mars"),
        None
      ),
      QuizQuestion(
        "p3",
        "Would you rather fly through the clouds or walk on the moon?",
        List("Fly through the clouds", "Walk on the moon", "Both"),
        None
      ),
      QuizQuestion(
        "p4",
        "Which one of the following is your favourite?",
        List("Scorpio", "Libra", "Leo"),
        None
      ),
      QuizQuestion(
        "p5",
        "Which of the following do you like the best?",
        List("Aurora", "Solar Eclipse", "Supernova"),
        None
      )
    )

    val notFound = List(QuizQuestion("nf1", "The topic is not found", List(""), None))

    // Pattern matching on topic
    topic.toLowerCase match {
      case "traditional" => traditional
      case "personal"    => personal
      case _             => notFound
    }
  }

  def evaluateQuizAnswer(userAnswer: String, correctAnswer: String): Boolean = {
    userAnswer.trim.toLowerCase == correctAnswer.toLowerCase ||
    correctAnswer.toLowerCase.contains(userAnswer.trim.toLowerCase)
  }
}
