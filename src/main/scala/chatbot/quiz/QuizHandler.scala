package chatbot.quiz

import chatbot.quiz.data.QuizQuestion

class QuizHandler {
  private val quizManager    = new QuizManager()
  private var quizActive     = false
  private var awaitingAnswer = false
  private var currentTopic   = "traditional"

  def handleMessage(message: String): String = {
    if (quizActive && awaitingAnswer) {
      handleQuizAnswer(message)
    } else {
      message.toLowerCase.trim match {
        case m if m.startsWith("start quiz") || m.startsWith("quiz me") || m == "quiz" =>
          startQuiz(m)
        case "quiz topics" | "show topics" | "available quizzes" =>
          "Available quiz topics: traditional, personal"
        case "quiz results" | "show results" | "quiz summary" =>
          quizManager.getQuizSummary()
        case "reset quiz" | "restart quiz" =>
          quizManager.resetQuiz()
          quizActive = false
          awaitingAnswer = false
          "Quiz reset. Start a new quiz with 'start quiz'."
        case "end quiz" | "stop quiz" | "exit quiz" =>
          endQuiz()
        case "help" | "quiz help" =>
          "Quiz commands: start quiz [topic], quiz topics, quiz results, reset quiz, end quiz"
        case _ =>
          if (quizActive) {
            "Please answer the current question or type 'end quiz' to stop."
          } else {
            "Try 'start quiz' to begin or 'help' for commands."
          }
      }
    }
  }

  def startQuiz(message: String): String = {
    val topicPattern = "(?:start|begin) quiz(?: on| about)? (.+)".r
    val topic = message.toLowerCase match {
      case topicPattern(requestedTopic) => requestedTopic.trim
      case _                            => "traditional"
    }
    currentTopic = topic
    val questionCount = 7

    quizManager.startQuiz(topic, questionCount) match {
      case Some(question) =>
        quizActive = true
        awaitingAnswer = true
        formatQuestion(question, isFirst = true, topic)
      case None =>
        s"Sorry, I couldn't start a quiz on '$topic'. Available topics are: traditional, personal"
    }
  }

  def isQuizActive(): Boolean = quizActive

  private def handleQuizAnswer(answer: String): String = {
    quizManager.answerCurrentQuestion(answer) match {
      case Some(response) =>
        val nextQuestion = quizManager.nextQuestion()
        if (nextQuestion.isDefined) {
          s"$response\n\n${formatQuestion(nextQuestion.get, isFirst = false, currentTopic)}"
        } else {
          quizActive = false
          awaitingAnswer = false
          s"$response\n\n${quizManager.getQuizSummary()}"
        }
      case None =>
        quizActive = false
        awaitingAnswer = false
        "No active question. Quiz ended."
    }
  }

  private def formatQuestion(question: QuizQuestion, isFirst: Boolean, topic: String): String = {
    val intro = if (isFirst) s"Starting $topic quiz!\n\n" else ""
    s"${intro}Question: ${question.text}\n\nOptions:\n${question.options.map(opt => s"- $opt").mkString("\n")}"
  }

  private def endQuiz(): String = {
    val summary = quizManager.getQuizSummary()
    quizManager.resetQuiz()
    quizActive = false
    awaitingAnswer = false
    s"Quiz ended. $summary"
  }
}
