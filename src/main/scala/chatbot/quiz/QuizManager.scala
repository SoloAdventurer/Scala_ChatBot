package chatbot.quiz

import chatbot.quiz.data.QuizQuestion

class QuizManager {
  private var questions: List[QuizQuestion] = List()
  private var currentIndex: Int             = 0
  private var correctAnswers: Int           = 0
  private var totalQuestions: Int           = 0

  def selectQuizQuestions(topic: String): List[QuizQuestion] = {
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
      QuizQuestion("q3", "Which planet is known as the Red Planet?", List("Jupiter", "Mars", "Saturn"), Some("Mars")),
      QuizQuestion(
        "q4",
        "What is the largest planet in our solar system?",
        List("Neptune", "Jupiter", "Saturn"),
        Some("Jupiter")
      ), // Fixed typo: "Neptun,Jupiter" to "Neptune"
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
      QuizQuestion("p4", "Which one of the following is your favourite?", List("Scorpio", "Libra", "Leo"), None),
      QuizQuestion(
        "p5",
        "Which of the following do you like the best?",
        List("Aurora", "Solar Eclipse", "Supernova"),
        None
      )
    )

    val notFound = List(QuizQuestion("nf1", "The topic is not found", List(""), None))

    topic.toLowerCase match {
      case "traditional" => traditional
      case "personal"    => personal
      case _             => notFound
    }
  }

  def evaluateQuizAnswer(userAnswer: String, correctAnswer: String): Boolean = {
    userAnswer.trim.toLowerCase == correctAnswer.toLowerCase || correctAnswer.toLowerCase.contains(
      userAnswer.trim.toLowerCase
    )
  }

  def startQuiz(topic: String, questionCount: Int): Option[QuizQuestion] = {
    questions = selectQuizQuestions(topic).take(questionCount)
    if (questions.isEmpty || questions.head.text == "The topic is not found") {
      None
    } else {
      currentIndex = 0
      correctAnswers = 0
      totalQuestions = questions.length
      questions.headOption
    }
  }

  def answerCurrentQuestion(userAnswer: String): Option[String] = {
    if (currentIndex >= questions.length) {
      None
    } else {
      val question = questions(currentIndex)
      question.correctAnswer match {
        case Some(correct) =>
          if (evaluateQuizAnswer(userAnswer, correct)) {
            correctAnswers += 1
            Some("Correct!")
          } else {
            Some(s"Incorrect. The correct answer is: $correct")
          }
        case None =>
          Some("Thanks for your answer!")
      }
    }
  }

  def nextQuestion(): Option[QuizQuestion] = {
    currentIndex += 1
    if (currentIndex < questions.length) {
      Some(questions(currentIndex))
    } else {
      None
    }
  }

  def getCurrentQuestion(): Option[QuizQuestion] = {
    if (currentIndex < questions.length) {
      Some(questions(currentIndex))
    } else {
      None
    }
  }

  def getQuizSummary(): String = {
    if (totalQuestions == 0) {
      "No quiz was taken."
    } else {
      s"Quiz Summary: You got $correctAnswers out of $totalQuestions correct!"
    }
  }

  def resetQuiz(): Unit = {
    questions = List()
    currentIndex = 0
    correctAnswers = 0
    totalQuestions = 0
  }

  def isQuizActive(): Boolean = {
    questions.nonEmpty && currentIndex < questions.length
  }

  def getCurrentQuestionIndex(): Int = {
    currentIndex
  }

  def getTotalQuestions(): Int = {
    totalQuestions
  }
}
