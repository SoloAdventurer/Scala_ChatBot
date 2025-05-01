package chatbot.quiz

import scala.util.Random

case class QuizQuestion(id: String, text: String, options: List[String], correctAnswer: Option[String])

case class QuizResult(feedback: String, correct: Boolean)

class QuizManager {
  private var currentQuestions: List[QuizQuestion] = List()

  def selectQuizQuestions(topic: String): List[QuizQuestion] = {
    val traditional = List(
      QuizQuestion(
        id = "q1",
        text = "What is the name of our galaxy?",
        options = List("Butterfly Galaxy", "Milky Way Galaxy", "Spiral Galaxy"),
        correctAnswer = Some("Milky Way Galaxy")
      ),
      QuizQuestion(
        id = "q2",
        text = "What is the smallest planet in our solar system?",
        options = List("Mercury", "Mars", "Saturn"),
        correctAnswer = Some("Mercury")
      ),
      QuizQuestion(
        id = "q3",
        text = "Which planet is known as the Red Planet?",
        options = List("Jupiter", "Mars", "Saturn"),
        correctAnswer = Some("Mars")
      ),
      QuizQuestion(
        id = "q4",
        text = "What is the largest planet in our solar system?",
        options = List("Neptune", "Jupiter", "Saturn"),
        correctAnswer = Some("Jupiter")
      ),
      QuizQuestion(
        id = "q5",
        text = "How does it rain on Venus?",
        options = List("Diamonds", "Methane", "Sulfuric Acid"),
        correctAnswer = Some("Sulfuric Acid")
      )
    )

    val personal = List(
      QuizQuestion(
        id = "p1",
        text = "What is your favourite planet in our solar system?",
        options = List("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"),
        correctAnswer = None
      ),
      QuizQuestion(
        id = "p2",
        text = "Which planet of the following would you want to live on?",
        options = List("Saturn", "Jupiter", "Mars"),
        correctAnswer = None
      ),
      QuizQuestion(
        id = "p3",
        text = "Would you rather fly through the clouds or walk on the moon?",
        options = List("Fly through the clouds", "Walk on the moon", "Both"),
        correctAnswer = None
      ),
      QuizQuestion(
        id = "p4",
        text = "Which one of the following is your favourite constellation?",
        options = List("Scorpio", "Libra", "Leo"),
        correctAnswer = None
      ),
      QuizQuestion(
        id = "p5",
        text = "Which of the following do you like the best?",
        options = List("Aurora", "Solar Eclipse", "Supernova"),
        correctAnswer = None
      )
    )

    val notFound = List(
      QuizQuestion(
        id = "nf1",
        text = "The topic is not found",
        options = List(""),
        correctAnswer = None
      )
    )

    topic.toLowerCase match {
      case "traditional" => traditional
      case "personal"    => personal
      case _             => notFound
    }
  }

  def evaluateQuizAnswer(userAnswer: String, correctAnswer: Option[String]): QuizResult = {
    correctAnswer match {
      case Some(correct) =>
        val isCorrect = userAnswer.trim.toLowerCase == correct.toLowerCase || correct.toLowerCase.contains(
          userAnswer.trim.toLowerCase
        )
        val feedback = if (isCorrect) "Well done!" else s"The correct answer was $correct."
        QuizResult(feedback, isCorrect)
      case None =>
        QuizResult("Thanks for sharing your preference!", true)
    }
  }

  def startQuiz(): QuizQuestion = {
    // Alternate between traditional and personal topics
    val topic = if (Random.nextBoolean()) "traditional" else "personal"
    currentQuestions = selectQuizQuestions(topic)
    currentQuestions(Random.nextInt(currentQuestions.length))
  }

  def checkAnswer(questionId: String, userAnswer: String): QuizResult = {
    currentQuestions.find(_.id == questionId) match {
      case Some(question) => evaluateQuizAnswer(userAnswer, question.correctAnswer)
      case None           => QuizResult("Question not found.", false)
    }
  }
}
