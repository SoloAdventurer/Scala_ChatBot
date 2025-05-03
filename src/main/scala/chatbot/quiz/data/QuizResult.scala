package chatbot.quiz.data

case class QuizResult(
  isCorrect: Boolean,
  correctAnswer: Option[String],
  explanation: String
)
