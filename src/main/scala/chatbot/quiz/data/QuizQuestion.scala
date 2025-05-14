package chatbot.quiz.data

// Represents a single quiz question
case class QuizQuestion(
  id: String,
  text: String,
  options: List[String],
  correctAnswer: Option[String]
)
