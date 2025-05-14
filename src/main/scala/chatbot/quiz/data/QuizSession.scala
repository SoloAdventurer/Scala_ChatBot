package chatbot.quiz.data

case class QuizSession(
  topic: String,
  currentQuestionIndex: Int,
  totalQuestions: Int,
  correctAnswers: Int,
  completed: Boolean
)
