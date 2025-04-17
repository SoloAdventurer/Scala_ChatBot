package chatbot.model

case class ChatContext(
  history: List[(String, String)] = Nil,
  quizState: Option[QuizState] = None
  // TODO (Your Name): Add methods to update history (e.g., append input/response).
  // TODO (Your Name): Implement quiz state transitions (e.g., next question).
)

case class QuizState(
  question: String,
  correctAnswer: String,
  questionNumber: Int
  // TODO (Your Name): Add fields for score, total questions.
)
