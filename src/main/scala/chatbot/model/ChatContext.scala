package chatbot.context
case class Question(text: String, options: List[String], correct: String)

class ChatContext {
  private val questions = List(
    Question("Which planet is known as the Red Planet?", List("Mars", "Jupiter", "Venus"), "a"),
    Question("Which planet has the most moons?", List("Saturn", "Jupiter", "Neptune"), "a")
  )
  private var currentQuestion: Option[Question] = None
  private var questionIndex: Int                = 0

  def startQuiz(): Unit = {
    questionIndex = 0
    currentQuestion = questions.headOption
  }

  def getCurrentQuestion: Option[Question] = currentQuestion

  def submitAnswer(answer: String): Option[Boolean] = currentQuestion.map { q =>
    val correct = answer.toLowerCase == q.correct
    questionIndex += 1
    currentQuestion = questions.lift(questionIndex)
    correct
  }
}
