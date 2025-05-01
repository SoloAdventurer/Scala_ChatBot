package chatbot.analytics

import chatbot.parser.AST.Command
import chatbot.parser.AST.Command._

class Analytics {
  private var interactions: List[Command] = List.empty

  def logInteraction(command: Command): Unit = {
    interactions = command :: interactions
  }

  def getQuizAccuracy: Double = {
    val quizAnswers = interactions.collect { case c: AnswerQuiz => c }
    if (quizAnswers.isEmpty) 0.0
    else {
      val correct = quizAnswers.count(_.answer == "a") // Placeholder; update with actual quiz logic
      correct.toDouble / quizAnswers.length
    }
  }

  def getInteractionCount: Map[String, Int] = {
    interactions.groupBy(_.getClass.getSimpleName).mapValues(_.length).toMap
  }
}
