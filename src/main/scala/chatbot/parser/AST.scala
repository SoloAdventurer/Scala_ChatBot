package chatbot.parser

object AST {
  sealed trait Command

  object Command {
    case object Help                                   extends Command
    case object ListPlanets                            extends Command
    case object RandomFact                             extends Command
    case object StartQuiz                              extends Command
    case class AskAbout(topic: String)                 extends Command
    case class ListCategory(category: String)          extends Command
    case class Compare(topic1: String, topic2: String) extends Command
    case class AnswerQuiz(answer: String)              extends Command
    case class Unknown(input: String)                  extends Command

    // New commands
    case object Exit                                               extends Command
    case object Greet                                              extends Command
    case class PlanetInfo(planet: String)                          extends Command
    case class StarInfo(star: String)                              extends Command
    case class ConstellationInfo(constellation: String)            extends Command
    case class Distance(object1: String, object2: String)          extends Command
    case class PropertyQuery(objectName: String, property: String) extends Command
    case class AstronomicalEvent(query: String)                    extends Command
    case class NightSkyInfo(query: String)                         extends Command
    case class FunFact(topic: String)                              extends Command
  }
}
