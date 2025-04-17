package chatbot.parser

object AST {
  sealed trait Command
  object Command {
    case object Help                   extends Command
    case class AskAbout(topic: String) extends Command
    case object ListCategory           extends Command
    case object ListPlanets            extends Command
    case object RandomFact             extends Command
    case object StartQuiz              extends Command
    case class Unknown(input: String)  extends Command
  }
}
