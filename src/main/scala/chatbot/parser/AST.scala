package chatbot.parser

object AST {
  sealed trait Command
  object Command {
    case object Help extends Command
    // TODO: Add other command types like Ask, ListCategory, etc.
  }
}
