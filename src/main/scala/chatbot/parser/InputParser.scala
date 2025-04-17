package chatbot.parser

import scala.util.parsing.combinator.RegexParsers
import chatbot.parser.AST.Command

class InputParser extends RegexParsers {
  def parseInput(input: String): Either[String, Command] = {
    parseAll(command, input) match {
      case Success(result, _) => Right(result)
      case failure            => Left(s"Parse error: $failure")
    }
    // TODO (Member A): Define regex parsers for commands (e.g., "ask about Jupiter", "list planets").
    // TODO (Member A): Support synonyms (e.g., "about" = "on").
    // TODO (Member A): Handle complex commands (e.g., "compare Earth Mars", "start quiz").
    // TODO (Member A): Add error messages for invalid inputs.
  }

  private def command: Parser[Command] = {
    literal("help") ^^ (_ => Command.Help)
    // TODO (Member A): Add parsers for Ask, ListCategory, Scrape, Compare, RandomFact, StartQuiz, AnswerQuiz.
  }
}
