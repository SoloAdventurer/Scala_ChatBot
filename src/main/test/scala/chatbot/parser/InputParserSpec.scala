package chatbot.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import chatbot.parser.AST._

class InputParserSpec extends AnyFlatSpec with Matchers {
  val parser = new InputParser()

  "InputParser" should "parse help command" in {
    parser.parseInput("help") shouldBe Right(Help())
    // TODO (Member A): Add tests for Ask, ListCategory, Scrape commands.
    // TODO (Member A): Test Compare, RandomFact, StartQuiz, AnswerQuiz.
    // TODO (Member A): Test invalid inputs (e.g., "invalid cmd").
    // TODO (Member A): Test synonyms (e.g., "about" vs "on").
  }
}
