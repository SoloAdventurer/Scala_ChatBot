package chatbot.core
package chatbot.Config

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    println("Astronomy Chatbot. Type 'exit' to quit.")
    while (true) {
      val input = StdIn.readLine(">> ")
      if (input == "exit") sys.exit(0)
      println(s"Received: $input") // Placeholder

      // TODO (Member C): Integrate InputParser to parse user input (e.g., "ask about Mars").
      // TODO (Member C): Call Responder to generate responses based on parsed commands.
      // TODO (Member C): Use AstronomyData and JsoupWebScraper for data and fallback scraping.
      // TODO (Member C): Handle errors gracefully (e.g., invalid input).
    }
  }
}
