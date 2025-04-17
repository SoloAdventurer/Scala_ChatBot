package chatbot.responder

import chatbot.scraper.WebScraper
import chatbot.data.DataSource
import chatbot.parser.AST.Command

class Responder(scraper: WebScraper, dataSource: DataSource) {
  def respond(command: Command): Option[String] = {
    Some(s"Response for $command")

    /* Example for a function for AskAbout:
      def respond(ast: AST): Map[String, String] = ast match {
      case AskAbout("cold") =>
        val coldPlanets = data.filter(_.tags.contains("cold")).map(_.name)
        Map("message" -> s"Cold planets: ${coldPlanets.mkString(", ")}", "topic" -> "suggestion")
      case AskAbout(topic) => data.getFacts(topic).getOrElse(Map("message" -> "Topic not found"))
      ...
    }
     */

    // TODO (Member C): Implement logic for each Command (e.g., Help -> usage text).
    // TODO (Member C): Use DataSource for local facts (astronomy.json).
    // TODO (Member C): Fallback to WebScraper for missing data.
    // TODO (Member C): Handle quiz state via ChatContext (e.g., track answers).
    // TODO (Member C): Format responses (e.g., "Mars: diameter = 6,792 km").
  }
}
