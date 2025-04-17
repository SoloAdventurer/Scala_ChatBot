package chatbot.scraper

case class ScraperConfig(
  timeout: Int = 10000,
  maxResults: Int = 1,
  topicUrls: Map[String, String] = Map()
  // TODO (Member B): Populate topicUrls with defaults (e.g., "black hole" -> "https://en.wikipedia.org/wiki/Black_hole").
  // TODO (Member B): Add validation for timeout and maxResults.
)
