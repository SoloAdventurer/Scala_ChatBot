package chatbot.scraper

import org.jsoup.Jsoup
import scala.util.Try

trait WebScraper {
  def scrape(topic: String): Try[ScrapedData]
}

class JsoupWebScraper extends WebScraper {
  def scrape(topic: String): Try[ScrapedData] = Try {
    ScrapedData(topic, s"Scraped info about $topic")
    // TODO (Member B): Implement Jsoup scraping for topics (e.g., Wikipedia URLs).
    // TODO (Member B): Map topics to URLs using ScraperConfig (e.g., "black hole" -> wiki).
    // TODO (Member B): Extract relevant text (e.g., first paragraph, clean HTML).
    // TODO (Member B): Add caching to avoid repeated scrapes.
  }
}

case class ScrapedData(topic: String, content: String)
