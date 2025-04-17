package chatbot.scraper

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WebScraperSpec extends AnyFlatSpec with Matchers {
  val scraper = new JsoupWebScraper()

  "WebScraper" should "scrape a topic" in {
    scraper.scrape("test").isSuccess shouldBe true
    // TODO (Member B): Mock Jsoup to test scraping (e.g., fake HTML).
    // TODO (Member B): Test specific topics (e.g., "Mars" -> Wikipedia).
    // TODO (Member B): Test caching behavior (same topic, no re-scrape).
    // TODO (Member B): Test failure cases (e.g., invalid URL, timeout).
  }
}
