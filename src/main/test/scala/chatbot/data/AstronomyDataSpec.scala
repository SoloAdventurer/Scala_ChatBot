package chatbot.data

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AstronomyDataSpec extends AnyFlatSpec with Matchers {
  val dataSource = new LocalDataSource()

  "LocalDataSource" should "return facts" in {
    dataSource.getFacts("Mars") shouldBe None
    // TODO (Your Name): Test loading astronomy.json (mock JSON file).
    // TODO (Your Name): Test getFacts for planets (e.g., Mars -> diameter).
    // TODO (Your Name): Test stars and phenomena.
    // TODO (Your Name): Test missing topics (e.g., return None).
  }
}
