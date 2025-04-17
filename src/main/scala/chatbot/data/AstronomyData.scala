package chatbot.data

import upickle.default._

case class AstronomyData(
  planets: Map[String, Map[String, String]],
  stars: Map[String, Map[String, String]],
  phenomena: Map[String, Map[String, String]]
)
implicit val rw: ReadWriter[AstronomyData] = macroRW

trait DataSource {
  def getFacts(topic: String): Option[Map[String, String]]
}

class LocalDataSource extends DataSource {
  def getFacts(topic: String): Option[Map[String, String]] = None
  // TODO (Your Name): Load astronomy.json using upickle.
  // TODO (Your Name): Implement getFacts to return planet/star/phenomena data.
  // TODO (Your Name): Add caching for performance.
  // TODO (Your Name): Validate JSON data on load (e.g., required fields).
}
