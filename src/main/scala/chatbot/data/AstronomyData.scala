package chatbot.data

import chatbot.config.Config
import scala.io.Source
import scala.util.{Try, Success, Failure}

class AstronomyData(config: Config) {
  private val data: Map[String, Map[String, String]] = {
    Try(Source.fromFile(config.dataPath).mkString) match {
      case Success(json) =>
        val entries = json.split("},\\s*\\{").map(_.replaceAll("[{}\\[\\]]", ""))
        entries.map { entry =>
          val fields = entry.split(",\\s*").map(_.split(":").map(_.trim))
          val name   = fields.find(_(0) == "name").map(_(1)).getOrElse("")
          val facts  = fields.filter(_(0) != "name").map(f => f(0) -> f(1)).toMap
          name -> facts
        }.toMap
      case Failure(_) =>
        // Default data if file is missing
        Map(
          "Jupiter" -> Map("diameter" -> "139820 km", "mass" -> "1.898e27 kg"),
          "Mars"    -> Map("diameter" -> "6792 km", "mass" -> "6.4171e23 kg"),
          "Earth"   -> Map("diameter" -> "12742 km", "mass" -> "5.9724e24 kg")
        )
    }
  }

  def getFacts(topic: String): Option[Map[String, String]] = data.get(topic)
  def getPlanets: List[String]                             = data.keys.toList
}
