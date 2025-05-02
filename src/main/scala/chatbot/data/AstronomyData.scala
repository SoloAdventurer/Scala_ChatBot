package chatbot.data

import chatbot.config.Config
import upickle.default._
import scala.util.{Try, Success, Failure}

case class CelestialObject(
  name: String,
  `type`: String,
  diameter: Option[String] = None,
  mass: Option[String] = None,
  distance_from_sun: Option[String] = None,
  orbital_period: Option[String] = None,
  rotation_period: Option[String] = None,
  surface_temperature: Option[String] = None,
  atmosphere: Option[String] = None,
  distance_from_earth: Option[String] = None,
  composition: Option[String] = None,
  description: Option[String] = None,
  star_count: Option[String] = None,
  location: Option[String] = None,
  parent_planet: Option[String] = None
)

object CelestialObject {
  implicit val rw: ReadWriter[CelestialObject] = macroRW
}

class AstronomyData(config: Config) {
  private val fallbackData: List[CelestialObject] = List(
    CelestialObject(
      name = "Mercury",
      `type` = "planet",
      diameter = Some("4879 km"),
      mass = Some("3.3011e23 kg"),
      distance_from_sun = Some("57.9 million km"),
      orbital_period = Some("88 Earth days"),
      rotation_period = Some("58.6 Earth days"),
      surface_temperature = Some("-173 C to 427 C")
    ),
    CelestialObject(
      name = "Mars",
      `type` = "planet",
      diameter = Some("6792 km"),
      mass = Some("6.4171e23 kg"),
      distance_from_sun = Some("227.9 million km"),
      orbital_period = Some("687 Earth days"),
      rotation_period = Some("24.6 hours"),
      surface_temperature = Some("-153 C to 20 C")
    ),
    CelestialObject(
      name = "Earth",
      `type` = "planet",
      diameter = Some("12742 km"),
      mass = Some("5.9724e24 kg"),
      distance_from_sun = Some("149.6 million km"),
      orbital_period = Some("365.25 days"),
      rotation_period = Some("23.9 hours"),
      surface_temperature = Some("-88 C to 58 C"),
      atmosphere = Some("78% nitrogen, 21% oxygen, 1% other gases")
    )
  )

  private val objects: List[CelestialObject] = parseAstronomyData()

  private def parseAstronomyData(): List[CelestialObject] = {
    if (config.dataContent.isEmpty) {
      println("Warning: Empty data content, using fallback data")
      return fallbackData
    }

    // Trim the data content to show a preview
    val previewLength = Math.min(200, config.dataContent.length)
    val preview =
      config.dataContent.take(previewLength) + (if (config.dataContent.length > previewLength) "..." else "")
    println(s"JSON content: $preview")

    // Try to parse the JSON with more comprehensive error handling
    Try {
      read[List[CelestialObject]](config.dataContent)
    } match {
      case Success(parsed) =>
        println(s"Successfully parsed ${parsed.length} celestial objects")
        parsed
      case Failure(e) =>
        val errorMessage = e.getMessage
        println(s"Failed to parse astronomy.json: $errorMessage, using fallback data")

        // More detailed error reporting
        Try {
          // Try to find the position in the error message
          val indexPattern = "at index (\\d+)".r
          val indexOption  = indexPattern.findFirstMatchIn(errorMessage).map(_.group(1).toInt)

          indexOption.foreach { index =>
            val contextStart   = Math.max(0, index - 40)
            val contextEnd     = Math.min(config.dataContent.length, index + 40)
            val errorContext   = config.dataContent.slice(contextStart, contextEnd)
            val markerPosition = Math.min(40, index - contextStart)

            // Show the error context with a marker
            val beforeError = errorContext.substring(0, markerPosition)
            val afterError  = errorContext.substring(markerPosition)
            println(s"Error position: index $index")
            println(s"Context: $beforeError >>> $afterError")
          }
        } recover { case e =>
          println(s"Error while printing detailed error context: ${e.getMessage}")
        }

        fallbackData
    }
  }

  def getFacts(topic: String): Option[Map[String, String]] = {
    objects.find(_.name.toLowerCase == topic.toLowerCase).map { obj =>
      Map(
        "diameter"            -> obj.diameter,
        "mass"                -> obj.mass,
        "distance_from_sun"   -> obj.distance_from_sun,
        "orbital_period"      -> obj.orbital_period,
        "rotation_period"     -> obj.rotation_period,
        "surface_temperature" -> obj.surface_temperature,
        "atmosphere"          -> obj.atmosphere,
        "distance_from_earth" -> obj.distance_from_earth,
        "composition"         -> obj.composition,
        "description"         -> obj.description,
        "star_count"          -> obj.star_count,
        "location"            -> obj.location,
        "parent_planet"       -> obj.parent_planet
      ).collect { case (k, Some(v)) => k -> v }
    }
  }

  def getPlanets: List[String] = {
    objects.filter(_.`type` == "planet").map(_.name)
  }

  def getAllObjects: List[CelestialObject] = objects

  def getObjectsByType(objectType: String): List[CelestialObject] = {
    objects.filter(_.`type`.toLowerCase == objectType.toLowerCase)
  }

  // Check if the data was loaded successfully from the original source
  def isUsingFallbackData: Boolean = objects eq fallbackData
}
