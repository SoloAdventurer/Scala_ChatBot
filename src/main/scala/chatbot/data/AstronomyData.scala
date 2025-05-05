package chatbot.data

import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import chatbot.config.Config

// Case class remains unchanged
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
  implicit val decoder: Decoder[CelestialObject] = deriveDecoder[CelestialObject]
  implicit val encoder: Encoder[CelestialObject] = deriveEncoder[CelestialObject]
}

object AstronomyData {
  // Fallback data as a pure value
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

  // Pure function to parse astronomy data
  def parseAstronomyData(config: Config): List[CelestialObject] = {
    if (config.dataContent.isEmpty) {
      println("Warning: Empty data content, using fallback data")
      fallbackData
    } else {
      // Trim the data content to show a preview
      val previewLength = Math.min(200, config.dataContent.length)
      val preview = config.dataContent.take(previewLength) +
        (if (config.dataContent.length > previewLength) "..." else "")
      println(s"JSON content: $preview")

      // Parse JSON with Circe using pattern matching
      decode[List[CelestialObject]](config.dataContent) match {
        case Right(parsed) =>
          println(s"Successfully parsed ${parsed.length} celestial objects")
          parsed
        case Left(error) =>
          println(s"Failed to parse astronomy.json: ${error.getMessage}, using fallback data")
          error match {
            case ParsingFailure(msg, underlying) =>
              println(s"JSON parsing error: $msg")
              println(s"Underlying exception: ${underlying.getMessage}")
            case DecodingFailure(msg, history) =>
              println(s"JSON decoding error: $msg")
              println(s"Error path: ${history.mkString(" -> ")}")
            case other =>
              println(s"Unexpected error: ${other.getMessage}")
          }
          fallbackData
      }
    }
  }

  // Pure function to get facts for a topic
  def getFacts(objects: List[CelestialObject], topic: String): Option[Map[String, String]] = {
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

  // Pure function to get planet names
  def getPlanets(objects: List[CelestialObject]): List[String] = {
    objects.filter(_.`type` == "planet").map(_.name)
  }

  // Pure function to get all objects
  def getAllObjects(objects: List[CelestialObject]): List[CelestialObject] = objects

  // Pure function to get objects by type
  def getObjectsByType(objects: List[CelestialObject], objectType: String): List[CelestialObject] = {
    objects.filter(_.`type`.toLowerCase == objectType.toLowerCase)
  }

  // Pure function to check if using fallback data
  def isUsingFallbackData(objects: List[CelestialObject]): Boolean = objects == fallbackData
}
