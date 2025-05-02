package chatbot.data

import scala.util.{Try, Success, Failure}

/** A client for accessing external planetary data APIs In a real implementation, this would make HTTP requests to an
  * astronomy API
  */
class PlanetApiClient {
  // Simulated API response data
  private val planetApiData = Map(
    "mercury" -> Map(
      "distance_from_sun"   -> "57.9 million km",
      "orbital_period"      -> "88 Earth days",
      "rotation_period"     -> "58.6 Earth days",
      "surface_temperature" -> "-173°C to 427°C",
      "atmosphere"          -> "Virtually none, extremely thin"
    ),
    "venus" -> Map(
      "distance_from_sun"   -> "108.2 million km",
      "orbital_period"      -> "225 Earth days",
      "rotation_period"     -> "243 Earth days (retrograde)",
      "surface_temperature" -> "462°C (average)",
      "atmosphere"          -> "Very dense, 96% carbon dioxide"
    ),
    "earth" -> Map(
      "distance_from_sun"   -> "149.6 million km",
      "orbital_period"      -> "365.25 days",
      "rotation_period"     -> "23.9 hours",
      "surface_temperature" -> "-88°C to 58°C",
      "atmosphere"          -> "78% nitrogen, 21% oxygen, 1% other gases"
    ),
    "mars" -> Map(
      "distance_from_sun"   -> "227.9 million km",
      "orbital_period"      -> "687 Earth days",
      "rotation_period"     -> "24.6 hours",
      "surface_temperature" -> "-153°C to 20°C",
      "atmosphere"          -> "Thin, 95% carbon dioxide"
    ),
    "jupiter" -> Map(
      "distance_from_sun"   -> "778.5 million km",
      "orbital_period"      -> "11.9 Earth years",
      "rotation_period"     -> "9.9 hours",
      "surface_temperature" -> "-145°C (cloud tops)",
      "atmosphere"          -> "Hydrogen and helium"
    ),
    "saturn" -> Map(
      "distance_from_sun"   -> "1.4 billion km",
      "orbital_period"      -> "29.5 Earth years",
      "rotation_period"     -> "10.7 hours",
      "surface_temperature" -> "-178°C (cloud tops)",
      "atmosphere"          -> "Hydrogen and helium, with traces of ammonia and methane"
    ),
    "uranus" -> Map(
      "distance_from_sun"   -> "2.9 billion km",
      "orbital_period"      -> "84 Earth years",
      "rotation_period"     -> "17.2 hours (retrograde)",
      "surface_temperature" -> "-224°C (cloud tops)",
      "atmosphere"          -> "Hydrogen, helium, and methane"
    ),
    "neptune" -> Map(
      "distance_from_sun"   -> "4.5 billion km",
      "orbital_period"      -> "165 Earth years",
      "rotation_period"     -> "16.1 hours",
      "surface_temperature" -> "-218°C (cloud tops)",
      "atmosphere"          -> "Hydrogen, helium, and methane"
    ),
    "pluto" -> Map(
      "distance_from_sun"   -> "5.9 billion km (average)",
      "orbital_period"      -> "248 Earth years",
      "rotation_period"     -> "6.4 Earth days (retrograde)",
      "surface_temperature" -> "-229°C (average)",
      "atmosphere"          -> "Thin, nitrogen with traces of methane and carbon monoxide"
    )
  )

  /** Get additional planet data from the API
    * @param planetName
    *   The name of the planet
    * @return
    *   Map of planet data properties, or None if not found
    */
  def getPlanetData(planetName: String): Option[Map[String, String]] = {
    planetApiData.get(planetName.toLowerCase)
  }

  /** Simulates making an API request for astronomical data In a real implementation, this would make an HTTP request
    */
  def fetchAstronomicalData(query: String): Try[Map[String, String]] = {
    // In a real implementation, this would make an HTTP request to an astronomy API
    // For demonstration, we'll just return some mock data
    Success(
      Map(
        "query"     -> query,
        "timestamp" -> System.currentTimeMillis().toString,
        "data"      -> s"Mock astronomical data for: $query"
      )
    )
  }
}
