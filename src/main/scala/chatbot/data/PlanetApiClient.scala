package chatbot.data

import sttp.client3._
import upickle.default._

class PlanetApiClient {
  private val backend = HttpClientSyncBackend()

  def fetchPlanets(): Map[String, Map[String, String]] = {
    val request = basicRequest
      .get(uri"https://api.le-systeme-solaire.net/rest/bodies?filter[]=isPlanet,eq,true")
      .response(asString)

    request.send(backend).body match {
      case Right(json) => parseJson(json)
      case Left(_)     => Map.empty[String, Map[String, String]]
    }
  }

  private def parseJson(json: String): Map[String, Map[String, String]] = {
    try {
      val data   = ujson.read(json)
      val bodies = data("bodies").arr
      bodies.map { body =>
        val name = body("englishName").str
        val facts = Map(
          "diameter" -> ((body("equaRadius").num * 2).toString + " km"),
          "mass" -> {
            val massValue    = body("mass")("massValue").num
            val massExponent = body("mass")("massExponent").num
            (massValue * math.pow(10, massExponent)).toString + " kg"
          }
        )
        name -> facts
      }.toMap
    } catch {
      case _: Exception => Map("Earth" -> Map("diameter" -> "12742 km", "mass" -> "5.9724e24 kg"))
    }
  }
}
