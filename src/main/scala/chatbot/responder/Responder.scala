package chatbot.responder

import chatbot.data.AstronomyData
import chatbot.analytics.Analytics
import chatbot.quiz.QuizManager
import scala.util.Random

class Responder(dataSource: AstronomyData, analytics: Analytics, quizManager: QuizManager) {
  private var recentTopics = List.empty[String]

  private val greetings = List(
    "Hello! How can I help with astronomy today?",
    "Hi! What would you like to know about space?",
    "Welcome! Ask me anything about the universe!"
  )

  private val helpMessages = List(
    "I can help with:\n- Ask about topics: 'tell me about Mars'\n- Compare: 'compare Earth and Mars'\n- Lists: 'list planets'\n- Facts: 'random fact'\n- Quiz: 'start quiz'\n- Exit: 'exit'",
    "Try these:\n- 'tell me about black holes'\n- 'compare Jupiter and Saturn'\n- 'list galaxies'\n- 'random fact'\n- 'start quiz'\n- 'exit'"
  )

  private val unknownResponses = List(
    "I’m not sure. Try rephrasing or type 'help'.",
    "That’s beyond my orbit! Try 'help' for options.",
    "I didn’t understand. Maybe try 'help'?"
  )

  private def fetchFacts(topic: String): Map[String, String] = {
    try {
      dataSource.getFacts(topic.trim.toLowerCase.capitalize).getOrElse(Map.empty)
    } catch {
      case _: Exception =>
        println(s"Error fetching facts for $topic")
        Map.empty
    }
  }

  private def formatResponse(topic: String, facts: Map[String, String], defaultMsg: String): String = {
    if (facts.isEmpty) return s"Sorry, I don’t have info on $topic. Try another topic!"
    val intro = s"Here’s what I know about $topic:"
    val formattedFacts = facts
      .map { case (k, v) =>
        val readableKey = k.split("_").map(_.capitalize).mkString(" ")
        s"- $readableKey: $v"
      }
      .mkString("\n")
    s"$intro\n$formattedFacts"
  }

  private def generateComparison(topic1: String, topic2: String): String = {
    val facts1 = fetchFacts(topic1)
    val facts2 = fetchFacts(topic2)
    if (facts1.isEmpty || facts2.isEmpty) {
      return s"I dont have enough info to compare $topic1 and $topic2."
    }
    val intro = s"Comparing $topic1 and $topic2:"
    val keysToCompare =
      List("diameter", "mass", "distance from sun").filter(k => facts1.contains(k) && facts2.contains(k))
    val comparisons =
      if (keysToCompare.isEmpty) ""
      else {
        keysToCompare
          .take(3)
          .map { key =>
            val readableKey = key.split("_").map(_.capitalize).mkString(" ")
            s"- $readableKey: $topic1 is ${facts1(key)}, while $topic2 is ${facts2(key)}"
          }
          .mkString("\n")
      }
    s"$intro\n$comparisons"
  }

  private def personalizeResponse(response: String, topic: Option[String] = None): String = {
    topic.foreach(t => recentTopics = (t :: recentTopics).distinct.take(3))
    if (recentTopics.nonEmpty && Random.nextDouble() < 0.3) {
      val prevTopic = recentTopics.head
      s"Since you asked about $prevTopic, $response"
    } else {
      response
    }
  }

  private def maybeAddFollowUp(response: String, topic: Option[String] = None): String = {
    if (Random.nextDouble() < 0.4) {
      val followUp = topic match {
        case Some(t) => s"Want to know more about $t?"
        case None    => "What else would you like to explore?"
      }
      s"$response\n\n$followUp"
    } else {
      response
    }
  }

  def respond(command: String): Map[String, String] = {
    analytics.logInteraction(command)
    val response = command match {
      case "exit" =>
        Map("message" -> "Goodbye! Safe travels through the cosmos!")

      case "help" =>
        Map("message" -> personalizeResponse(Random.shuffle(helpMessages).head, None))

      case "listplanets" =>
        val planets = dataSource.getPlanets
        val message = if (planets.isEmpty) "No planets found." else s"Planets: ${planets.mkString(", ")}"
        Map("message" -> personalizeResponse(message, None))

      case "randomfact" =>
        val allFacts = dataSource.getPlanets.flatMap(planet => fetchFacts(planet).values)
        val message =
          if (allFacts.isEmpty) "No facts available. Try a planet!"
          else s"Fact: ${allFacts(Random.nextInt(allFacts.size))}"
        Map("message" -> maybeAddFollowUp(personalizeResponse(message, None), None))

      case "startquiz" =>
        Map("message" -> "Starting a quiz! Get ready for the first question.", "quizActive" -> "true")

      case cmd if cmd.startsWith("answerquiz_") =>
        val answer = cmd.drop("answerquiz_".length)
        Map("message" -> s"Your answer: $answer. Checking with the quiz system...")

      case cmd if cmd.startsWith("askabout_") =>
        val topic   = cmd.drop("askabout_".length).capitalize
        val facts   = fetchFacts(topic)
        val message = formatResponse(topic, facts, s"No info on $topic. Try another topic!")
        Map("message" -> maybeAddFollowUp(personalizeResponse(message, Some(topic)), Some(topic)))

      case cmd if cmd.startsWith("compare_") =>
        val parts = cmd.drop("compare_".length).split("_")
        if (parts.length < 2) {
          Map("message" -> "Invalid comparison. Try 'compare Earth and Mars'.")
        } else {
          val topic1 = parts(0).capitalize
          val topic2 = parts(1).capitalize
          recentTopics = (List(topic1, topic2) ++ recentTopics).distinct.take(3)
          val message = generateComparison(topic1, topic2)
          Map("message" -> personalizeResponse(message, None))
        }

      case cmd if cmd.startsWith("listcategory_") =>
        val category = cmd.drop("listcategory_".length)
        val items = category match {
          case "stars"          => List("Sun", "Sirius", "Vega")
          case "constellations" => List("Orion", "Big Dipper")
          case "galaxies"       => List("Milky Way", "Andromeda")
          case "black holes"    => List("Sagittarius A*")
          case "moons"          => List("Luna", "Phobos")
          case _                => List(s"No items for '$category'")
        }
        val message = s"$category: ${items.mkString(", ")}"
        Map("message" -> personalizeResponse(message, None))

      case cmd if cmd.startsWith("unknown_") =>
        val input = cmd.drop("unknown_".length)
        val message =
          if (input.contains("thank")) "You’re welcome! What’s next?"
          else Random.shuffle(unknownResponses).head
        Map("message" -> maybeAddFollowUp(message, None))

      case _ =>
        Map("message" -> "Something went wrong. Try 'help' for options.")
    }
    response
  }
}
