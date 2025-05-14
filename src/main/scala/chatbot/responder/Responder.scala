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
    "Welcome! Ask me anything about the universe!",
    "Hey there! Ready to explore the stars?",
    "Greetings, space traveler! What's on your mind?"
  )

  private val helpMessages = List(
    "I can help with:\n- Ask about topics: 'tell me about Mars'\n- Compare: 'compare Earth and Mars'\n- Size: 'how big is Earth'\n- Temperature: 'how hot is Uranus'\n- Lists: 'list planets'\n- Facts: 'random fact'\n- Quiz: 'start quiz'\n- Exit: 'exit'",
    "Try these:\n- 'tell me about black holes'\n- 'compare Jupiter and Saturn'\n- 'how far is Venus'\n- 'list galaxies'\n- 'random fact'\n- 'start quiz'\n- 'exit'"
  )

  private val unknownResponses = List(
    "I'm not sure. Try rephrasing or type 'help'.",
    "That's beyond my orbit! Try 'help' for options.",
    "I didn't understand. Maybe try 'help'?"
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
    if (facts.isEmpty) return s"Sorry, I don't have info on $topic. Try another topic!"
    val intro = s"Here's what I know about $topic:"
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
      return s"I don’t have enough info to compare $topic1 and $topic2."
    }
    val intro = s"Comparing $topic1 and $topic2:"
    val keysToCompare = List("diameter", "mass", "distance_from_sun", "surface_temperature")
      .filter(k => facts1.contains(k) && facts2.contains(k))
    val comparisons = if (keysToCompare.isEmpty) {
      "No comparable attributes found."
    } else {
      keysToCompare
        .map { key =>
          val readableKey = key.split("_").map(_.capitalize).mkString(" ")
          s"- $readableKey: $topic1 is ${facts1(key)}, while $topic2 is ${facts2(key)}"
        }
        .mkString("\n")
    }
    s"$intro\n$comparisons"
  }

  private def compareSize(topic1: String, topic2: String): String = {
    val facts1 = fetchFacts(topic1)
    val facts2 = fetchFacts(topic2)
    if (!facts1.contains("diameter") || !facts2.contains("diameter")) {
      return s"I don’t have size info for $topic1 or $topic2."
    }
    val size1 = facts1("diameter").replaceAll("[^0-9.]", "").toDouble
    val size2 = facts2("diameter").replaceAll("[^0-9.]", "").toDouble
    val result =
      if (size1 > size2) s"$topic1 is bigger than $topic2."
      else if (size2 > size1) s"$topic2 is bigger than $topic1."
      else s"$topic1 and $topic2 are the same size."
    s"Size comparison:\n- $topic1: ${facts1("diameter")}\n- $topic2: ${facts2("diameter")}\n$result"
  }

  private def getFeature(topic: String, attribute: String): String = {
    val facts = fetchFacts(topic)
    if (!facts.contains(attribute)) {
      return s"I don’t have $attribute info for $topic."
    }
    val readableKey = attribute.split("_").map(_.capitalize).mkString(" ")
    s"$topic's $readableKey is ${facts(attribute)}."
  }

  private def getRandomFact(): String = {
    val planets = dataSource.getPlanets
    if (planets.isEmpty) return "No facts available. Try asking about a planet!"
    val randomPlanet = planets(Random.nextInt(planets.size))
    val facts        = fetchFacts(randomPlanet)
    if (facts.isEmpty) return "No facts available for this planet."
    val factEntry   = facts.toList(Random.nextInt(facts.size))
    val readableKey = factEntry._1.split("_").map(_.capitalize).mkString(" ")
    s"Here's a fact about $randomPlanet: Its $readableKey is ${factEntry._2}."
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
      case "greetings" =>
        Map("message" -> Random.shuffle(greetings).head)

      case "exit" =>
        Map("message" -> "Goodbye! Safe travels through the cosmos!")

      case "help" =>
        Map("message" -> Random.shuffle(helpMessages).head)

      case "listplanets" =>
        val planets = dataSource.getPlanets
        val message = if (planets.isEmpty) "No planets found." else s"Planets: ${planets.mkString(", ")}"
        Map("message" -> message)

      case "randomfact" =>
        val message = getRandomFact()
        Map("message" -> maybeAddFollowUp(message, None))

      case "startquiz" =>
        Map("message" -> "Starting a quiz! Get ready for the first question.", "quizActive" -> "true")

      case cmd if cmd.startsWith("answerquiz_") =>
        val answer = cmd.drop("answerquiz_".length)
        Map("message" -> s"Your answer: $answer. Checking with the quiz system...")

      case cmd if cmd.startsWith("askabout_") =>
        val topic   = cmd.drop("askabout_".length).capitalize
        val facts   = fetchFacts(topic)
        val message = formatResponse(topic, facts, s"No info on $topic. Try another topic!")
        Map("message" -> maybeAddFollowUp(message, Some(topic)))

      case cmd if cmd.startsWith("compare_") =>
        val parts = cmd.drop("compare_".length).split("_")
        if (parts.length < 2) {
          Map("message" -> "Invalid comparison. Try 'compare Earth and Mars'.")
        } else {
          val topic1 = parts(0).capitalize
          val topic2 = parts(1).capitalize
          recentTopics = (List(topic1, topic2) ++ recentTopics).distinct.take(3)
          val message = generateComparison(topic1, topic2)
          Map("message" -> message)
        }

      case cmd if cmd.startsWith("bigger_") =>
        val parts = cmd.drop("bigger_".length).split("_")
        if (parts.length < 2) {
          Map("message" -> "Invalid query. Try 'which planet is bigger Earth or Mercury'.")
        } else {
          val topic1 = parts(0).capitalize
          val topic2 = parts(1).capitalize
          recentTopics = (List(topic1, topic2) ++ recentTopics).distinct.take(3)
          val message = compareSize(topic1, topic2)
          Map("message" -> message)
        }

      case cmd if cmd.startsWith("feature_") =>
        val parts = cmd.drop("feature_".length).split("_")
        if (parts.length < 2) {
          Map("message" -> "Invalid query. Try 'how big is Earth'.")
        } else {
          val topic = parts(0).capitalize
          val attr  = parts(1)
          recentTopics = (topic :: recentTopics).distinct.take(3)
          val message = getFeature(topic, attr)
          Map("message" -> maybeAddFollowUp(message, Some(topic)))
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
        Map("message" -> message)

      case cmd if cmd.startsWith("unknown_") =>
        val input = cmd.drop("unknown_".length)
        val message =
          if (input.contains("thank")) "You're welcome! What's next?"
          else Random.shuffle(unknownResponses).head
        Map("message" -> maybeAddFollowUp(message, None))

      case _ =>
        Map("message" -> "Something went wrong. Try 'help' for options.")
    }
    response
  }
}
