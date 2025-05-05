package chatbot.responder

import scala.util.Random

object Responder {
  // Common response templates
  val greetings = List(
    "Hello! How can I help with astronomy today?",
    "Hi! What would you like to know about space?",
    "Welcome! Ask me anything about the universe!",
    "Hey there! Ready to explore the stars?",
    "Greetings, space traveler! What's on your mind?"
  )

  val helpMessages = List(
    "I can help with:\n- Ask about topics: 'tell me about Mars'\n- Compare: 'compare Earth and Mars'\n- Lists: 'list planets'\n- Facts: 'random fact'\n- Quiz: 'start quiz'\n- Exit: 'exit'",
    "Try these:\n- 'tell me about black holes'\n- 'compare Jupiter and Saturn'\n- 'list galaxies'\n- 'random fact'\n- 'start quiz'\n- 'exit'"
  )

  val unknownResponses = List(
    "I'm not sure. Try rephrasing or type 'help'.",
    "That's beyond my orbit! Try 'help' for options.",
    "I didn't understand. Maybe try 'help'?"
  )

  // Fetch facts for a topic from the data source
  def fetchFacts(topic: String, dataSource: Map[String, Map[String, String]]): Map[String, String] = {
    dataSource.getOrElse(topic.trim.toLowerCase.capitalize, Map.empty)
  }

  // Format a response for a topic
  def formatResponse(topic: String, facts: Map[String, String]): String = {
    if (facts.isEmpty) {
      s"Sorry, I don't have info on $topic. Try another topic!"
    } else {
      val intro = s"Here's what I know about $topic:"
      val formattedFacts = facts
        .map { case (k, v) =>
          val readableKey = k.split("_").map(_.capitalize).mkString(" ")
          s"- $readableKey: $v"
        }
        .mkString("\n")
      s"$intro\n$formattedFacts"
    }
  }

  // Generate a comparison between two topics
  def generateComparison(topic1: String, topic2: String, dataSource: Map[String, Map[String, String]]): String = {
    val facts1 = fetchFacts(topic1, dataSource)
    val facts2 = fetchFacts(topic2, dataSource)

    if (facts1.isEmpty || facts2.isEmpty) {
      return s"I don't have enough info to compare $topic1 and $topic2."
    }

    val intro = s"Comparing $topic1 and $topic2:"
    val keysToCompare =
      List("diameter", "mass", "distance_from_sun").filter(k => facts1.contains(k) && facts2.contains(k))

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

  // Get a random fact about a planet
  def getRandomFact(dataSource: Map[String, Map[String, String]]): String = {
    val planets = dataSource.keys.toList
    if (planets.isEmpty) {
      return "No facts available. Try asking about a planet!"
    }

    val randomPlanet = planets(Random.nextInt(planets.size))
    val facts        = fetchFacts(randomPlanet, dataSource)

    if (facts.isEmpty) {
      return "No facts available for this planet."
    }

    val factEntry   = facts.toList(Random.nextInt(facts.size))
    val readableKey = factEntry._1.split("_").map(_.capitalize).mkString(" ")
    s"Here's a fact about $randomPlanet: Its $readableKey is ${factEntry._2}."
  }

  // Maybe add a follow-up question
  def maybeAddFollowUp(response: String, topic: Option[String] = None): String = {
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

  // Generate a response based on the command
  def respond(
    command: String,
    dataSource: Map[String, Map[String, String]],
    analyticsData: Map[String, Int]
  ): Map[String, String] = {
    // Update analytics data (this would be handled by Analytics module)
    val updatedAnalytics = updateAnalytics(command, analyticsData)

    // Pattern match on the command
    val response = command match {
      case "greetings" =>
        Map("message" -> randomElement(greetings))

      case "exit" =>
        Map("message" -> "Goodbye! Safe travels through the cosmos!")

      case "help" =>
        Map("message" -> randomElement(helpMessages))

      case "listplanets" =>
        val planets = dataSource.keys.toList
        val message = if (planets.isEmpty) "No planets found." else s"Planets: ${planets.mkString(", ")}"
        Map("message" -> message)

      case "randomfact" =>
        val message = getRandomFact(dataSource)
        Map("message" -> maybeAddFollowUp(message, None))

      case "startquiz" =>
        Map("message" -> "Starting a quiz! Get ready for the first question.", "quizActive" -> "true")

      case cmd if cmd.startsWith("answerquiz_") =>
        val answer = cmd.drop("answerquiz_".length)
        Map("message" -> s"Your answer: $answer. Checking with the quiz system...")

      case cmd if cmd.startsWith("askabout_") =>
        val topic   = cmd.drop("askabout_".length).capitalize
        val facts   = fetchFacts(topic, dataSource)
        val message = formatResponse(topic, facts)
        Map("message" -> maybeAddFollowUp(message, Some(topic)))

      case cmd if cmd.startsWith("compare_") =>
        val parts = cmd.drop("compare_".length).split("_")
        if (parts.length < 2) {
          Map("message" -> "Invalid comparison. Try 'compare Earth and Mars'.")
        } else {
          val topic1  = parts(0).capitalize
          val topic2  = parts(1).capitalize
          val message = generateComparison(topic1, topic2, dataSource)
          Map("message" -> message)
        }

      case cmd if cmd.startsWith("listcategory_") =>
        val category = cmd.drop("listcategory_".length)
        val items    = getItemsForCategory(category)
        val message  = s"$category: ${items.mkString(", ")}"
        Map("message" -> message)

      case cmd if cmd.startsWith("unknown_") =>
        val input = cmd.drop("unknown_".length)
        val message =
          if (input.contains("thank")) "You're welcome! What's next?"
          else randomElement(unknownResponses)
        Map("message" -> maybeAddFollowUp(message, None))

      case _ =>
        Map("message" -> "Something went wrong. Try 'help' for options.")
    }

    response
  }

  // Helper function to get items for a category
  def getItemsForCategory(category: String): List[String] = {
    category match {
      case "stars"          => List("Sun", "Sirius", "Vega")
      case "constellations" => List("Orion", "Big Dipper")
      case "galaxies"       => List("Milky Way", "Andromeda")
      case "black holes"    => List("Sagittarius A*")
      case "moons"          => List("Luna", "Phobos")
      case _                => List(s"No items for '$category'")
    }
  }

  // Helper function to update analytics
  def updateAnalytics(command: String, analyticsData: Map[String, Int]): Map[String, Int] = {
    val cmdType = command.split("_").headOption.getOrElse("unknown")
    analyticsData.updated(cmdType, analyticsData.getOrElse(cmdType, 0) + 1)
  }

  // Helper function to get a random element from a list
  def randomElement[T](list: List[T]): T = {
    list(Random.nextInt(list.size))
  }
}
