package chatbot.responder

import chatbot.parser.AST.Command
import chatbot.parser.AST.Command._
import chatbot.data.{AstronomyData, PlanetApiClient}
import chatbot.analytics.Analytics
import scala.util.Random

class Responder(dataSource: AstronomyData, apiClient: PlanetApiClient, analytics: Analytics) {
  private val apiFacts = apiClient.fetchPlanets()

  // Simple quiz state with expanded questions
  private val quizQuestions = List(
    ("Which planet is closest to the Sun?", "a", Map("a" -> "Mercury", "b" -> "Venus", "c" -> "Earth", "d" -> "Mars")),
    ("Which planet has the most moons?", "c", Map("a" -> "Earth", "b" -> "Mars", "c" -> "Saturn", "d" -> "Jupiter")),
    ("Which of these is a dwarf planet?", "b", Map("a" -> "Neptune", "b" -> "Pluto", "c" -> "Venus", "d" -> "Mercury")),
    (
      "Which planet is known as the 'Red Planet'?",
      "d",
      Map("a" -> "Jupiter", "b" -> "Venus", "c" -> "Mercury", "d" -> "Mars")
    ),
    (
      "Which planet has the Great Red Spot?",
      "a",
      Map("a" -> "Jupiter", "b" -> "Mars", "c" -> "Neptune", "d" -> "Uranus")
    ),
    (
      "Which planet has rings that are easily visible from Earth?",
      "b",
      Map("a" -> "Uranus", "b" -> "Saturn", "c" -> "Neptune", "d" -> "Jupiter")
    )
  )

  private var currentQuestion: Option[Int] = None
  private var userScore: Int               = 0
  private var totalQuestions: Int          = 0

  // Store user's recent topics to personalize responses
  private var recentTopics = List.empty[String]

  // Greeting variations
  private val greetings = List(
    "Hello there! How can I help with your astronomy questions today?",
    "Hi! I'm your astronomy assistant. What would you like to know about the cosmos?",
    "Greetings, stargazer! What astronomical wonders can I help you explore?",
    "Welcome! The universe awaits your questions. What would you like to discover?",
    "Hey there! Ready to explore the stars together? What's on your mind?"
  )

  // Help message variations
  private val helpMessages = List(
    "I'm your astronomy guide! You can ask me things like:\n• \"Tell me about Jupiter\"\n• \"Compare Earth and Mars\"\n• \"Show me all the planets\"\n• \"Give me a random space fact\"\n• \"Start a quiz\"\nWhat would you like to explore?",
    "Here's how I can help with your cosmic curiosities:\n• Ask about celestial bodies: \"What is Saturn like?\"\n• Compare objects: \"How does Earth compare to Venus?\"\n• See lists: \"Show me all the planets\"\n• Learn random facts: \"Tell me something interesting\"\n• Test your knowledge: \"Start a quiz\"\nWhat interests you?",
    "Your personal guide to the cosmos! Try these:\n• \"Tell me about black holes\"\n• \"How does Jupiter compare to Saturn?\"\n• \"List all the planets\"\n• \"Share a cool astronomy fact\"\n• \"Let's do a space quiz\"\nWhat part of the universe shall we explore?"
  )

  // Unknown command variations
  private val unknownResponses = List(
    "I'm not quite sure what you mean by that. Could you try rephrasing or check out what I can do with 'help'?",
    "Hmm, that's a bit outside my orbit. Try asking about planets, stars, or use 'help' to see what I can do!",
    "I didn't catch that. Maybe try asking about a specific planet or star? Or say 'help' to see all options.",
    "Sorry, I'm having trouble understanding that request. Perhaps try asking about astronomy in a different way?",
    "That's one question that's got me lost in space! Could you try asking something about astronomy differently?"
  )

  // Enthusiasm indicators
  private val enthusiasticPhrases = List(
    "Fascinating! ",
    "Amazing! ",
    "Wow! ",
    "Incredible! ",
    "How cool - ",
    "Did you know? ",
    "Here's something interesting: ",
    "You might be surprised to learn that "
  )

  // Transition phrases
  private val transitionPhrases = List(
    "Speaking of which, ",
    "On that note, ",
    "By the way, ",
    "Interestingly, ",
    "Also worth mentioning, ",
    "It's worth noting that "
  )

  // Follow-up suggestions
  private def getFollowUpSuggestion(topic: String): String = {
    val suggestions = List(
      s"Would you like to know more about $topic?",
      s"Anything specific about $topic you'd like to explore?",
      s"I have more information about $topic if you're interested!",
      s"Want to compare $topic with another celestial body?",
      "What else would you like to discover about our universe?"
    )
    Random.shuffle(suggestions).head
  }

  // Personalization based on user history
  private def personalizeResponse(response: String, topic: Option[String] = None): String = {
    // Update recent topics if a new one is provided
    topic.foreach(t => recentTopics = (t :: recentTopics).distinct.take(5))

    // If we have recent topics, maybe reference them
    if (recentTopics.nonEmpty && Random.nextDouble() < 0.3) {
      val prevTopic = Random.shuffle(recentTopics).head
      val personalizedIntros = List(
        s"Since you were interested in $prevTopic earlier, you might like to know that ",
        s"Following up on $prevTopic, ",
        s"This reminds me of $prevTopic, where "
      )
      return Random.shuffle(personalizedIntros).head + response.toLowerCase
    }

    // Randomly add enthusiasm or interesting framing
    if (Random.nextDouble() < 0.4) {
      Random.shuffle(enthusiasticPhrases).head + response.toLowerCase
    } else {
      response
    }
  }

  // Helper for natural language variations
  private def getVariation[T](items: List[T]): T = {
    items(Random.nextInt(items.size))
  }

  // Add a conversational follow-up question sometimes
  private def maybeAddFollowUp(response: String, topic: Option[String] = None): String = {
    if (Random.nextDouble() < 0.4) {
      val followUp = topic match {
        case Some(t) => getFollowUpSuggestion(t)
        case None =>
          getVariation(
            List(
              "What else would you like to know?",
              "Anything else you're curious about?",
              "What other cosmic mysteries should we explore?",
              "Any other astronomy questions I can help with?"
            )
          )
      }
      s"$response\n\n$followUp"
    } else {
      response
    }
  }

  // Format facts in a more engaging way
  private def formatFacts(facts: Map[String, String], topic: String): String = {
    if (facts.isEmpty)
      return s"I don't have much information about $topic yet, but I'd be happy to learn more together!"

    val intro = getVariation(
      List(
        s"Here's what I know about $topic:",
        s"Let me tell you about $topic:",
        s"When it comes to $topic, I can share that:",
        s"$topic is fascinating! Here's what I know:"
      )
    )

    val formattedFacts = facts
      .map { case (k, v) =>
        // Convert keys from camelCase or snake_case to readable text
        val readableKey = k
          .replaceAll("([A-Z])", " $1")
          .replaceAll("_", " ")
          .trim
          .toLowerCase
          .capitalize

        s"• $readableKey: $v"
      }
      .mkString("\n")

    s"$intro\n$formattedFacts"
  }

  // Generate a more natural comparison
  private def generateNaturalComparison(
    entity1: String,
    entity2: String,
    facts1: Map[String, String],
    facts2: Map[String, String]
  ): String = {
    val intro = getVariation(
      List(
        s"Let's compare $entity1 and $entity2:",
        s"Here's how $entity1 stacks up against $entity2:",
        s"When comparing $entity1 and $entity2, I can tell you that:",
        s"$entity1 vs $entity2 - here's what I know:"
      )
    )

    // Get some common keys to compare
    val commonKeys       = facts1.keySet.intersect(facts2.keySet)
    val keysToPrioritize = List("diameter", "mass", "distance", "orbitalPeriod", "rotationPeriod", "temperature")

    val prioritizedKeys = keysToPrioritize.filter(commonKeys.contains) ++
      (commonKeys -- keysToPrioritize.toSet).toList.sorted

    if (prioritizedKeys.isEmpty) {
      return s"I'd love to compare $entity1 and $entity2, but I don't have enough information about both to make a meaningful comparison."
    }

    val comparisons = prioritizedKeys
      .take(3)
      .map { key =>
        val readableKey = key
          .replaceAll("([A-Z])", " $1")
          .replaceAll("_", " ")
          .trim
          .toLowerCase
          .capitalize

        val value1 = facts1(key)
        val value2 = facts2(key)

        s"• $readableKey: $entity1 is $value1, while $entity2 is $value2"
      }
      .mkString("\n")

    s"$intro\n$comparisons"
  }

  def respond(command: Command): Map[String, String] = {
    analytics.logInteraction(command) // Log every command

    val response = command match {
      case PropertyQuery(entity, property) =>
        Map(
          "message" -> s"I'm not yet equipped to handle property queries like '$property' for '$entity'. Stay tuned for updates!"
        )

      case AstronomicalEvent(event) =>
        Map(
          "message" -> s"Details about astronomical events like '$event' are not available yet. I'll work on learning more!"
        )

      case NightSkyInfo(location) =>
        Map(
          "message" -> s"I currently don't have information about the night sky for '$location'. Let me know if there's something else I can assist with!"
        )

      case FunFact(topic) =>
        Map(
          "message" -> s"Fun facts about '$topic' are not in my database yet. Try asking about planets or stars instead!"
        )
      case Help =>
        Map("message" -> personalizeResponse(getVariation(helpMessages)))

      case AskAbout(topic) =>
        val normalizedTopic = topic.trim.toLowerCase.capitalize
        val fact = dataSource.getFacts(normalizedTopic).orElse {
          apiFacts.get(normalizedTopic.toLowerCase)
        }

        val message = fact match {
          case Some(facts) =>
            val formattedResponse = formatFacts(facts, normalizedTopic)
            personalizeResponse(formattedResponse, Some(normalizedTopic))
          case None =>
            val notFoundResponses = List(
              s"I don't have information about $normalizedTopic yet, but I'd be happy to learn more! Try asking about planets or stars I might know better.",
              s"$normalizedTopic is a fascinating topic, but I don't have those details yet. Would you like to know about something else in our solar system?",
              s"Hmm, $normalizedTopic isn't in my database yet. I'm more knowledgeable about planets, stars, and major celestial bodies. Can I tell you about one of those instead?"
            )
            getVariation(notFoundResponses)
        }

        Map("message" -> maybeAddFollowUp(message, Some(normalizedTopic)))

      case ListPlanets =>
        val planets = dataSource.getPlanets
        val introductions = List(
          "Our solar system contains eight planets: ",
          "The planets in our solar system are: ",
          "Going from the Sun outward, our planets are: ",
          "The eight planets that orbit our Sun are: "
        )

        val message = if (planets.isEmpty) {
          "Hmm, my planetary database seems empty right now. That's strange - I should know all about our solar system's planets!"
        } else {
          val intro      = getVariation(introductions)
          val planetList = planets.mkString(", ")
          val funFact = getVariation(
            List(
              "Did you know that Pluto was reclassified as a dwarf planet in 2006?",
              "Mercury is the smallest planet while Jupiter is the largest.",
              "Venus is the hottest planet despite not being closest to the Sun.",
              "Earth is the only planet known to support life!"
            )
          )

          s"$intro$planetList. $funFact"
        }

        Map("message" -> personalizeResponse(message))

      case ListCategory(category) =>
        val normalizedCategory = category.toLowerCase
        val items              = if (normalizedCategory == "planets") dataSource.getPlanets else List()

        val message = if (items.isEmpty) {
          val noItemsResponses = List(
            s"I don't have a list of $normalizedCategory in my database yet. I'm most familiar with planets in our solar system.",
            s"I'd love to tell you about $normalizedCategory, but I don't have that information yet. My specialty is planets - would you like to know about those instead?",
            s"My $normalizedCategory database needs an update! For now, I can tell you all about planets if you're interested."
          )
          getVariation(noItemsResponses)
        } else {
          val introductions = List(
            s"Here are the $normalizedCategory I know about: ",
            s"The $normalizedCategory in our solar system include: ",
            s"When it comes to $normalizedCategory, I can list: ",
            s"The $normalizedCategory you might be interested in are: "
          )

          val intro    = getVariation(introductions)
          val itemList = items.mkString(", ")

          s"$intro$itemList"
        }

        Map("message" -> personalizeResponse(message))

      case Compare(entity1, entity2) =>
        val normalizedEntity1 = entity1.trim.toLowerCase.capitalize
        val normalizedEntity2 = entity2.trim.toLowerCase.capitalize

        // Update recent topics with both entities
        recentTopics = (List(normalizedEntity1, normalizedEntity2) ++ recentTopics).distinct.take(5)

        val facts1 = dataSource
          .getFacts(normalizedEntity1)
          .orElse(apiFacts.get(normalizedEntity1.toLowerCase))
          .getOrElse(Map.empty)
        val facts2 = dataSource
          .getFacts(normalizedEntity2)
          .orElse(apiFacts.get(normalizedEntity2.toLowerCase))
          .getOrElse(Map.empty)

        val message = (facts1.nonEmpty, facts2.nonEmpty) match {
          case (true, true) =>
            generateNaturalComparison(normalizedEntity1, normalizedEntity2, facts1, facts2)
          case (true, false) =>
            s"I'd love to compare $normalizedEntity1 and $normalizedEntity2, but I only have information about $normalizedEntity1. Would you like to learn more about $normalizedEntity1 specifically?"
          case (false, true) =>
            s"I'd love to compare $normalizedEntity1 and $normalizedEntity2, but I only have information about $normalizedEntity2. Would you like to learn more about $normalizedEntity2 specifically?"
          case (false, false) =>
            getVariation(
              List(
                s"I don't have enough data to compare $normalizedEntity1 and $normalizedEntity2. I'm most knowledgeable about planets and major celestial bodies.",
                s"Neither $normalizedEntity1 nor $normalizedEntity2 are in my database yet. Would you like to compare planets instead? I know quite a bit about those!",
                s"I'm not familiar enough with $normalizedEntity1 or $normalizedEntity2 to make a good comparison. Can I interest you in comparing some planets instead?"
              )
            )
        }

        Map("message" -> personalizeResponse(message))

      case RandomFact =>
        val allFacts = dataSource.getPlanets.flatMap(dataSource.getFacts).flatMap(_.values) ++
          apiFacts.values.flatMap(_.values)

        val message = if (allFacts.isEmpty) {
          "I'd love to share a fascinating space fact, but my fact database seems to be empty right now. Try asking me about specific planets instead!"
        } else {
          val factIntros = List(
            "Here's a fascinating space fact: ",
            "Did you know that ",
            "Space is amazing! For instance, ",
            "Here's something that might blow your mind: ",
            "Astronomy fact of the day: ",
            "One of my favorite cosmic facts is that "
          )

          val intro = getVariation(factIntros)
          val fact  = allFacts(Random.nextInt(allFacts.size))

          s"$intro$fact"
        }

        Map("message" -> maybeAddFollowUp(personalizeResponse(message)))

      case StartQuiz =>
        // Select a random question
        val qIdx = Random.nextInt(quizQuestions.size)
        currentQuestion = Some(qIdx)
        totalQuestions += 1

        val (question, _, options) = quizQuestions(qIdx)
        val formattedOptions       = options.map { case (k, v) => s"$k: $v" }.mkString("\n")

        val quizIntros = List(
          s"Let's test your astronomy knowledge! Here's your question:\n\n$question\n\n$formattedOptions",
          s"Time for a cosmic quiz! Question ${totalQuestions}:\n\n$question\n\n$formattedOptions",
          s"Let's see how much you know about space! Try this one:\n\n$question\n\n$formattedOptions",
          s"Challenge time! Question ${totalQuestions}:\n\n$question\n\n$formattedOptions"
        )

        Map("message" -> getVariation(quizIntros))

      case AnswerQuiz(answer) =>
        val response = currentQuestion match {
          case Some(qIdx) =>
            val (question, correctAnswer, options) = quizQuestions(qIdx)
            val normalizedAnswer                   = answer.trim.toLowerCase

            // Check if answer matches either the letter or the full text
            val isCorrect = normalizedAnswer == correctAnswer.toLowerCase ||
              options.get(correctAnswer).exists(_.toLowerCase == normalizedAnswer)

            if (isCorrect) {
              userScore += 1
              val correctResponses = List(
                s"That's correct! ${options(correctAnswer)} is the right answer. You now have $userScore out of $totalQuestions right.",
                s"Exactly right! ${options(correctAnswer)} is correct. Your cosmic knowledge is impressive!",
                s"Well done! ${options(correctAnswer)} is correct. You're getting good at this!",
                s"Spot on! ${options(correctAnswer)} is the answer. Your space knowledge is stellar!"
              )
              getVariation(correctResponses) + "\n\nWant to try another? Just say 'start quiz'."
            } else {
              val wrongResponses = List(
                s"Not quite. The correct answer is ${options(correctAnswer)}. No worries though, space is complicated!",
                s"That's not it this time. The answer is ${options(correctAnswer)}. Space has so many fascinating facts to learn!",
                s"Sorry, that's incorrect. The right answer is ${options(correctAnswer)}. Don't worry, even astronomers get things wrong sometimes!",
                s"Not exactly. It's actually ${options(correctAnswer)}. These questions can be tricky!"
              )
              getVariation(wrongResponses) + "\n\nWant to try another question? Say 'start quiz'."
            }

          case None =>
            getVariation(
              List(
                "There's no active quiz question right now. Want to start one? Just say 'start quiz'!",
                "I don't have a question for you at the moment. Type 'start quiz' to begin the cosmic challenge!",
                "You need to start a quiz first! Just say 'start quiz' and I'll test your astronomy knowledge.",
                "It seems you're eager to answer, but there's no quiz active. Try 'start quiz' to get started!"
              )
            )
        }

        currentQuestion = None // Reset after answer
        Map("message" -> response)

      case Unknown(input) =>
        Map("message" -> getVariation(unknownResponses))

      case Exit =>
        val exitResponses = List(
          "Goodbye! May the stars light your way until we meet again!",
          "Farewell, space explorer! Come back anytime for more cosmic adventures.",
          "Safe travels through the cosmos! Come back soon!",
          "It's been stellar chatting with you! See you among the stars!",
          "Goodbye for now! The universe will be waiting for your return!"
        )
        Map("message" -> getVariation(exitResponses))

      case Greet =>
        Map("message" -> getVariation(greetings))

      case PlanetInfo(planet) =>
        val normalizedPlanet = planet.trim.toLowerCase.capitalize
        val facts            = dataSource.getFacts(normalizedPlanet).getOrElse(Map.empty)
        val message          = formatFacts(facts, normalizedPlanet)
        Map("message" -> maybeAddFollowUp(personalizeResponse(message, Some(normalizedPlanet)), Some(normalizedPlanet)))

      case StarInfo(star) =>
        val normalizedStar = star.trim.toLowerCase.capitalize
        val facts          = dataSource.getFacts(normalizedStar).getOrElse(Map.empty)
        val message        = formatFacts(facts, normalizedStar)
        Map("message" -> maybeAddFollowUp(personalizeResponse(message, Some(normalizedStar)), Some(normalizedStar)))

      case ConstellationInfo(constellation) =>
        val normalizedConstellation = constellation.trim.toLowerCase.capitalize
        val facts                   = dataSource.getFacts(normalizedConstellation).getOrElse(Map.empty)
        val message                 = formatFacts(facts, normalizedConstellation)
        Map(
          "message" -> maybeAddFollowUp(
            personalizeResponse(message, Some(normalizedConstellation)),
            Some(normalizedConstellation)
          )
        )

      case Distance(entity1, entity2) =>
        val normalizedEntity1 = entity1.trim.toLowerCase.capitalize
        val normalizedEntity2 = entity2.trim.toLowerCase.capitalize

        // Update recent topics with both entities
        recentTopics = (List(normalizedEntity1, normalizedEntity2) ++ recentTopics).distinct.take(5)

        val distanceResponses = List(
          s"I'd love to tell you the exact distance between $normalizedEntity1 and $normalizedEntity2, but that calculation depends on their positions which change over time!",
          s"The distance between $normalizedEntity1 and $normalizedEntity2 varies as they orbit the Sun. If you're looking for average distances, I can tell you about each individually.",
          s"Calculating the distance between $normalizedEntity1 and $normalizedEntity2 is complex since both are moving through space. Would you like to know about either one specifically?"
        )

        Map("message" -> getVariation(distanceResponses))
    }

    response
  }
}
