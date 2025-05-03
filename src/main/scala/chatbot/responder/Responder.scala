package chatbot.responder

import chatbot.parser.AST.Command
import chatbot.parser.AST.Command._
import chatbot.data.{AstronomyData, PlanetApiClient}
import chatbot.analytics.Analytics
import chatbot.quiz.QuizManager
import scala.util.Random
import scala.util.control.NonFatal

class Responder(dataSource: AstronomyData, apiClient: PlanetApiClient, analytics: Analytics, quizManager: QuizManager) {
  private var recentTopics = List.empty[String]

  private val greetings = List(
    "Hello there! How can I help with your astronomy questions today?",
    "Hi! I'm your astronomy assistant. What would you like to know about the cosmos?",
    "Greetings, stargazer! What astronomical wonders can I help you explore?",
    "Welcome! The universe awaits your questions. What would you like to discover?",
    "Hey there! Ready to explore the stars together? What's on your mind?"
  )

  private val helpMessages = List(
    """I'm your astronomy guide! You can:
      |• Ask about celestial bodies: "Tell me about Jupiter"
      |• Compare objects: "Compare Earth and Mars"
      |• List items: "List planets"
      |• Get facts: "Fact about black holes"
      |• Start a quiz: "Start quiz"
      |• Exit: "Exit"
      |What would you like to explore?""".stripMargin,
    """Here's how I can help:
      |• Ask about planets, stars, etc.: "What is Saturn like?"
      |• Compare: "How does Earth compare to Venus?"
      |• Lists: "List constellations"
      |• Random facts: "Random fact"
      |• Quiz: "Start quiz"
      |• Exit: "Exit"
      |What interests you?""".stripMargin,
    """Your cosmic guide! Try:
      |• "Tell me about black holes"
      |• "Compare Jupiter and Saturn"
      |• "List galaxies"
      |• "Random fact" | "Start quiz"
      |• "Exit"
      |What part of the universe shall we explore?""".stripMargin
  )

  private val unknownResponses = List(
    "I'm not sure what you mean. Try rephrasing or use 'help' to see options.",
    "That's outside my orbit! Ask about planets, stars, or try 'help'.",
    "I didn't catch that. Try a specific planet or 'help' for commands.",
    "Sorry, I don't understand. Ask about astronomy or try 'help'.",
    "Lost in space! Try asking differently or use 'help'."
  )

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

  private def getFollowUpSuggestion(topic: String): String = {
    val suggestions = List(
      s"Would you like to know more about $topic?",
      s"Anything specific about $topic you'd like to explore?",
      s"I have more info about $topic if you're interested!",
      s"Want to compare $topic with another celestial body?",
      "What else would you like to discover about our universe?"
    )
    Random.shuffle(suggestions).head
  }

  private def personalizeResponse(response: String, topic: Option[String] = None): String = {
    topic.foreach(t => recentTopics = (t :: recentTopics).distinct.take(5))
    if (recentTopics.nonEmpty && Random.nextDouble() < 0.3) {
      val prevTopic = Random.shuffle(recentTopics).head
      val personalizedIntros = List(
        s"Since you were interested in $prevTopic earlier, you might like to know that ",
        s"Following up on $prevTopic, ",
        s"This reminds me of $prevTopic, where "
      )
      Random.shuffle(personalizedIntros).head + response.toLowerCase
    } else if (Random.nextDouble() < 0.4) {
      Random.shuffle(enthusiasticPhrases).head + response.toLowerCase
    } else {
      response
    }
  }

  private def maybeAddFollowUp(response: String, topic: Option[String] = None): String = {
    if (Random.nextDouble() < 0.4) {
      val followUp = topic match {
        case Some(t) => getFollowUpSuggestion(t)
        case None =>
          Random
            .shuffle(
              List(
                "What else would you like to know?",
                "Anything else you're curious about?",
                "What other cosmic mysteries should we explore?",
                "Any other astronomy questions?"
              )
            )
            .head
      }
      s"$response\n\n$followUp"
    } else {
      response
    }
  }

  private def formatFacts(facts: Map[String, String], topic: String): String = {
    if (facts.isEmpty)
      return s"I don't have much info about $topic. Try another topic or check back later!"
    val intro = Random
      .shuffle(
        List(
          s"Here's what I know about $topic:",
          s"Let me tell you about $topic:",
          s"$topic is fascinating! Here's what I know:"
        )
      )
      .head
    val formattedFacts = facts
      .map { case (k, v) =>
        val readableKey = k.replaceAll("([A-Z])", " $1").replaceAll("_", " ").trim.toLowerCase.capitalize
        s"• $readableKey: $v"
      }
      .mkString("\n")
    s"$intro\n$formattedFacts"
  }

  private def generateNaturalComparison(
    entity1: String,
    entity2: String,
    facts1: Map[String, String],
    facts2: Map[String, String]
  ): String = {
    val intro = Random
      .shuffle(
        List(
          s"Let's compare $entity1 and $entity2:",
          s"Here's how $entity1 stacks up against $entity2:",
          s"Comparing $entity1 and $entity2:"
        )
      )
      .head
    val commonKeys = facts1.keySet.intersect(facts2.keySet)
    val keysToPrioritize =
      List("diameter", "mass", "distance_from_sun", "orbital_period", "rotation_period", "surface_temperature")
    val prioritizedKeys = keysToPrioritize.filter(commonKeys.contains) ++ (commonKeys -- keysToPrioritize).toList.sorted
    if (prioritizedKeys.isEmpty) {
      return s"I don't have enough info to compare $entity1 and $entity2 meaningfully."
    }
    val comparisons = prioritizedKeys
      .take(3)
      .map { key =>
        val readableKey = key.replaceAll("([A-Z])", " $1").replaceAll("_", " ").trim.toLowerCase.capitalize
        s"• $readableKey: $entity1 is ${facts1(key)}, while $entity2 is ${facts2(key)}"
      }
      .mkString("\n")
    s"$intro\n$comparisons"
  }

  def respond(command: Command): Map[String, String] = {
    analytics.logInteraction(command)
    try {
      val response = command match {
        case Exit =>
          Map(
            "message" -> Random
              .shuffle(
                List(
                  "Goodbye! May the stars light your way!",
                  "Farewell, explorer! Come back soon.",
                  "Safe travels through the cosmos!"
                )
              )
              .head
          )

        case Greet =>
          Map("message" -> personalizeResponse(Random.shuffle(greetings).head))

        case Help =>
          Map("message" -> personalizeResponse(Random.shuffle(helpMessages).head))

        case PlanetInfo(planet) =>
          val normalizedPlanet = planet.trim.toLowerCase.capitalize
          val facts =
            try {
              dataSource.getFacts(normalizedPlanet).getOrElse(Map.empty)
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching facts for $normalizedPlanet: ${e.getMessage}")
                Map.empty
            }
          Map("message" -> maybeAddFollowUp(formatFacts(facts, normalizedPlanet), Some(normalizedPlanet)))

        case StarInfo(star) =>
          val normalizedStar = star.trim.toLowerCase.capitalize
          val facts =
            try {
              dataSource.getFacts(normalizedStar).getOrElse(Map.empty)
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching facts for $normalizedStar: ${e.getMessage}")
                Map.empty
            }
          Map("message" -> maybeAddFollowUp(formatFacts(facts, normalizedStar), Some(normalizedStar)))

        case ConstellationInfo(constellation) =>
          val normalizedConstellation = constellation.trim.toLowerCase.capitalize
          val facts =
            try {
              dataSource.getFacts(normalizedConstellation).getOrElse(Map.empty)
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching facts for $normalizedConstellation: ${e.getMessage}")
                Map.empty
            }
          Map("message" -> maybeAddFollowUp(formatFacts(facts, normalizedConstellation), Some(normalizedConstellation)))

        case Distance(object1, object2) =>
          val normalized1 = object1.trim.toLowerCase.capitalize
          val normalized2 = object2.trim.toLowerCase.capitalize
          recentTopics = (List(normalized1, normalized2) ++ recentTopics).distinct.take(5)
          Map(
            "message" -> Random
              .shuffle(
                List(
                  s"Distance between $normalized1 and $normalized2 varies due to orbits. Would you like info on either?",
                  s"Calculating exact distance between $normalized1 and $normalized2 is complex. Want details on one?"
                )
              )
              .head
          )

        case PropertyQuery(objectName, property) =>
          val normalizedObject = objectName.trim.toLowerCase.capitalize
          val facts =
            try {
              dataSource.getFacts(normalizedObject).getOrElse(Map.empty)
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching facts for $normalizedObject: ${e.getMessage}")
                Map.empty
            }
          val message = facts.get(property.toLowerCase.replace(" ", "_")) match {
            case Some(value) => s"$normalizedObject's $property is $value."
            case None        => s"I don't have info on $normalizedObject's $property. Try another property or topic."
          }
          Map(
            "message" -> maybeAddFollowUp(personalizeResponse(message, Some(normalizedObject)), Some(normalizedObject))
          )

        case AstronomicalEvent(query) =>
          Map(
            "message" -> Random
              .shuffle(
                List(
                  s"I don't have details on '$query' yet. Try asking about planets or stars!",
                  s"Info on '$query' isn't available. Want to explore another topic?"
                )
              )
              .head
          )

        case FunFact(topic) =>
          val normalizedTopic = topic.trim.toLowerCase.capitalize
          val allFacts =
            try {
              dataSource.getFacts(normalizedTopic).toList.flatMap(_.values)
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching facts for $normalizedTopic: ${e.getMessage}")
                List.empty
            }
          val message = if (allFacts.isEmpty) {
            s"No fun facts about $normalizedTopic yet. Try another topic!"
          } else {
            Random
              .shuffle(
                List(
                  s"Fun fact about $normalizedTopic: ",
                  s"Here's something cool about $normalizedTopic: "
                )
              )
              .head + allFacts(Random.nextInt(allFacts.size))
          }
          Map("message" -> maybeAddFollowUp(personalizeResponse(message, Some(normalizedTopic)), Some(normalizedTopic)))

        case NightSkyInfo(query) =>
          Map(
            "message" -> Random
              .shuffle(
                List(
                  s"I can't provide night sky info for '$query' yet. Try asking about planets or constellations!",
                  s"No night sky data for '$query'. Want info on another topic?"
                )
              )
              .head
          )

        case ListPlanets =>
          val planets =
            try {
              dataSource.getPlanets
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching planets: ${e.getMessage}")
                List.empty
            }
          val message = if (planets.isEmpty) {
            "My planetary database is empty. I should know about our solar system's planets!"
          } else {
            Random
              .shuffle(
                List(
                  "Our solar system's planets: ",
                  "The eight planets are: "
                )
              )
              .head + planets.mkString(", ")
          }
          Map("message" -> personalizeResponse(message))

        case ListCategory(category) =>
          val normalizedCategory = category.toLowerCase
          val items = normalizedCategory match {
            case "planets" =>
              try {
                dataSource.getPlanets
              } catch {
                case NonFatal(e) =>
                  println(s"Error fetching planets: ${e.getMessage}")
                  List.empty
              }
            case "stars"          => List("Sun", "Sirius", "Alpha Centauri", "Betelgeuse", "Vega", "Proxima Centauri")
            case "constellations" => List("Orion", "Big Dipper", "Cassiopeia", "Leo", "Scorpius", "Taurus")
            case "galaxies"       => List("Milky Way", "Andromeda", "Triangulum", "Sombrero", "Whirlpool")
            case "black holes"    => List("Sagittarius A*", "Cygnus X-1", "M87 Black Hole", "TON 618")
            case "moons" => List("Luna (Earth)", "Phobos (Mars)", "Io (Jupiter)", "Europa (Jupiter)", "Titan (Saturn)")
            case "dwarf planets" => List("Pluto", "Ceres", "Eris", "Haumea", "Makemake")
            case _               => List(s"No items found for '$normalizedCategory'")
          }
          val message = Random
            .shuffle(
              List(
                s"${normalizedCategory.capitalize}: ",
                s"List of $normalizedCategory: "
              )
            )
            .head + items.mkString(", ")
          Map("message" -> personalizeResponse(message))

        case RandomFact =>
          val allFacts =
            try {
              dataSource.getPlanets.flatMap(dataSource.getFacts).flatMap(_.values)
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching random facts: ${e.getMessage}")
                List.empty
            }
          val message = if (allFacts.isEmpty) {
            "My fact database is empty. Try asking about specific planets!"
          } else {
            Random
              .shuffle(
                List(
                  "Here's a cool space fact: ",
                  "Did you know? "
                )
              )
              .head + allFacts(Random.nextInt(allFacts.size))
          }
          Map("message" -> maybeAddFollowUp(personalizeResponse(message)))

        case AskAbout(topic) =>
          val normalizedTopic = topic.trim.toLowerCase.capitalize
          val facts =
            try {
              dataSource.getFacts(normalizedTopic).getOrElse(Map.empty)
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching facts for $normalizedTopic: ${e.getMessage}")
                Map.empty
            }
          val message = if (facts.nonEmpty) {
            formatFacts(facts, normalizedTopic)
          } else {
            Random
              .shuffle(
                List(
                  s"I don't have info on $normalizedTopic. Try planets, stars, or constellations!",
                  s"$normalizedTopic isn't in my database. Want to explore another topic?"
                )
              )
              .head
          }
          Map("message" -> maybeAddFollowUp(personalizeResponse(message, Some(normalizedTopic)), Some(normalizedTopic)))

        case Compare(topic1, topic2) =>
          val normalized1 = topic1.trim.toLowerCase.capitalize
          val normalized2 = topic2.trim.toLowerCase.capitalize
          recentTopics = (List(normalized1, normalized2) ++ recentTopics).distinct.take(5)
          val facts1 =
            try {
              dataSource.getFacts(normalized1).getOrElse(Map.empty)
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching facts for $normalized1: ${e.getMessage}")
                Map.empty
            }
          val facts2 =
            try {
              dataSource.getFacts(normalized2).getOrElse(Map.empty)
            } catch {
              case NonFatal(e) =>
                println(s"Error fetching facts for $normalized2: ${e.getMessage}")
                Map.empty
            }
          val message = (facts1.nonEmpty, facts2.nonEmpty) match {
            case (true, true)   => generateNaturalComparison(normalized1, normalized2, facts1, facts2)
            case (true, false)  => s"I only have info on $normalized1. Want to learn more about it?"
            case (false, true)  => s"I only have info on $normalized2. Want to learn more about it?"
            case (false, false) => s"I don't have enough info to compare $normalized1 and $normalized2."
          }
          Map("message" -> personalizeResponse(message))

        case StartQuiz =>
          Map("message" -> "Starting a quiz! Get ready for the first question.", "quizActive" -> "true")

        case AnswerQuiz(answer) =>
          Map("message" -> s"Your answer: $answer. Checking with the quiz system...")

        case Unknown(input) =>
          val inputLower = input.toLowerCase
          val planets = Vector("mercury", "venus", "earth", "mars", "jupiter", "saturn", "uranus", "neptune", "pluto")
          val planetMatches = planets.filter(inputLower.contains)
          val message = if (planetMatches.nonEmpty) {
            val planet = planetMatches.head.capitalize
            val facts =
              try {
                dataSource.getFacts(planet).getOrElse(Map.empty)
              } catch {
                case NonFatal(e) =>
                  println(s"Error fetching facts for $planet: ${e.getMessage}")
                  Map.empty
              }
            if (facts.nonEmpty) {
              formatFacts(facts, planet)
            } else {
              s"I don't have info on $planet. Try another topic!"
            }
          } else {
            inputLower match {
              case s if s.contains("thank")              => "You're welcome! What's next?"
              case s if s.contains("hello") || s == "hi" => "Hi! I'm CHATURN, your astronomy bot. What's up?"
              case s if s.contains("how are you")        => "Orbiting smoothly! What's your question?"
              case s if s.contains("your name")          => "I'm CHATURN, your cosmic guide!"
              case _                                     => Random.shuffle(unknownResponses).head
            }
          }
          Map("message" -> maybeAddFollowUp(message))
      }
      response
    } catch {
      case NonFatal(e) =>
        println(s"Error processing command $command: ${e.getMessage}")
        Map("message" -> "Sorry, something went wrong while processing your request. Try again or ask something else!")
    }
  }
}
