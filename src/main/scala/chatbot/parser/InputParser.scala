package chatbot.parser

import scala.util.Random
import scala.util.matching.Regex

class InputParser {
  private val CMD_UNKNOWN       = "UNKNOWN"
  private val CMD_HELP          = "HELP"
  private val CMD_LIST_PLANETS  = "LIST_PLANETS"
  private val CMD_LIST_CATEGORY = "LIST_CATEGORY"
  private val CMD_RANDOM_FACT   = "RANDOM_FACT"
  private val CMD_START_QUIZ    = "START_QUIZ"
  private val CMD_ANSWER_QUIZ   = "ANSWER_QUIZ"
  private val CMD_ASK_ABOUT     = "ASK_ABOUT"
  private val CMD_COMPARE       = "COMPARE"
  private val CMD_BIGGER        = "BIGGER"
  private val CMD_FEATURE       = "FEATURE"
  private val CMD_EXIT_QUIZ     = "EXIT_QUIZ"
  private val CMD_SKIP_QUESTION = "SKIP_QUESTION"
  private val CMD_GREETINGS     = "GREETINGS"

  case class CommandResponse(commandType: String, payload: String = "", extraPayload: String = "")

  def parseInput(input: String, isQuizActive: Boolean = false): String = {
    val normalizedInput = input.trim.toLowerCase
    if (normalizedInput.isEmpty) return "unknown_empty"

    val words = normalizedInput.split("\\s+").toList
    val cmdResponse = if (isQuizActive) {
      parseQuizMode(words, normalizedInput)
    } else {
      parseRegularMode(words, normalizedInput)
    }

    cmdResponse.commandType.toLowerCase match {
      case "unknown"       => "unknown_" + cmdResponse.payload
      case "help"          => "help"
      case "list_planets"  => "listplanets"
      case "list_category" => "listcategory_" + cmdResponse.payload
      case "random_fact"   => "randomfact"
      case "start_quiz"    => "startquiz"
      case "answer_quiz"   => "answerquiz_" + cmdResponse.payload
      case "ask_about"     => "askabout_" + cmdResponse.payload
      case "compare"       => "compare_" + cmdResponse.payload + "_" + cmdResponse.extraPayload
      case "bigger"        => "bigger_" + cmdResponse.payload + "_" + cmdResponse.extraPayload
      case "feature"       => "feature_" + cmdResponse.payload + "_" + cmdResponse.extraPayload
      case "exit_quiz"     => "exit"
      case "skip_question" => "startquiz"
      case "greetings"     => "greetings"
      case _               => "unknown_" + normalizedInput
    }
  }

  private def parseQuizMode(words: List[String], originalInput: String): CommandResponse = {
    if (words.exists(exitWords.contains) && (words.contains("quiz") || words.length == 1)) {
      CommandResponse(CMD_EXIT_QUIZ)
    } else if (words.contains("skip")) {
      CommandResponse(CMD_SKIP_QUESTION)
    } else {
      CommandResponse(CMD_ANSWER_QUIZ, originalInput)
    }
  }

  private def parseRegularMode(words: List[String], originalInput: String): CommandResponse = {
    if (matchesGreetings(words)) {
      CommandResponse(CMD_GREETINGS)
    } else if (matchesHelp(words)) {
      CommandResponse(CMD_HELP)
    } else if (matchesListPlanets(words)) {
      CommandResponse(CMD_LIST_PLANETS)
    } else if (matchesRandomFact(words)) {
      CommandResponse(CMD_RANDOM_FACT)
    } else if (matchesQuiz(words)) {
      CommandResponse(CMD_START_QUIZ)
    } else if (matchesFeature(originalInput)) {
      extractFeature(originalInput) match {
        case Some((topic, attr)) => CommandResponse(CMD_FEATURE, topic, attr)
        case None                => CommandResponse(CMD_UNKNOWN, originalInput)
      }
    } else if (matchesCompare(originalInput)) {
      extractCompareTopics(originalInput) match {
        case Some((topic1, topic2)) => CommandResponse(CMD_COMPARE, topic1, topic2)
        case None                   => CommandResponse(CMD_UNKNOWN, originalInput)
      }
    } else if (matchesBigger(originalInput)) {
      extractCompareTopics(originalInput) match {
        case Some((topic1, topic2)) => CommandResponse(CMD_BIGGER, topic1, topic2)
        case None                   => CommandResponse(CMD_UNKNOWN, originalInput)
      }
    } else if (matchesCategory(words)) {
      val foundCategory = categories
        .find(cat =>
          words.contains(cat) ||
            (cat.contains(" ") && cat.split(" ").forall(words.contains))
        )
        .getOrElse("unknown")
      CommandResponse(CMD_LIST_CATEGORY, foundCategory)
    } else if (matchesPlanet(words)) {
      val foundPlanet = planets
        .find(planet => words.contains(planet))
        .getOrElse("unknown")
      CommandResponse(CMD_ASK_ABOUT, foundPlanet.capitalize)
    } else {
      val topic = extractTopic(originalInput)
      if (topic.nonEmpty) CommandResponse(CMD_ASK_ABOUT, topic.capitalize)
      else CommandResponse(CMD_UNKNOWN, originalInput)
    }
  }

  private val helpWords     = Set("help", "commands", "guide", "instructions")
  private val listWords     = Set("list", "show", "display", "name", "what")
  private val factWords     = Set("fact", "trivia", "interesting", "random", "cool", "fun")
  private val quizWords     = Set("quiz", "trivia", "test", "challenge", "game")
  private val compareWords  = Set("compare", "difference", "versus", "vs", "between", "against")
  private val biggerWords   = Set("bigger", "larger", "biggest", "largest")
  private val exitWords     = Set("exit", "quit", "stop", "end")
  private val greetingWords = Set("hello", "hi", "hey", "greetings", "yo")
  private val planets = Set("mars", "jupiter", "saturn", "uranus", "neptune", "venus", "mercury", "earth", "pluto")
  private val categories = Set(
    "stars",
    "constellations",
    "moons",
    "dwarf planets",
    "galaxies",
    "black holes",
    "asteroids",
    "comets",
    "nebulae",
    "star systems",
    "exoplanets"
  )

  private def matchesGreetings(words: List[String]): Boolean = {
    val hasGreeting     = words.exists(greetingWords.contains)
    val mentionsChaturn = words.contains("chaturn")
    hasGreeting || mentionsChaturn
  }

  private def matchesHelp(words: List[String]): Boolean = {
    words.exists(helpWords.contains) ||
    (words.contains("what") && words.contains("can") && words.contains("you") && words.contains("do"))
  }

  private def matchesListPlanets(words: List[String]): Boolean = {
    words.exists(listWords.contains) && words.contains("planets")
  }

  private def matchesRandomFact(words: List[String]): Boolean = {
    (words.contains("random") && words.exists(w => w == "fact" || w == "trivia")) ||
    (words.exists(factWords.contains) && words.contains("fact")) ||
    (words.contains("surprise") && words.contains("me"))
  }

  private def matchesQuiz(words: List[String]): Boolean = {
    words.exists(quizWords.contains) || (words.contains("test") && words.contains("knowledge"))
  }

  private def matchesCompare(input: String): Boolean = {
    compareWords.exists(input.contains) || input.matches("(?i)compare\\s+between\\s+\\w+\\s+and\\s+\\w+.*")
  }

  private def matchesBigger(input: String): Boolean = {
    biggerWords.exists(input.contains) && input.contains(" or ") &&
    input.matches("(?i).*which\\s+planet\\s+is\\s+(bigger|larger|biggest|largest)\\s+\\w+\\s+or\\s+\\w+.*")
  }

  private def matchesFeature(input: String): Boolean = {
    input.matches("(?i)how\\s+(big|hot|cold|far|heavy|fast|long|dense)\\s+is\\s+\\w+.*")
  }

  private def matchesCategory(words: List[String]): Boolean = {
    categories.exists(cat =>
      words.contains(cat) ||
        (cat.contains(" ") && cat.split(" ").forall(words.contains))
    )
  }

  private def matchesPlanet(words: List[String]): Boolean = {
    planets.exists(planet => words.contains(planet))
  }

  private def extractCompareTopics(input: String): Option[(String, String)] = {
    val comparePattern =
      "(?i)(?:compare\\s+between\\s+|compare\\s+|which\\s+planet\\s+is\\s+(?:bigger|larger|biggest|largest)\\s+)([\\w\\s]+?)\\s+(?:and|or|vs|versus|to|with|against|between)\\s+([\\w\\s]+?)\\s*[!?.]*$".r
    comparePattern.findFirstMatchIn(input).map { m =>
      val topic1 = m.group(1).trim.toLowerCase
      val topic2 = m.group(2).trim.toLowerCase
      (topic1, topic2)
    }
  }

  private def extractFeature(input: String): Option[(String, String)] = {
    val featurePattern = "(?i)how\\s+(big|hot|cold|far|heavy|fast|long|dense)\\s+is\\s+([\\w\\s]+?)\\s*[!?.]*$".r
    featurePattern.findFirstMatchIn(input).map { m =>
      val attr = m.group(1).toLowerCase match {
        case "big"          => "diameter"
        case "hot" | "cold" => "surface_temperature"
        case "far"          => "distance_from_sun"
        case "heavy"        => "mass"
        case "fast"         => "orbital_period"
        case "long"         => "rotation_period"
        case "dense"        => "composition"
      }
      val topic = m.group(2).trim.toLowerCase
      (topic, attr)
    }
  }

  private def extractTopic(input: String): String = {
    val prefixes = List(
      "tell me about",
      "what is",
      "what are",
      "who is",
      "where is",
      "tell me",
      "explain",
      "describe",
      "talk about",
      "can you tell me about",
      "i want to know about",
      "information about",
      "details about"
    )
    var cleaned = input
    prefixes.foreach { prefix =>
      if (cleaned.startsWith(prefix)) cleaned = cleaned.substring(prefix.length).trim
    }
    cleaned = cleaned.replaceAll("[?!.,]+$", "").trim
    val fillerWords = List("please", "can you", "could you", "would you")
    fillerWords.foreach { filler =>
      if (cleaned.startsWith(filler)) cleaned = cleaned.substring(filler.length).trim
    }
    cleaned
  }
}
