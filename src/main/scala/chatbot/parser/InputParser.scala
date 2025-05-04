package chatbot.parser

import chatbot.quiz.QuizManager
import scala.util.matching.Regex
import scala.util.Random

class InputParser {
  // Define command types as string constants
  private val CMD_UNKNOWN       = "UNKNOWN"
  private val CMD_HELP          = "HELP"
  private val CMD_LIST_PLANETS  = "LIST_PLANETS"
  private val CMD_LIST_CATEGORY = "LIST_CATEGORY"
  private val CMD_RANDOM_FACT   = "RANDOM_FACT"
  private val CMD_START_QUIZ    = "START_QUIZ"
  private val CMD_ANSWER_QUIZ   = "ANSWER_QUIZ"
  private val CMD_ASK_ABOUT     = "ASK_ABOUT"
  private val CMD_COMPARE       = "COMPARE"
  private val CMD_EXIT_QUIZ     = "EXIT_QUIZ"
  private val CMD_SKIP_QUESTION = "SKIP_QUESTION"
  private val CMD_GREETINGS     = "GREETINGS"

  // Command response structure
  case class CommandResponse(commandType: String, payload: String = "", extraPayload: String = "")

  // Enhanced word lists for pattern matching
  private val helpWords = Set(
    "help",
    "commands",
    "guide",
    "instructions",
    "manual",
    "tutorial",
    "assist",
    "assistance",
    "menu",
    "options",
    "capabilities",
    "functions",
    "features",
    "usage",
    "how to use"
  )

  private val listWords = Set(
    "list",
    "show",
    "display",
    "name",
    "what",
    "tell",
    "give",
    "enumerate",
    "identify",
    "catalog",
    "inventory",
    "mention",
    "reveal",
    "present",
    "all"
  )

  private val factWords = Set(
    "fact",
    "trivia",
    "interesting",
    "random",
    "cool",
    "fun",
    "fascinating",
    "amazing",
    "surprising",
    "incredible",
    "unusual",
    "curious",
    "unexpected",
    "wow",
    "mind-blowing"
  )

  private val quizWords = Set(
    "quiz",
    "trivia",
    "test",
    "challenge",
    "game",
    "question",
    "exam",
    "competition",
    "questionnaire",
    "assessment",
    "puzzle",
    "play",
    "start"
  )

  private val compareWords = Set(
    "compare",
    "difference",
    "versus",
    "vs",
    "between",
    "against",
    "contrast",
    "differentiate",
    "distinguish",
    "similarities",
    "alike",
    "different",
    "comparison",
    "relation"
  )

  private val exitWords = Set(
    "exit",
    "quit",
    "stop",
    "end",
    "leave",
    "terminate",
    "close",
    "finish",
    "abandon",
    "cancel",
    "done",
    "bye",
    "goodbye",
    "break",
    "enough"
  )

  private val greetingWords = Set(
    "hello",
    "hi",
    "hey",
    "greetings",
    "yo",
    "howdy",
    "hiya",
    "good morning",
    "good afternoon",
    "good evening",
    "what's up",
    "sup",
    "hola",
    "salutations",
    "welcome"
  )

  // Enhanced planet recognition
  private val planets = Map(
    "mercury" -> "Mercury",
    "venus"   -> "Venus",
    "earth"   -> "Earth",
    "mars"    -> "Mars",
    "jupiter" -> "Jupiter",
    "saturn"  -> "Saturn",
    "uranus"  -> "Uranus",
    "neptune" -> "Neptune",
    "pluto"   -> "Pluto" // Including Pluto for user convenience
  )

  // Planet alternative names and common misspellings
  private val planetAliases = Map(
    "mecury"        -> "Mercury",
    "venius"        -> "Venus",
    "earht"         -> "Earth",
    "terra"         -> "Earth",
    "red planet"    -> "Mars",
    "gas giant"     -> "Jupiter",
    "ringed planet" -> "Saturn",
    "urans"         -> "Uranus",
    "netpune"       -> "Neptune",
    "plotu"         -> "Pluto",
    "dwarf planet"  -> "Pluto"
  )

  // Enhanced categories with common variations
  private val categories = Map(
    "stars"              -> "stars",
    "star"               -> "stars",
    "constellations"     -> "constellations",
    "constellation"      -> "constellations",
    "moons"              -> "moons",
    "moon"               -> "moons",
    "natural satellites" -> "moons",
    "dwarf planets"      -> "dwarf planets",
    "dwarf planet"       -> "dwarf planets",
    "galaxies"           -> "galaxies",
    "galaxy"             -> "galaxies",
    "black holes"        -> "black holes",
    "black hole"         -> "black holes",
    "asteroids"          -> "asteroids",
    "asteroid"           -> "asteroids",
    "comets"             -> "comets",
    "comet"              -> "comets",
    "nebulae"            -> "nebulae",
    "nebula"             -> "nebulae",
    "star systems"       -> "star systems",
    "star system"        -> "star systems",
    "solar systems"      -> "star systems",
    "solar system"       -> "star systems",
    "exoplanets"         -> "exoplanets",
    "exoplanet"          -> "exoplanets",
    "extrasolar planets" -> "exoplanets",
    "pulsars"            -> "pulsars",
    "pulsar"             -> "pulsars",
    "supernovas"         -> "supernovas",
    "supernova"          -> "supernovas",
    "meteor showers"     -> "meteor showers",
    "meteor shower"      -> "meteor showers",
    "quasars"            -> "quasars",
    "quasar"             -> "quasars"
  )

  def parseInput(input: String, isQuizActive: Boolean = false): String = {
    val normalizedInput = input.trim.toLowerCase

    // Handle empty input
    if (normalizedInput.isEmpty) {
      return "unknown_empty"
    }

    // Split input into words for easier pattern matching
    val words = normalizedInput.split("\\s+").toList

    // Process based on quiz mode
    val cmdResponse = if (isQuizActive) {
      parseQuizMode(words, normalizedInput)
    } else {
      parseRegularMode(words, normalizedInput)
    }

    // Convert CommandResponse to a string format expected by Responder
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
      case "exit_quiz"     => "exit"
      case "skip_question" => "startquiz" // Treat "skip" as moving to next question
      case "greetings"     => "greetings"
      case _               => "unknown_" + normalizedInput
    }
  }

  private def parseQuizMode(words: List[String], originalInput: String): CommandResponse = {
    // Check for quiz exit commands with improved exit detection
    if (
      (words.exists(exitWords.contains) && (words.contains("quiz") || words.length <= 2)) ||
      originalInput.matches("(?i).*(quit|exit|stop|leave).*")
    ) {
      CommandResponse(CMD_EXIT_QUIZ)
    } else if (words.exists(w => w == "skip" || w == "next" || w == "pass" || w == "don't know" || w == "idk")) {
      CommandResponse(CMD_SKIP_QUESTION)
    } else {
      // Default action in quiz mode is to treat as an answer
      CommandResponse(CMD_ANSWER_QUIZ, originalInput)
    }
  }

  private def parseRegularMode(words: List[String], originalInput: String): CommandResponse = {
    // Check for questions about the chatbot itself first
    if (matchesChatbotSelfQuestion(words, originalInput)) {
      CommandResponse(CMD_GREETINGS) // Use GREETINGS type for chatbot self-questions
    } else if (matchesGreetings(words, originalInput)) {
      CommandResponse(CMD_GREETINGS)
    } else if (matchesHelp(words, originalInput)) {
      CommandResponse(CMD_HELP)
    } else if (matchesListPlanets(words, originalInput)) {
      CommandResponse(CMD_LIST_PLANETS)
    } else if (matchesRandomFact(words, originalInput)) {
      CommandResponse(CMD_RANDOM_FACT)
    } else if (matchesQuiz(words, originalInput)) {
      CommandResponse(CMD_START_QUIZ)
    } else if (matchesCompare(originalInput)) {
      extractCompareTopics(originalInput) match {
        case Some((topic1, topic2)) => CommandResponse(CMD_COMPARE, topic1, topic2)
        case None                   => CommandResponse(CMD_UNKNOWN, originalInput)
      }
    } else {
      // Try to match categories or planets
      val categoryMatch = findCategoryMatch(words, originalInput)
      val planetMatch   = findPlanetMatch(words, originalInput)

      if (categoryMatch.nonEmpty && (matchesListIntent(words, originalInput) || !planetMatch.nonEmpty)) {
        CommandResponse(CMD_LIST_CATEGORY, categoryMatch)
      } else if (planetMatch.nonEmpty) {
        CommandResponse(CMD_ASK_ABOUT, planetMatch)
      } else {
        // General ask about query
        val topic = extractTopic(originalInput)
        if (topic.nonEmpty) {
          CommandResponse(CMD_ASK_ABOUT, topic.capitalize)
        } else {
          CommandResponse(CMD_UNKNOWN, originalInput)
        }
      }
    }
  }

  // Enhanced pattern matching functions that use both word lists and regex patterns
  private def matchesGreetings(words: List[String], input: String): Boolean = {
    val hasGreeting = words.exists(greetingWords.contains) ||
      greetingWords.exists(g => g.contains(" ") && input.contains(g))
    val mentionsChaturn = input.toLowerCase.contains("chaturn") ||
      input.toLowerCase.contains("chat bot") ||
      input.toLowerCase.contains("bot")
    hasGreeting || mentionsChaturn
  }

  // Match questions about the chatbot itself
  private def matchesChatbotSelfQuestion(words: List[String], input: String): Boolean = {
    // Match patterns like "tell me about yourself" when directed at the chatbot
    val botNames      = List("chaturn", "chatbot", "bot")
    val selfWords     = List("yourself", "you", "your")
    val questionWords = List("tell", "about", "who", "what", "describe", "introduce")

    // Check for explicit patterns
    (input.matches("(?i).*(?:tell|talk|explain)\\s+(?:me|us)?\\s+about\\s+(?:yourself|you).*") &&
    botNames.exists(input.toLowerCase.contains) ||

    // Check for "who are you" pattern directed at the chatbot
    (input.matches("(?i).*who\\s+are\\s+you.*") &&
      botNames.exists(input.toLowerCase.contains)) ||

    // Check for "what are you" or "what is chaturn"
    (input.matches("(?i).*what\\s+(?:are|is)\\s+(?:you|chaturn|this bot).*")) ||

    // Check for "introduce yourself" patterns
    (input.matches("(?i).*introduce\\s+yourself.*")) ||

    // General pattern: bot name + self reference + question word
    (botNames.exists(input.toLowerCase.contains) &&
      selfWords.exists(input.toLowerCase.contains) &&
      questionWords.exists(words.contains)))
  }

  private def matchesHelp(words: List[String], input: String): Boolean = {
    words.exists(helpWords.contains) ||
    input.matches("(?i).*(what|how)\\s+(can|do)\\s+(you|chaturn)\\s+(do|help|tell).*") ||
    input.matches("(?i).*tell\\s+me\\s+what\\s+(you|chaturn)\\s+can\\s+do.*") ||
    input.matches("(?i).*how\\s+(does|do)\\s+(this|chaturn)\\s+work.*")
  }

  private def matchesListIntent(words: List[String], input: String): Boolean = {
    words.exists(listWords.contains) ||
    input.matches("(?i).*(tell|give|show)\\s+me\\s+(all|the).*") ||
    input.matches("(?i).*(what|which)\\s+(are|is)\\s+(all|the).*")
  }

  private def matchesListPlanets(words: List[String], input: String): Boolean = {
    (matchesListIntent(words, input) &&
      (words.contains("planets") || words.contains("planet") || input.contains("solar system"))) ||
    input.matches("(?i).*name\\s+(all|the)\\s+planets.*") ||
    input.matches("(?i).*planets\\s+in\\s+(our|the)\\s+solar\\s+system.*")
  }

  private def matchesRandomFact(words: List[String], input: String): Boolean = {
    (words.exists(w => w == "random" || w == "interesting") &&
      words.exists(w => w == "fact" || w == "facts" || w == "trivia")) ||
    (words.exists(factWords.contains) &&
      (words.contains("fact") || words.contains("facts") || words.contains("information"))) ||
    (words.contains("surprise") && words.contains("me")) ||
    input.matches("(?i).*tell\\s+me\\s+(something|anything)\\s+(interesting|cool|fun).*") ||
    input.matches("(?i).*did\\s+you\\s+know.*")
  }

  private def matchesQuiz(words: List[String], input: String): Boolean = {
    words.exists(quizWords.contains) ||
    (words.contains("test") && (words.contains("knowledge") || words.contains("me"))) ||
    input.matches("(?i).*(start|begin|play|take)\\s+a\\s+(quiz|test).*") ||
    input.matches("(?i).*(ask|give)\\s+me\\s+(some|a)\\s+question.*")
  }

  private def matchesCompare(input: String): Boolean = {
    compareWords.exists(input.contains) ||
    input.matches("(?i).*(what('s| is) the|tell me( the)?) difference between.*") ||
    input.matches("(?i).*(how|what) (does|is).*compare(d)? (to|with).*")
  }

  // Find the best category match
  private def findCategoryMatch(words: List[String], input: String): String = {
    // First try direct matches
    val directMatch = categories.keys.find(cat =>
      words.contains(cat) ||
        (cat.contains(" ") && input.toLowerCase.contains(cat))
    )

    if (directMatch.isDefined) {
      return categories(directMatch.get)
    }

    // Try fuzzy matches for multi-word categories
    val multiWordMatches = categories.keys
      .filter(_.contains(" "))
      .filter(cat => {
        val catParts = cat.split(" ")
        // Check if all parts of the category are in the input (in any order)
        catParts.forall(part => words.contains(part))
      })

    if (multiWordMatches.nonEmpty) {
      return categories(multiWordMatches.head)
    }

    // Check for pluralization differences
    val singularMatch = words.find(w => categories.keys.exists(cat => cat == w + "s" || cat == w + "es"))

    if (singularMatch.isDefined) {
      val pluralForm =
        if (singularMatch.get.endsWith("s"))
          singularMatch.get + "es"
        else singularMatch.get + "s"

      return categories.getOrElse(
        pluralForm,
        categories.find(_._1.startsWith(singularMatch.get)).map(_._2).getOrElse("")
      )
    }

    ""
  }

  // Find the best planet match including aliases and fuzzy matching
  private def findPlanetMatch(words: List[String], input: String): String = {
    // Direct planet name match
    val directMatch = planets.keys.find(words.contains)
    if (directMatch.isDefined) {
      planets(directMatch.get)
    } else {
      // Check planet aliases
      val aliasMatch = planetAliases.keys.find(alias =>
        words.contains(alias) ||
          (alias.contains(" ") && input.toLowerCase.contains(alias))
      )
      if (aliasMatch.isDefined) {
        planetAliases(aliasMatch.get)
      } else {
        // Check for fuzzy matches (e.g., "tell me about mars" or "mars facts")
        // Using find instead of foreach with return
        val fuzzyMatch = planets.keys.find(planet => input.toLowerCase.contains(planet))
        fuzzyMatch.map(planets(_)).getOrElse("")
      }
    }
  }

  // Fixed comparison topic extraction
  private def extractCompareTopics(input: String): Option[(String, String)] = {
    // Simpler, more robust approach to comparison extraction

    // Pattern for explicit "compare X and Y" or "difference between X and Y"
    val comparePattern1 =
      """(?i)(?:compare|comparison(?:\s+of)?|differences?(?:\s+between)?)\s+([a-z0-9 ]+)\s+(?:and|with|to|vs\.?|versus)\s+([a-z0-9 ]+)""".r
    val m1 = comparePattern1.findFirstMatchIn(input)
    if (m1.isDefined) {
      val topic1 = m1.get.group(1).trim.replaceAll("[?!.,]+$", "")
      val topic2 = m1.get.group(2).trim.replaceAll("[?!.,]+$", "")
      if (topic1.nonEmpty && topic2.nonEmpty) {
        return Some((topic1, topic2))
      }
    }

    // Pattern for "What's the difference between X and Y"
    val comparePattern2 =
      """(?i)(?:what(?:'s|s| is) the |tell me(?: the)? )?differences?(?: between)?\s+([a-z0-9 ]+)\s+(?:and|with|to|vs\.?|versus)\s+([a-z0-9 ]+)""".r
    val m2 = comparePattern2.findFirstMatchIn(input)
    if (m2.isDefined) {
      val topic1 = m2.get.group(1).trim.replaceAll("[?!.,]+$", "")
      val topic2 = m2.get.group(2).trim.replaceAll("[?!.,]+$", "")
      if (topic1.nonEmpty && topic2.nonEmpty) {
        return Some((topic1, topic2))
      }
    }

    // Pattern for "How does X compare to Y"
    val comparePattern3 =
      """(?i)how\s+(?:does|do|is|are)\s+([a-z0-9 ]+)\s+(?:compare(?:d)?|similar|different|relate(?:d)?)\s+(?:to|with|and)\s+([a-z0-9 ]+)""".r
    val m3 = comparePattern3.findFirstMatchIn(input)
    if (m3.isDefined) {
      val topic1 = m3.get.group(1).trim.replaceAll("[?!.,]+$", "")
      val topic2 = m3.get.group(2).trim.replaceAll("[?!.,]+$", "")
      if (topic1.nonEmpty && topic2.nonEmpty) {
        return Some((topic1, topic2))
      }
    }

    // Pattern for "X vs Y"
    val comparePattern4 = """(?i)([a-z0-9 ]+?)(?:\s+)(?:vs\.?|versus)(?:\s+)([a-z0-9 ]+)""".r
    val m4              = comparePattern4.findFirstMatchIn(input)
    if (m4.isDefined) {
      val topic1 = m4.get.group(1).trim.replaceAll("[?!.,]+$", "")
      val topic2 = m4.get.group(2).trim.replaceAll("[?!.,]+$", "")
      if (topic1.nonEmpty && topic2.nonEmpty) {
        return Some((topic1, topic2))
      }
    }

    // Fallback to original logic with improvements
    val separators        = List("and", "vs", "versus", "to", "with", "against", "between", "from", "or")
    val comparisonPhrases = List("compare", "difference", "versus", "vs", "compared to", "different from")

    // Find the comparison phrase
    val phraseMatch = comparisonPhrases.find(phrase => input.toLowerCase.contains(phrase))

    phraseMatch match {
      case Some(phrase) =>
        val parts = input.toLowerCase.split(phrase)
        if (parts.length < 2) return None

        val beforePhrase = parts(0).trim
        val afterPhrase  = parts(1).trim

        // Find separator in the after phrase
        val separatorMatch = separators.find(sep => afterPhrase.contains(sep))

        separatorMatch match {
          case Some(separator) =>
            val afterParts = afterPhrase.split(separator)
            if (afterParts.length < 2) return None

            val topic2 = afterParts(1).trim.replaceAll("[?!.,]+$", "")

            // Determine topic1
            val topic1 = if (beforePhrase.isEmpty || beforePhrase.split("\\s+").length <= 1) {
              afterParts(0).trim.replaceAll("[?!.,]+$", "")
            } else {
              // Extract topic from before phrase if it seems substantial
              val beforeWords = beforePhrase.split("\\s+").filter(_.length > 2)
              if (beforeWords.nonEmpty) {
                // Use the last few substantial words as the first topic
                val potentialTopic = beforeWords.takeRight(math.min(3, beforeWords.length)).mkString(" ")
                potentialTopic
              } else {
                afterParts(0).trim.replaceAll("[?!.,]+$", "")
              }
            }

            if (topic1.nonEmpty && topic2.nonEmpty) {
              Some((topic1, topic2))
            } else None

          case None =>
            // No separator found, try to extract from context
            if (afterPhrase.isEmpty) return None

            val topic2 = afterPhrase.replaceAll("[?!.,]+$", "").trim

            if (beforePhrase.isEmpty) return None

            // Extract last few words from before phrase as topic1
            val beforeWords = beforePhrase.split("\\s+").filter(_.length > 2)
            if (beforeWords.isEmpty) return None

            val topic1 = beforeWords.takeRight(math.min(2, beforeWords.length)).mkString(" ")

            if (topic1.nonEmpty && topic2.nonEmpty) {
              Some((topic1, topic2))
            } else None
        }

      case None => None
    }
  }

  // Enhanced topic extraction with better prefix and filler word handling
  private def extractTopic(input: String): String = {
    // More comprehensive prefixes with regex patterns
    val prefixPatterns = List(
      "(?i)^tell me about ",
      "(?i)^what is ",
      "(?i)^what are ",
      "(?i)^who is ",
      "(?i)^where is ",
      "(?i)^when is ",
      "(?i)^why is ",
      "(?i)^how is ",
      "(?i)^tell me ",
      "(?i)^explain ",
      "(?i)^describe ",
      "(?i)^talk about ",
      "(?i)^can you tell me about ",
      "(?i)^i want to know about ",
      "(?i)^i'd like to know about ",
      "(?i)^information about ",
      "(?i)^details about ",
      "(?i)^facts about ",
      "(?i)^info on ",
      "(?i)^do you know about "
    )

    var cleaned = input

    // Find and remove matching prefix using regex
    prefixPatterns.foreach { pattern =>
      val regex = pattern.r
      cleaned = regex.replaceFirstIn(cleaned, "")
    }

    // Remove question marks and punctuation at the end
    cleaned = cleaned.replaceAll("[?!.,;:]+$", "").trim

    // Remove filler words and politeness markers
    val fillerPatterns = List(
      "(?i)^please ",
      "(?i)^can you ",
      "(?i)^could you ",
      "(?i)^would you ",
      "(?i)^i was wondering ",
      "(?i)^i'm curious ",
      "(?i)^i am curious "
    )

    fillerPatterns.foreach { pattern =>
      val regex = pattern.r
      cleaned = regex.replaceFirstIn(cleaned, "")
    }

    // Handle "what is/are X" pattern at the end
    cleaned = cleaned.replaceAll("(?i)\\s+what (is|are) (it|they|them)\\s*$", "").trim

    // If the extracted topic seems too long or contains suspicious phrases, truncate intelligently
    if (cleaned.split("\\s+").length > 6) {
      // Look for natural break points
      val breakPoints     = List(" and ", " or ", " that ", " which ", " because ", " since ")
      val firstBreakPoint = breakPoints.map(bp => cleaned.indexOf(bp)).filter(_ > 0).sorted.headOption

      if (firstBreakPoint.isDefined) {
        cleaned = cleaned.substring(0, firstBreakPoint.get).trim
      }
    }

    cleaned
  }
}
