package chatbot.parser

object InputParser {
  // Define command types as string constants
  val CMD_UNKNOWN       = "UNKNOWN"
  val CMD_HELP          = "HELP"
  val CMD_LIST_PLANETS  = "LIST_PLANETS"
  val CMD_LIST_CATEGORY = "LIST_CATEGORY"
  val CMD_RANDOM_FACT   = "RANDOM_FACT"
  val CMD_START_QUIZ    = "START_QUIZ"
  val CMD_ANSWER_QUIZ   = "ANSWER_QUIZ"
  val CMD_ASK_ABOUT     = "ASK_ABOUT"
  val CMD_COMPARE       = "COMPARE"
  val CMD_EXIT_QUIZ     = "EXIT_QUIZ"
  val CMD_SKIP_QUESTION = "SKIP_QUESTION"
  val CMD_GREETINGS     = "GREETINGS"

  // Word lists for pattern matching
  val helpWords     = Set("help", "commands", "guide", "instructions")
  val listWords     = Set("list", "show", "display", "name", "what")
  val factWords     = Set("fact", "trivia", "interesting", "random", "cool", "fun")
  val quizWords     = Set("quiz", "trivia", "test", "challenge", "game")
  val compareWords  = Set("compare", "difference", "versus", "vs", "between", "against")
  val exitWords     = Set("exit", "quit", "stop", "end")
  val greetingWords = Set("hello", "hi", "hey", "greetings", "yo")
  val planets       = Set("mars", "jupiter", "saturn", "uranus", "neptune", "venus", "mercury", "earth", "pluto")
  val categories = Set(
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

  // Parse user input and return a command string
  def parseInput(input: String, isQuizActive: Boolean = false): String = {
    val normalizedInput = input.trim.toLowerCase

    // Handle empty input
    if (normalizedInput.isEmpty) {
      return "unknown_empty"
    }

    // Split input into words for easier pattern matching
    val words = normalizedInput.split(" ").toList

    // Process based on quiz mode
    val (commandType, payload, extraPayload) = if (isQuizActive) {
      parseQuizMode(words, normalizedInput)
    } else {
      parseRegularMode(words, normalizedInput)
    }

    // Convert the tuple to a string format expected by Responder
    commandType.toLowerCase match {
      case "unknown"       => "unknown_" + payload
      case "help"          => "help"
      case "list_planets"  => "listplanets"
      case "list_category" => "listcategory_" + payload
      case "random_fact"   => "randomfact"
      case "start_quiz"    => "startquiz"
      case "answer_quiz"   => "answerquiz_" + payload
      case "ask_about"     => "askabout_" + payload
      case "compare"       => "compare_" + payload + "_" + extraPayload
      case "exit_quiz"     => "exit"
      case "skip_question" => "startquiz" // Treat "skip" as moving to next question
      case "greetings"     => "greetings"
      case _               => "unknown_" + normalizedInput
    }
  }

  // Parse input in quiz mode
  def parseQuizMode(words: List[String], originalInput: String): (String, String, String) = {
    // Check for quiz exit commands
    if (words.exists(exitWords.contains) && (words.contains("quiz") || words.length == 1)) {
      (CMD_EXIT_QUIZ, "", "")
    } else if (words.contains("skip")) {
      (CMD_SKIP_QUESTION, "", "")
    } else {
      // Default action in quiz mode is to treat as an answer
      (CMD_ANSWER_QUIZ, originalInput, "")
    }
  }

  // Parse input in regular mode
  def parseRegularMode(words: List[String], originalInput: String): (String, String, String) = {
    // Prioritize different command types
    if (matchesGreetings(words)) {
      (CMD_GREETINGS, "", "")
    } else if (matchesHelp(words)) {
      (CMD_HELP, "", "")
    } else if (matchesListPlanets(words)) {
      (CMD_LIST_PLANETS, "", "")
    } else if (matchesRandomFact(words)) {
      (CMD_RANDOM_FACT, "", "")
    } else if (matchesQuiz(words)) {
      (CMD_START_QUIZ, "", "")
    } else if (matchesCompare(words)) {
      extractCompareTopics(originalInput, words) match {
        case (topic1, topic2) if topic1.nonEmpty && topic2.nonEmpty => (CMD_COMPARE, topic1, topic2)
        case _                                                      => (CMD_UNKNOWN, originalInput, "")
      }
    } else if (matchesCategory(words)) {
      val foundCategory = findCategory(words)
      (CMD_LIST_CATEGORY, foundCategory, "")
    } else if (matchesPlanet(words)) {
      val foundPlanet = findPlanet(words)
      (CMD_ASK_ABOUT, foundPlanet.capitalize, "")
    } else {
      // General ask about query
      val topic = extractTopic(originalInput)
      if (topic.nonEmpty) {
        (CMD_ASK_ABOUT, topic.capitalize, "")
      } else {
        (CMD_UNKNOWN, originalInput, "")
      }
    }
  }

  // Pattern matching functions using string operations
  def matchesGreetings(words: List[String]): Boolean = {
    words.exists(greetingWords.contains) || words.contains("chaturn")
  }

  def matchesHelp(words: List[String]): Boolean = {
    words.exists(helpWords.contains) ||
    (words.contains("what") && words.contains("can") && words.contains("you") && words.contains("do"))
  }

  def matchesListPlanets(words: List[String]): Boolean = {
    words.exists(listWords.contains) && words.contains("planets")
  }

  def matchesRandomFact(words: List[String]): Boolean = {
    (words.contains("random") && (words.contains("fact") || words.contains("trivia"))) ||
    (words.exists(factWords.contains) && words.contains("fact")) ||
    (words.contains("surprise") && words.contains("me"))
  }

  def matchesQuiz(words: List[String]): Boolean = {
    words.exists(quizWords.contains) ||
    (words.contains("test") && words.contains("knowledge"))
  }

  def matchesCompare(words: List[String]): Boolean = {
    words.exists(compareWords.contains)
  }

  def matchesCategory(words: List[String]): Boolean = {
    val foundCategory = categories.exists(cat => {
      val catWords = cat.split(" ")
      if (catWords.length == 1) {
        words.contains(cat)
      } else {
        catWords.forall(words.contains)
      }
    })
    (words.exists(listWords.contains) && foundCategory) || foundCategory
  }

  def matchesPlanet(words: List[String]): Boolean = {
    planets.exists(words.contains)
  }

  // Find the category mentioned in the input
  def findCategory(words: List[String]): String = {
    categories
      .find(cat => {
        val catWords = cat.split(" ")
        if (catWords.length == 1) {
          words.contains(cat)
        } else {
          catWords.forall(words.contains)
        }
      })
      .getOrElse("unknown")
  }

  // Find the planet mentioned in the input
  def findPlanet(words: List[String]): String = {
    planets.find(words.contains).getOrElse("unknown")
  }

  // Extract comparison topics from input
  def extractCompareTopics(input: String, words: List[String]): (String, String) = {
    val separators = List("and", "vs", "versus", "to", "with", "against", "between", "from")

    // First find which comparison word is used
    val comparisonWord = compareWords.find(words.contains).getOrElse("")

    if (comparisonWord.isEmpty) {
      return ("", "")
    }

    // Find where in the input the comparison word appears
    val compIndex = words.indexOf(comparisonWord)
    if (compIndex == -1 || compIndex >= words.length - 1) {
      return ("", "")
    }

    // Find which separator is used after the comparison word
    val afterComp = words.drop(compIndex + 1)
    val sepIndex  = afterComp.indexWhere(separators.contains)

    if (sepIndex == -1 || sepIndex >= afterComp.length - 1) {
      return ("", "")
    }

    // Extract the two topics
    val firstPart  = afterComp.take(sepIndex).mkString(" ")
    val secondPart = afterComp.drop(sepIndex + 1).mkString(" ")

    (cleanTopic(firstPart), cleanTopic(secondPart))
  }

  // Clean a topic string by removing trailing punctuation
  def cleanTopic(topic: String): String = {
    val cleanedTopic = topic.trim
    if (cleanedTopic.nonEmpty && "?!.,".contains(cleanedTopic.last)) {
      cleanedTopic.init.trim
    } else {
      cleanedTopic
    }
  }

  // Extract a topic from a general query
  def extractTopic(input: String): String = {
    // List of common prefixes to remove
    val prefixes = List(
      "tell me about ",
      "what is ",
      "what are ",
      "who is ",
      "where is ",
      "tell me ",
      "explain ",
      "describe ",
      "talk about ",
      "can you tell me about ",
      "i want to know about ",
      "information about ",
      "details about "
    )

    // Try to remove a matching prefix
    val withoutPrefix = prefixes.foldLeft(input) { (currentInput, prefix) =>
      if (currentInput.startsWith(prefix)) {
        currentInput.substring(prefix.length)
      } else {
        currentInput
      }
    }

    // Remove trailing punctuation
    val withoutPunctuation = cleanTopic(withoutPrefix)

    // Remove filler words at the beginning
    val fillerWords = List("please ", "can you ", "could you ", "would you ")
    fillerWords.foldLeft(withoutPunctuation) { (currentInput, filler) =>
      if (currentInput.startsWith(filler)) {
        currentInput.substring(filler.length)
      } else {
        currentInput
      }
    }
  }
}
