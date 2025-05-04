package chatbot.parser

<<<<<<< Updated upstream
class InputParser {
  def parseInput(input: String, isQuizActive: Boolean = false): Either[String, Command] = {
=======
import chatbot.quiz.QuizManager

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

  // Command response structure
  case class CommandResponse(commandType: String, payload: String = "", extraPayload: String = "")

  def parseInput(input: String, isQuizActive: Boolean = false): String = {
>>>>>>> Stashed changes
    val normalizedInput = input.trim.toLowerCase

    // Handle empty input
    if (normalizedInput.isEmpty) {
      return "unknown_empty"
    }

<<<<<<< Updated upstream
    // Split input into words for easier pattern matching
    val words = normalizedInput.split("\\s+").toList

    // Process based on quiz mode
    if (isQuizActive) {
      parseQuizMode(words, normalizedInput)
    } else {
      parseRegularMode(words, normalizedInput)
    }
  }

  private def parseQuizMode(words: List[String], originalInput: String): Either[String, Command] = {
    // Check for quiz exit commands
    if (words.exists(exitWords.contains) && (words.contains("quiz") || words.length == 1)) {
      Right(Unknown("exit"))
    } else if (words.contains("skip")) {
      Right(StartQuiz)
    } else {
      // Default action in quiz mode is to treat as an answer
      Right(AnswerQuiz(originalInput))
    }
  }

  private def parseRegularMode(words: List[String], originalInput: String): Either[String, Command] = {
    // Try to match command patterns in priority order
    if (matchesHelp(words)) {
      Right(Help)
    } else if (matchesListPlanets(words)) {
      Right(ListPlanets)
    } else if (matchesRandomFact(words)) {
      Right(RandomFact)
    } else if (matchesQuiz(words)) {
      Right(StartQuiz)
    } else if (matchesCompare(originalInput)) {
      extractCompareTopics(originalInput) match {
        case Some((topic1, topic2)) => Right(Compare(topic1, topic2))
        case None                   => Right(Unknown(originalInput))
=======
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
      case _               => "unknown_" + normalizedInput
    }
  }

  private def parseQuizMode(words: List[String], originalInput: String): CommandResponse = {
    // Check for quiz exit commands
    if (words.exists(exitWords.contains) && (words.contains("quiz") || words.length == 1)) {
      CommandResponse(CMD_EXIT_QUIZ)
    } else if (words.contains("skip")) {
      CommandResponse(CMD_SKIP_QUESTION)
    } else {
      // Default action in quiz mode is to treat as an answer
      CommandResponse(CMD_ANSWER_QUIZ, originalInput)
    }
  }

  private def parseRegularMode(words: List[String], originalInput: String): CommandResponse = {
    // Try to match command patterns in priority order
    if (matchesHelp(words)) {
      CommandResponse(CMD_HELP)
    } else if (matchesListPlanets(words)) {
      CommandResponse(CMD_LIST_PLANETS)
    } else if (matchesRandomFact(words)) {
      CommandResponse(CMD_RANDOM_FACT)
    } else if (matchesQuiz(words)) {
      CommandResponse(CMD_START_QUIZ)
    } else if (matchesCompare(originalInput)) {
      extractCompareTopics(originalInput) match {
        case Some((topic1, topic2)) => CommandResponse(CMD_COMPARE, topic1, topic2)
        case None                   => CommandResponse(CMD_UNKNOWN, originalInput)
>>>>>>> Stashed changes
      }
    } else if (matchesCategory(words)) {
      val foundCategory = categories
        .find(cat =>
          words.contains(cat) ||
            originalInput.contains(cat)
        )
        .getOrElse("unknown")
<<<<<<< Updated upstream
      Right(ListCategory(foundCategory))
    } else if (matchesPlanet(words)) {
      val foundPlanet = planets.find(planet => words.contains(planet)).getOrElse("unknown")
      Right(AskAbout(foundPlanet.capitalize))
=======
      CommandResponse(CMD_LIST_CATEGORY, foundCategory)
    } else if (matchesPlanet(words)) {
      val foundPlanet = planets.find(planet => words.contains(planet)).getOrElse("unknown")
      CommandResponse(CMD_ASK_ABOUT, foundPlanet.capitalize)
>>>>>>> Stashed changes
    } else {
      // General ask about query
      val topic = extractTopic(originalInput)
      if (topic.nonEmpty) {
<<<<<<< Updated upstream
        Right(AskAbout(topic.capitalize))
      } else {
        Right(Unknown(originalInput))
=======
        CommandResponse(CMD_ASK_ABOUT, topic.capitalize)
      } else {
        CommandResponse(CMD_UNKNOWN, originalInput)
>>>>>>> Stashed changes
      }
    }
  }

  // Word lists for pattern matching
  private val helpWords    = Set("help", "commands", "guide", "instructions")
  private val listWords    = Set("list", "show", "display", "name", "what")
  private val factWords    = Set("fact", "trivia", "interesting", "random", "cool", "fun")
  private val quizWords    = Set("quiz", "trivia", "test", "challenge", "game")
  private val compareWords = Set("compare", "difference", "versus", "vs", "between", "against")
  private val exitWords    = Set("exit", "quit", "stop", "end")
  private val planets      = Set("mars", "jupiter", "saturn", "uranus", "neptune", "venus", "mercury", "earth", "pluto")
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

  // Pattern matching functions
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
    words.exists(quizWords.contains) ||
    (words.contains("test") && words.contains("knowledge"))
  }

  private def matchesCompare(input: String): Boolean = {
    compareWords.exists(input.contains)
  }

  private def matchesCategory(words: List[String]): Boolean = {
    val foundCategory = categories.exists(cat =>
      words.contains(cat) ||
        (cat.contains(" ") && cat.split(" ").forall(words.contains))
    )
    (words.exists(listWords.contains) && foundCategory) || foundCategory
  }

  private def matchesPlanet(words: List[String]): Boolean = {
    planets.exists(words.contains)
  }

  // Helper for extracting topics for comparison
  private def extractCompareTopics(input: String): Option[(String, String)] = {
    val separators = List("and", "vs", "versus", "to", "with", "against", "between", "from")

    // Try to find where the comparison is stated
    val comparisonPhrases = List("compare", "difference", "versus", "vs", "how does", "compared to")
    val comparisonPhrase  = comparisonPhrases.find(input.contains).getOrElse("")

    if (comparisonPhrase.nonEmpty) {
      // Find the text after the comparison phrase
      val afterPhrase = input.split(comparisonPhrase)(1).trim

      // Find which separator is used
      val separator = separators.find(afterPhrase.contains).getOrElse("")

      if (separator.nonEmpty) {
        val parts = afterPhrase.split(separator)
        if (parts.length >= 2) {
          val part1 = parts(0).trim
          val part2 = parts.drop(1).mkString(" ").trim

          // Remove trailing punctuation
          val cleanPart1 = part1.replaceAll("[?!.,]+$", "").trim
          val cleanPart2 = part2.replaceAll("[?!.,]+$", "").trim

          if (cleanPart1.nonEmpty && cleanPart2.nonEmpty) {
            Some((cleanPart1, cleanPart2))
          } else None
        } else None
      } else None
    } else None
  }

  // Helper for extracting a topic from a query
  private def extractTopic(input: String): String = {
    // Remove common prefixes
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

    // Find and remove matching prefix
    prefixes.foreach { prefix =>
      if (cleaned.startsWith(prefix)) {
        cleaned = cleaned.substring(prefix.length).trim
      }
    }

    // Remove question marks and punctuation at the end
    cleaned = cleaned.replaceAll("[?!.,]+$", "").trim

    // Remove filler words
    val fillerWords = List("please", "can you", "could you", "would you")
    fillerWords.foreach { filler =>
      if (cleaned.startsWith(filler)) {
        cleaned = cleaned.substring(filler.length).trim
      }
    }

    cleaned
  }
}
