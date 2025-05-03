package chatbot.parser

import scala.util.parsing.combinator.RegexParsers
import chatbot.parser.AST.Command
import chatbot.parser.AST.Command._

class InputParser extends RegexParsers {
  override val whiteSpace = """\s+""".r

  def parseInput(input: String, isQuizActive: Boolean = false): Either[String, Command] = {
    val normalizedInput = input.trim.toLowerCase

    // Quick handling for empty input
    if (normalizedInput.isEmpty) {
      return Right(Unknown(""))
    }

    // Special handling for quiz mode
    if (isQuizActive) {
      // Check for quiz exit commands first
      normalizedInput match {
        case s if s.matches("(?i).*\\b(exit|quit|stop|end)\\b.*quiz.*") =>
          return Right(Unknown("exit"))
        case s if s.matches("(?i).*\\b(exit|quit|stop|end)\\b.*") =>
          return Right(Unknown("exit"))
        case s if s.matches("(?i).*\\b(skip)\\b.*") =>
          return Right(StartQuiz)
        case _ => // Continue to regular parsing
      }
    }

    // Try regular parsing with combinator parsers
    parseAll(command(isQuizActive), normalizedInput) match {
      case Success(result, _) => Right(result)
      case Failure(_, _)      => Left("Parsing failed due to a failure.")
      case Error(_, _)        => Left("Parsing failed due to an error.")
      case NoSuccess(_, _)    =>
        // Fallback heuristic parsing for common patterns
        normalizedInput match {
          case s if s.contains("random") && (s.contains("fact") || s.contains("trivia")) =>
            Right(RandomFact)
          case s if s.contains("list") && s.contains("planet") =>
            Right(ListPlanets)
          case s
              if (s.contains("start") || s.contains("begin") || s.contains("play")) &&
                (s.contains("quiz") || s.contains("trivia") || s.contains("game")) =>
            Right(StartQuiz)
          case s if s.contains("help") =>
            Right(Help)
          case s
              if !isQuizActive && s.matches(
                ".*\\b(mars|jupiter|saturn|uranus|neptune|venus|mercury|earth|pluto)\\b.*"
              ) =>
            // Extract planet name
            val planets = List("mars", "jupiter", "saturn", "uranus", "neptune", "venus", "mercury", "earth", "pluto")
            val foundPlanet = planets.find(p => s.contains(p)).getOrElse("")
            Right(AskAbout(foundPlanet.capitalize))
          case _ =>
            if (isQuizActive) {
              Right(AnswerQuiz(normalizedInput))
            } else {
              Right(Unknown(normalizedInput))
            }
        }
    }
  }

  private def command(isQuizActive: Boolean): Parser[Command] = {
    if (isQuizActive) {
      answerQuizCommand | skipQuizCommand | exitQuizCommand | unknownCommand
    } else {
      helpCommand | listCommand | randomFactCommand | quizCommand | compareCommand | askAboutCommand | unknownCommand
    }
  }

  private def greeting: Parser[String] =
    opt(
      "hi" | "hello" | "hey" | "yo" | "hiya" | "greetings" | "okay" | "so" | "hey chaturn" |
        "i want you to" | "hey can you" | "please can you" | "wassup" | "wassup chaturn" | "i want you to" | "can you" | "okay so" | "tell me" | "tell me more about" | "do you know about" | "what do you know about"
    ) ^^ { case _ => "" }

  private def filler: Parser[String] =
    rep(
      "um" | "uh" | "like" | "so" | "well" | "uhm" | "hmm" | "err" | "you know" | "so like" |
        "i want you to" | "can you" | "okay so" | "tell me" | "tell me more about" | "do you know about" | "what do you know about" | "can you tell me about" | "can you"
    ) ^^ { case _ => "" }

  private def honorific: Parser[String] =
    opt(
      "bot" | "chatbot" | "assistant" | "buddy" | "friend" | "pal" | "dude" | "chaturn" | "man" | "boss" | "robot" | "mister robot"
    ) ^^ { case _ =>
      ""
    }

  private def polite: Parser[String] =
    opt(
      "please" | "pls" | "plz" | "if you can" | "if you could" | "would you" | "if possible" | "pretty please" | "kindly" | "if you don't mind" | "i want you to" | "can you" | "okay so" | "tell me" | "tell me more about" | "do you know about" | "what do you know about"
    ) ^^ { case _ => "" }

  private def thanks: Parser[String] =
    opt("thanks" | "thank you" | "thx" | "ty" | "appreciate it" | "good boy" | "nice" | "good job") ^^ { case _ => "" }

  private def urgency: Parser[String] =
    opt("quickly" | "asap" | "right now" | "immediately" | "when you get a chance") ^^ { case _ => "" }

  private def questionPrefix: Parser[String] =
    opt("can" | "could" | "would" | "will" | "do" | "does" | "should" | "may" | "might") ~ opt("you" | "ya" | "u") ^^ {
      case _ => ""
    }

  private def questionSuffix: Parser[String] =
    opt("?" | "??" | "???" | "!?" | "!") ^^ { case _ => "" }

  private def showVerb: Parser[String] =
    "show" | "display" | "list" | "reveal" | "present" | "exhibit" | "demonstrate" | "tell me" | "tell me more about" | "i want you to" | "can you" | "okay so" | "tell me" | "tell me more about" | "do you know about" | "what do you know about"

  private def tellVerb: Parser[String] =
    "tell" | "inform" | "let me know" | "share" | "explain" | "describe" | "talk about" | "give info on" | "enlighten me about" | "tell me" | "tell me more about" | "i want you to" | "can you" | "okay so" | "tell me" | "tell me more about" | "do you know about" | "what do you know about"

  private def giveVerb: Parser[String] =
    "give" | "provide" | "offer" | "share" | "let me have" | "hit me with" | "throw me" | "send" | "tell" | "tell me" | "i want you to" | "can you" | "okay so" | "tell me" | "tell me more about" | "do you know about" | "what do you know about"

  private def generalVerb: Parser[String] =
    showVerb | tellVerb | giveVerb | "find" | "search for" | "look up" | "check" | "get" | "i want you to" | "can you" | "okay so" | "tell me" | "tell me more about" | "do you know about" | "what do you know about"

  private def meObj: Parser[String] =
    opt("me" | "us" | "myself" | "ourselves" | "me more about" | "about" | "on" | "on the topic of") ^^ { case _ => "" }

  private def preposition: Parser[String] =
    "about" | "on" | "regarding" | "concerning" | "for" | "related to" | "pertaining to" | "dealing with" | "in regards to" | "with respect to" | "of"

  private def contraction: Parser[String] =
    "what's" | "who's" | "where's" | "how's" | "when's" | "why's" | "that's" | "there's" | "it's" | "i'm" | "you're" | "we're" | "they're" | "isn't" | "aren't" | "wasn't" | "weren't" | "don't" | "can't" | "won't" | "shouldn't" | "couldn't" | "wouldn't"

  private def wantExpression: Parser[String] =
    "i want" | "i'd like" | "i need" | "i wanna" | "i wish" | "gimme" | "lemme get" | "hook me up with" | "i'm interested in" | "i'm looking for"

  private def topic: Parser[String] =
    """[a-zA-Z0-9\s\-\.\,\'\!\@\#\$\%\^\&\*\(\)\_\+\=\[\]\{\}\;\:\"\,\.\/\<\>\?]+""".r

  private def category: Parser[String] =
    "planets" | "stars" | "constellations" | "moons" | "dwarf planets" | "galaxies" | "black holes" | "asteroids" | "comets" | "nebulae" | "star systems" | "exoplanets"

  private def helpCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      (questionPrefix ~ opt("can you") ~ opt("give me" | "provide" | "show me") ~ "help" ~ polite ~ questionSuffix) |
        (questionPrefix ~ "what" ~ opt("kind of" | "types of") ~ "commands" ~ opt("can you" | "do you") ~ opt(
          "understand" | "accept" | "know" | "have"
        ) ~ questionSuffix) |
        (questionPrefix ~ "how" ~ opt("do i" | "can i" | "should i") ~ opt("use" | "interact with" | "talk to") ~ opt(
          "you" | "this" | "the bot" | "this bot"
        ) ~ questionSuffix) |
        (questionPrefix ~ "what" ~ opt("can you" | "do you") ~ opt("do" | "know" | "help with") ~ questionSuffix) |
        ("help" ~ opt("me" | "us" | "please" | "command" | "commands" | "info" | "information" | "usage" | "guide")) |
        (tellVerb ~ meObj ~ opt("about") ~ opt("the") ~ opt(
          "available"
        ) ~ ("commands" | "functions" | "features" | "capabilities" | "what you can do") ~ polite) |
        (wantExpression ~ opt("some") ~ "help" ~ polite) |
        (showVerb ~ meObj ~ opt("the") ~ opt(
          "available"
        ) ~ ("commands" | "help" | "options" | "functions" | "features") ~ polite) |
        ("i'm" ~ "confused" ~ opt("about") ~ opt("how") ~ opt("to") ~ opt("use") ~ opt("you" | "this") ~ polite) |
        (contraction ~ opt("your") ~ ("commands" | "functions" | "features" | "capabilities") ~ questionSuffix)
    ) ~ thanks ~ honorific) ^^ { case _ => Help }
  }

  private def listCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      ((showVerb | "list") ~ meObj ~ opt("all" | "the") ~ "planets" ~ polite) |
        (questionPrefix ~ "what" ~ opt("are" | "is") ~ opt("all") ~ opt("the") ~ "planets" ~ opt(
          "in the solar system" | "that exist" | "that you know"
        ) ~ questionSuffix) |
        (questionPrefix ~ "name" ~ opt("all") ~ opt("the") ~ "planets" ~ opt(
          "for me" | "in our solar system"
        ) ~ questionSuffix) |
        (wantExpression ~ opt("a list of" | "to know" | "to see") ~ opt("all") ~ opt("the") ~ "planets" ~ polite) |
        ("planets" ~ opt("list" | "all" | "info" | "information")) |
        ((showVerb | "list") ~ meObj ~ opt("all" | "the") ~ category ~ polite) |
        (questionPrefix ~ "what" ~ opt("are" | "is") ~ opt("all") ~ opt("the") ~ category ~ opt(
          "that exist" | "that you know"
        ) ~ questionSuffix) |
        (questionPrefix ~ "name" ~ opt("all") ~ opt("the") ~ category ~ opt("for me") ~ questionSuffix) |
        (wantExpression ~ opt("a list of" | "to know" | "to see") ~ opt("all") ~ opt("the") ~ category ~ polite) |
        (category ~ opt("list" | "all" | "info" | "information"))
    ) ~ thanks ~ honorific) ^^ { case _ ~ content ~ _ ~ _ =>
      val contentStr = content.toString.toLowerCase
      if (contentStr.contains("planet")) ListPlanets
      else {
        val categoryWords = List(
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
        val foundCategory = categoryWords.find(cat => contentStr.contains(cat)).getOrElse("unknown")
        ListCategory(foundCategory)
      }
    }
  }

  private def randomFactCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      ("random" ~ opt("space" | "astronomy" | "cool" | "interesting") ~ ("fact" | "info" | "trivia") ~ polite) |
        (tellVerb ~ meObj ~ opt("a" | "some") ~ opt(
          "random" | "cool" | "interesting" | "fun" | "amazing" | "mind-blowing"
        ) ~ ("fact" | "trivia" | "information" | "knowledge" | "tidbit") ~ opt(
          "about space" | "about astronomy" | "about the universe" | "about our solar system"
        ) ~ polite) |
        (giveVerb ~ meObj ~ opt("a" | "some") ~ opt(
          "random" | "cool" | "interesting"
        ) ~ ("fact" | "trivia" | "information") ~ polite) |
        (questionPrefix ~ tellVerb ~ meObj ~ opt(
          "something"
        ) ~ ("cool" | "interesting" | "amazing" | "fascinating" | "wild") ~ opt(
          "about space" | "about astronomy" | "about the universe" | "about our solar system"
        ) ~ questionSuffix) |
        (wantExpression ~ opt("a" | "some") ~ opt(
          "random" | "cool" | "interesting"
        ) ~ ("fact" | "trivia" | "knowledge" | "information") ~ polite) |
        (contraction ~ opt("a") ~ ("cool fact" | "random fact" | "interesting fact" | "fun fact") ~ questionSuffix) |
        ("surprise me" ~ opt(
          "with" ~ opt("a" | "some") ~ opt("cool" | "interesting") ~ ("fact" | "trivia" | "information")
        )) |
        ("did you know" ~ questionSuffix) |
        ("tell me something i don't know" ~ opt(
          "about space" | "about astronomy" | "about the universe" | "about our solar system"
        ))
    ) ~ thanks ~ honorific) ^^ { case _ => RandomFact }
  }

  private def quizCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      (("start" | "begin" | "launch" | "initiate" | "let's do" | "give me") ~ opt(
        "a" | "the"
      ) ~ ("quiz" | "trivia" | "test" | "game" | "challenge") ~ polite) |
        (questionPrefix ~ ("start" | "begin" | "have" | "do") ~ opt(
          "a" | "the"
        ) ~ ("quiz" | "trivia" | "test" | "challenge") ~ opt("for me" | "with me") ~ questionSuffix) |
        (wantExpression ~ opt("to") ~ opt("take" | "do" | "try" | "play") ~ opt(
          "a" | "the"
        ) ~ ("quiz" | "trivia" | "test" | "challenge") ~ polite) |
        (tellVerb ~ meObj ~ opt("a" | "some") ~ ("quiz" | "trivia") ~ opt("question" | "questions") ~ polite) |
        ("quiz" ~ "me" ~ opt("on" ~ opt("space" | "astronomy" | "planets" | "stars"))) |
        ("ask" ~ meObj ~ opt("a" | "some") ~ ("quiz" | "trivia") ~ opt("question" | "questions") ~ polite) |
        ("let's" ~ "play" ~ opt("a" | "some") ~ ("quiz" | "trivia" | "game") ~ polite) |
        ("test" ~ "my" ~ "knowledge" ~ opt("of" ~ opt("space" | "astronomy" | "planets" | "stars"))) |
        ("i'm" ~ "ready" ~ opt("for" | "to") ~ opt("a" | "the") ~ ("quiz" | "trivia" | "test" | "challenge"))
    ) ~ thanks ~ honorific) ^^ { case _ => StartQuiz }
  }

  private def compareCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      ("compare" ~ topic ~ ("to" | "and" | "with" | "vs" | "versus" | "against") ~ topic) |
        ("how" ~ opt("does" | "do") ~ topic ~ ("compare" | "stack up" | "measure up") ~
          opt("to" | "with" | "against") ~ topic ~ questionSuffix) |
        ("how" ~ opt("does" | "do") ~ topic ~ ("differ" | "compare") ~
          ("from" | "to" | "with" | "against") ~ topic ~ questionSuffix) |
        (questionPrefix ~ "compare" ~ topic ~ ("with" | "to" | "and" | "versus" | "vs" | "against") ~
          topic ~ questionSuffix) |
        (("what" ~ opt("is" | "are") ~ opt("the") ~ "difference" ~ opt("between") |
          "what's" ~ opt("the") ~ "difference" ~ opt("between") |
          "how" ~ opt("does" | "do") ~ opt("the")) ~
          topic ~ ("and" | "vs" | "versus" | "compared to" | "differ from") ~ topic ~ questionSuffix) |
        (tellVerb ~ meObj ~ opt("about") ~ opt("the") ~ ("difference" | "differences" | "distinction" | "comparison") ~
          opt("between") ~ topic ~ ("and" | "vs" | "versus" | "compared to") ~ topic) |
        ("what" ~ "makes" ~ topic ~ ("different" | "distinct" | "unique") ~
          opt("from" | "compared to") ~ topic ~ questionSuffix) |
        (wantExpression ~ opt("to") ~ ("know" | "understand" | "learn" | "see") ~ opt("the") ~
          ("difference" | "differences" | "distinction" | "comparison") ~ opt("between") ~
          topic ~ ("and" | "vs" | "versus") ~ topic)
    ) ~ thanks ~ honorific) ^^ { case _ ~ content ~ _ ~ _ =>
      val inputStr = content.toString
      val comparePatterns = List(
        """compare\s+(.*?)\s+(?:to|and|with|vs|versus|against)\s+(.*?)$""",
        """how\s+(?:does|do)?\s+(.*?)\s+(?:compare|stack up|measure up|differ)(?:\s+(?:to|with|against|from))?\s+(.*?)(?:\?|$)""",
        """(?:what(?:'s|\s+is|\s+are)?\s+(?:the)?\s+difference\s+(?:between)?|how\s+(?:does|do)?\s+(?:the)?)\s+(.*?)\s+(?:and|vs|versus|compared to|differ from)\s+(.*?)(?:\?|$)""",
        """(?:tell|inform|let me know|share|explain|describe|talk about)(?:\s+(?:me|us))?\s+(?:about)?\s+(?:the)?\s+(?:difference|differences|distinction|comparison)\s+(?:between)?\s+(.*?)\s+(?:and|vs|versus|compared to)\s+(.*?)$""",
        """what\s+makes\s+(.*?)\s+(?:different|distinct|unique)\s+(?:from|compared to)?\s+(.*?)(?:\?|$)""",
        """(?:i want|i'd like|i need|i wanna)(?:\s+to)?\s+(?:know|understand|learn|see)(?:\s+the)?\s+(?:difference|differences|distinction|comparison)\s+(?:between)?\s+(.*?)\s+(?:and|vs|versus)\s+(.*?)$"""
      )
      val topicPairs = comparePatterns.flatMap { pattern =>
        val regex = pattern.r
        regex.findFirstMatchIn(inputStr) match {
          case Some(m) if m.groupCount >= 2 => Some((m.group(1).trim, m.group(2).trim))
          case _                            => None
        }
      }
      topicPairs.headOption match {
        case Some((t1, t2)) => Compare(t1, t2)
        case None           => Unknown(inputStr)
      }
    }
  }

  private def answerQuizCommand: Parser[Command] = {
    (greeting ~ filler ~ topic ~ thanks ~ honorific) ^^ { case _ ~ _ ~ ans ~ _ ~ _ => AnswerQuiz(ans.trim) }
  }

  private def skipQuizCommand: Parser[Command] = {
    (greeting ~ filler ~ "skip" ~ polite ~ questionSuffix ~ thanks ~ honorific) ^^ { _ => StartQuiz }
  }

  private def exitQuizCommand: Parser[Command] = {
    (greeting ~ filler ~ ("exit" | "end" | "stop" | "quit") ~ opt(
      "quiz"
    ) ~ polite ~ questionSuffix ~ thanks ~ honorific) ^^ { _ =>
      Unknown("exit")
    }
  }

  private def askAboutCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      (tellVerb ~ meObj ~ opt("about" | "regarding") ~ topic ~ polite) |
        (questionPrefix ~ tellVerb ~ meObj ~ opt("about" | "regarding") ~ topic ~ questionSuffix) |
        ("what" ~ opt("is" | "are") ~ category ~ questionSuffix) |
        ("tell me about" ~ category) |
        ("who" ~ opt("is" | "are") ~ topic ~ questionSuffix) |
        ("where" ~ opt("is" | "are") ~ topic ~ questionSuffix) |
        ("when" ~ opt("is" | "are" | "did" | "does" | "will") ~ topic ~ questionSuffix) |
        ("why" ~ opt("is" | "are" | "did" | "does" | "will") ~ topic ~ questionSuffix) |
        ("how" ~ opt("is" | "are" | "did" | "does" | "will") ~ topic ~ questionSuffix) |
        (wantExpression ~ opt("some" | "more") ~ ("info" | "information" | "details" | "facts" | "knowledge") ~
          opt("about" | "on" | "regarding" | "concerning") ~ topic ~ polite) |
        (questionPrefix ~ "what" ~ opt("do you") ~ "know" ~ opt("about") ~ topic ~ questionSuffix) |
        (questionPrefix ~ "what" ~ "can" ~ "you" ~ tellVerb ~ meObj ~ opt("about") ~ topic ~ questionSuffix) |
        (contraction ~ opt("up with" | "the deal with" | "going on with" | "the story with") ~ topic ~ questionSuffix) |
        (topic ~ opt("info" | "facts" | "details" | "please" | "search" | "look up" | "lookup"))
    ) ~ thanks ~ honorific) ^^ { case _ ~ _ ~ content ~ _ ~ _ =>
      val queryText = content.toString
      val cleanupPatterns = List(
        """^(?:tell|inform|let me know|share|explain|describe|talk about)(?:\s+(?:me|us))?(?:\s+(?:about|regarding))?""",
        """^(?:can|could|would|will|do|does|should|may|might)(?:\s+you)?(?:\s+(?:tell|inform|let me know|share|explain|describe|talk about))(?:\s+(?:me|us))?(?:\s+(?:about|regarding))?""",
        """^(?:what|who|where|when|why|how)(?:\s+(?:is|are|did|does|will))?""",
        """^(?:i want|i'd like|i need|i wanna|i wish|gimme|lemme get|hook me up with|i'm interested in|i'm looking for)(?:\s+(?:some|more))?(?:\s+(?:info|information|details|facts|knowledge))?(?:\s+(?:about|on|regarding|concerning))?""",
        """^(?:what)(?:\s+(?:do you))?(?:\s+know)(?:\s+(?:about))?""",
        """^(?:what)(?:\s+can)(?:\s+you)(?:\s+(?:tell|inform|let me know|share|explain|describe|talk about))(?:\s+(?:me|us))?(?:\s+(?:about))?""",
        """^(?:what's|who's|where's|how's|when's|why's|that's|there's|it's)(?:\s+(?:up with|the deal with|going on with|the story with))?"""
      )
      val suffixPatterns = List(
        """(?:\?|\!)+$""",
        """(?:\s+(?:info|facts|details|please|search|look up|lookup))$"""
      )
      var topic = queryText
      cleanupPatterns.foreach { pattern =>
        topic = topic.replaceAll(pattern, "").trim
      }
      suffixPatterns.foreach { pattern =>
        topic = topic.replaceAll(pattern, "").trim
      }
      // Ensure topic is not empty and capitalize first letter
      if (topic.isEmpty) Unknown(queryText)
      else AskAbout(topic.capitalize)
    }
  }

  private def unknownCommand: Parser[Command] = {
    ".+".r ^^ (input => Unknown(input))
  }
}
