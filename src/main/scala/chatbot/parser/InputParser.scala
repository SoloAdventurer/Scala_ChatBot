package chatbot.parser

import scala.util.parsing.combinator.RegexParsers
import chatbot.parser.AST.Command
import chatbot.parser.AST.Command._

class InputParser extends RegexParsers {
  override val whiteSpace = """\s+""".r

  def parseInput(input: String): Either[String, Command] = {
    // Convert to lowercase for case-insensitive matching
    val normalizedInput = input.toLowerCase.trim
    parseAll(command, normalizedInput) match {
      case Success(result, _) => Right(result)
      case NoSuccess(_, _)    => Right(Unknown(input)) // Return Unknown for invalid input
      case Failure(msg, _)    => Left(s"Parsing failed: $msg")
      case Error(msg, _)      => Left(s"Parsing error: $msg")
    }
  }

  private def command: Parser[Command] = {
    // Try more specific commands first, fall back to general patterns
    (helpCommand |
      listCommand |
      randomFactCommand |
      quizCommand |
      compareCommand |
      answerQuizCommand |
      askAboutCommand |
      unknownCommand)
  }

  // =================== Common Elements ===================

  // Common prefixes and fillers
  private def greeting: Parser[String] =
    opt("hi" | "hello" | "hey" | "yo" | "hiya" | "greetings") ^^ { case _ => "" }

  private def filler: Parser[String] =
    rep(("um" | "uh" | "like" | "so" | "well" | "uhm" | "hmm" | "err" | "you know")) ^^ { case _ => "" }

  private def honorific: Parser[String] =
    opt("bot" | "chatbot" | "assistant" | "buddy" | "friend" | "pal" | "dude") ^^ { case _ => "" }

  private def polite: Parser[String] =
    opt(
      "please" | "pls" | "plz" | "if you can" | "if you could" | "would you" | "if possible" | "pretty please" | "kindly" | "if you don't mind"
    ) ^^ { case _ => "" }

  private def thanks: Parser[String] =
    opt("thanks" | "thank you" | "thx" | "ty" | "appreciate it") ^^ { case _ => "" }

  private def urgency: Parser[String] =
    opt("quickly" | "asap" | "right now" | "immediately" | "when you get a chance") ^^ { case _ => "" }

  // Question prefixes & variants
  private def questionPrefix: Parser[String] =
    opt("can" | "could" | "would" | "will" | "do" | "does" | "should" | "may" | "might") ~ opt("you" | "ya" | "u") ^^ {
      case _ => ""
    }

  private def questionSuffix: Parser[String] =
    opt("?" | "??" | "???" | "!?" | "!") ^^ { case _ => "" }

  // Verbs & actions
  private def showVerb: Parser[String] =
    "show" | "display" | "list" | "reveal" | "present" | "exhibit" | "demonstrate"

  private def tellVerb: Parser[String] =
    "tell" | "inform" | "let me know" | "share" | "explain" | "describe" | "talk about" | "give info on" | "enlighten me about"

  private def giveVerb: Parser[String] =
    "give" | "provide" | "offer" | "share" | "let me have" | "hit me with" | "throw me" | "send"

  private def generalVerb: Parser[String] =
    showVerb | tellVerb | giveVerb | "find" | "search for" | "look up" | "check" | "get"

  // Pronouns & connectors
  private def meObj: Parser[String] =
    opt("me" | "us" | "myself" | "ourselves") ^^ { case _ => "" }

  private def preposition: Parser[String] =
    "about" | "on" | "regarding" | "concerning" | "for" | "related to" | "pertaining to" | "dealing with" | "in regards to" | "with respect to" | "of"

  // Contractions & informal language
  private def contraction: Parser[String] =
    "what's" | "who's" | "where's" | "how's" | "when's" | "why's" | "that's" | "there's" | "it's" | "i'm" | "you're" | "we're" | "they're" | "isn't" | "aren't" | "wasn't" | "weren't" | "don't" | "can't" | "won't" | "shouldn't" | "couldn't" | "wouldn't"

  private def wantExpression: Parser[String] =
    "i want" | "i'd like" | "i need" | "i wanna" | "i wish" | "gimme" | "lemme get" | "hook me up with" | "i'm interested in" | "i'm looking for"

  // Topics and subjects
  private def topic: Parser[String] =
    """[a-zA-Z0-9\s\-\.\,\'\!\@\#\$\%\^\&\*\(\)\_\+\=\[\]\{\}\;\:\"\,\.\/\<\>\?]+""".r

  private def category: Parser[String] =
    "planets" | "stars" | "constellations" | "moons" | "dwarf planets" | "galaxies" | "black holes" | "asteroids" | "comets" | "nebulae" | "star systems" | "exoplanets"

  // =================== Command Parsers ===================

  // Help command with expanded patterns
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

  // List commands - expanded to handle various categories
  private def listCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      // List planets specific patterns
      ((showVerb | "list") ~ meObj ~ opt("all" | "the") ~ "planets" ~ polite) |
        (questionPrefix ~ "what" ~ opt("are" | "is") ~ opt("all") ~ opt("the") ~ "planets" ~ opt(
          "in the solar system" | "that exist" | "that you know"
        ) ~ questionSuffix) |
        (questionPrefix ~ "name" ~ opt("all") ~ opt("the") ~ "planets" ~ opt(
          "for me" | "in our solar system"
        ) ~ questionSuffix) |
        (wantExpression ~ opt("a list of" | "to know" | "to see") ~ opt("all") ~ opt("the") ~ "planets" ~ polite) |
        ("planets" ~ opt("list" | "all" | "info" | "information")) |

        // General list category patterns
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
        // Extract the category - find any of the category keywords
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

  // Random fact command with more conversational patterns
  private def randomFactCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      ("random" ~ opt("space" | "astronomy" | "cool" | "interesting") ~ ("fact" | "info" | "trivia") ~ polite) |
        (tellVerb ~ meObj ~ opt("a" | "some") ~ opt(
          "random" | "cool" | "interesting" | "fun" | "amazing" | "mind-blowing"
        ) ~ ("fact" | "trivia" | "information" | "knowledge" | "tidbit") ~ opt(
          "about space" | "about astronomy" | "about the universe"
        ) ~ polite) |
        (giveVerb ~ meObj ~ opt("a" | "some") ~ opt(
          "random" | "cool" | "interesting"
        ) ~ ("fact" | "trivia" | "information") ~ polite) |
        (questionPrefix ~ tellVerb ~ meObj ~ opt(
          "something"
        ) ~ ("cool" | "interesting" | "amazing" | "fascinating" | "wild") ~ opt(
          "about space" | "about astronomy" | "about the universe"
        ) ~ questionSuffix) |
        (wantExpression ~ opt("a" | "some") ~ opt(
          "random" | "cool" | "interesting"
        ) ~ ("fact" | "trivia" | "knowledge" | "information") ~ polite) |
        (contraction ~ opt("a") ~ ("cool fact" | "random fact" | "interesting fact" | "fun fact") ~ questionSuffix) |
        ("surprise me" ~ opt(
          "with" ~ opt("a" | "some") ~ opt("cool" | "interesting") ~ ("fact" | "trivia" | "information")
        )) |
        ("did you know" ~ questionSuffix) |
        ("tell me something i don't know" ~ opt("about space" | "about astronomy" | "about the universe"))
    ) ~ thanks ~ honorific) ^^ { case _ => RandomFact }
  }

  // Quiz command with more natural patterns
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
    ) ~ thanks ~ honorific) ^^ { case _ =>
      StartQuiz
    }
  }

  // Compare command with improved natural language patterns
  private def compareCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      // Direct comparison patterns
      ("compare" ~ topic ~ ("to" | "and" | "with" | "vs" | "versus" | "against") ~ topic) |

        // Question patterns about comparison
        ("how" ~ opt("does" | "do") ~ topic ~ ("compare" | "stack up" | "measure up") ~
          opt("to" | "with" | "against") ~ topic ~ questionSuffix) |

        ("how" ~ opt("does" | "do") ~ topic ~ ("differ" | "compare") ~
          ("from" | "to" | "with" | "against") ~ topic ~ questionSuffix) |

        (questionPrefix ~ "compare" ~ topic ~ ("with" | "to" | "and" | "versus" | "vs" | "against") ~
          topic ~ questionSuffix) |

        // What's the difference patterns
        (("what" ~ opt("is" | "are") ~ opt("the") ~ "difference" ~ opt("between") |
          "what's" ~ opt("the") ~ "difference" ~ opt("between") |
          "how" ~ opt("does" | "do") ~ opt("the")) ~
          topic ~ ("and" | "vs" | "versus" | "compared to" | "differ from") ~ topic ~ questionSuffix) |

        // Tell me about the differences
        (tellVerb ~ meObj ~ opt("about") ~ opt("the") ~ ("difference" | "differences" | "distinction" | "comparison") ~
          opt("between") ~ topic ~ ("and" | "vs" | "versus" | "compared to") ~ topic) |

        // What makes X different from Y
        ("what" ~ "makes" ~ topic ~ ("different" | "distinct" | "unique") ~
          opt("from" | "compared to") ~ topic ~ questionSuffix) |

        // I want to understand the difference
        (wantExpression ~ opt("to") ~ ("know" | "understand" | "learn" | "see") ~ opt("the") ~
          ("difference" | "differences" | "distinction" | "comparison") ~ opt("between") ~
          topic ~ ("and" | "vs" | "versus") ~ topic)
    ) ~ thanks ~ honorific) ^^ { case _ ~ content ~ _ ~ _ =>
      // Find the two topics to compare
      val inputStr = content.toString

      // Common patterns for extracting comparison topics
      val comparePatterns = List(
        """compare\s+(.*?)\s+(?:to|and|with|vs|versus|against)\s+(.*?)$""",
        """how\s+(?:does|do)?\s+(.*?)\s+(?:compare|stack up|measure up|differ)(?:\s+(?:to|with|against|from))?\s+(.*?)(?:\?|$)""",
        """(?:what(?:'s|\s+is|\s+are)?\s+(?:the)?\s+difference\s+(?:between)?|how\s+(?:does|do)?\s+(?:the)?)\s+(.*?)\s+(?:and|vs|versus|compared to|differ from)\s+(.*?)(?:\?|$)""",
        """(?:tell|inform|let me know|share|explain|describe|talk about)(?:\s+(?:me|us))?\s+(?:about)?\s+(?:the)?\s+(?:difference|differences|distinction|comparison)\s+(?:between)?\s+(.*?)\s+(?:and|vs|versus|compared to)\s+(.*?)$""",
        """what\s+makes\s+(.*?)\s+(?:different|distinct|unique)\s+(?:from|compared to)?\s+(.*?)(?:\?|$)""",
        """(?:i want|i'd like|i need|i wanna)(?:\s+to)?\s+(?:know|understand|learn|see)(?:\s+the)?\s+(?:difference|differences|distinction|comparison)\s+(?:between)?\s+(.*?)\s+(?:and|vs|versus)\s+(.*?)$"""
      )

      // Try each pattern to extract the topics
      val topicPairs = comparePatterns.flatMap { pattern =>
        val regex = pattern.r
        regex.findFirstMatchIn(inputStr) match {
          case Some(m) if m.groupCount >= 2 =>
            Some((m.group(1).trim, m.group(2).trim))
          case _ => None
        }
      }

      topicPairs.headOption match {
        case Some((t1, t2)) => Compare(t1, t2)
        // If we can't extract topics, return Unknown
        case None => Unknown(inputStr)
      }
    }
  }

  // Answer quiz command with more variations
  private def answerQuizCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      ("answer" ~ opt("is") ~ topic) |
        (("my answer is" | "my response is" | "i answer" | "i respond" | "i say" | "i think" | "i believe" |
          "i guess" | "maybe" | "perhaps" | "possibly" | "i'm thinking" | "i'm going with" | "i'll go with" |
          "i'll say" | "i'll choose" | "i'll pick" | "i choose" | "i pick" | "i select" | "my choice is" |
          "the answer is" | "it's" | "it is" | "that's" | "that is") ~ topic) |
        (questionPrefix ~ ("is it" | "could it be" | "would it be" | "is the answer") ~ topic ~ questionSuffix) |
        (topic ~ opt(
          "is my answer" | "is my response" | "is my choice" | "is what i pick" | "is what i choose" | "is what i'm going with" | "final answer"
        ))
    ) ~ thanks ~ honorific) ^^ { case _ ~ content ~ _ ~ _ =>
      val answer = content.toString
        .replaceAll(
          """(?i)(?:answer(?:\s+is)?|my answer is|my response is|i answer|i respond|i say|i think|i believe|i guess|
             maybe|perhaps|possibly|i'm thinking|i'm going with|i'll go with|i'll say|i'll choose|i'll pick|
             i choose|i pick|i select|my choice is|the answer is|it's|it is|that's|that is|is it|could it be|
             would it be|is the answer|is my answer|is my response|is my choice|is what i pick|is what i choose|
             is what i'm going with|final answer|\?|\.)""".stripMargin.replaceAll("\n", ""),
          ""
        )
        .trim
      AnswerQuiz(answer)
    }
  }

  // Ask about command with enhanced natural patterns
  private def askAboutCommand: Parser[Command] = {
    (greeting ~ filler ~ (
      // Direct questions
      (tellVerb ~ meObj ~ opt("about" | "regarding") ~ topic ~ polite) |
        (questionPrefix ~ tellVerb ~ meObj ~ opt("about" | "regarding") ~ topic ~ questionSuffix) |
        ("what" ~ opt("is" | "are") ~ topic ~ questionSuffix) |
        ("who" ~ opt("is" | "are") ~ topic ~ questionSuffix) |
        ("where" ~ opt("is" | "are") ~ topic ~ questionSuffix) |
        ("when" ~ opt("is" | "are" | "did" | "does" | "will") ~ topic ~ questionSuffix) |
        ("why" ~ opt("is" | "are" | "did" | "does" | "will") ~ topic ~ questionSuffix) |
        ("how" ~ opt("is" | "are" | "did" | "does" | "will") ~ topic ~ questionSuffix) |

        // Information requests
        (wantExpression ~ opt("some" | "more") ~ ("info" | "information" | "details" | "facts" | "knowledge") ~
          opt("about" | "on" | "regarding" | "concerning") ~ topic ~ polite) |

        // What do you know about X
        (questionPrefix ~ "what" ~ opt("do you") ~ "know" ~ opt("about") ~ topic ~ questionSuffix) |

        // What can you tell me about X
        (questionPrefix ~ "what" ~ "can" ~ "you" ~ tellVerb ~ meObj ~ opt("about") ~ topic ~ questionSuffix) |

        // Informal patterns
        (contraction ~ opt("up with" | "the deal with" | "going on with" | "the story with") ~ topic ~ questionSuffix) |

        // Search-like queries
        (topic ~ opt("info" | "facts" | "details" | "please" | "search" | "look up" | "lookup"))
    ) ~ thanks ~ honorific) ^^ { case _ ~ content ~ _ ~ _ =>
      val queryText = content.toString

      // Common patterns to clean up the query and extract the actual topic
      val cleanupPatterns = List(
        // Remove question beginnings
        """^(?:tell|inform|let me know|share|explain|describe|talk about)(?:\s+(?:me|us))?(?:\s+(?:about|regarding))?""",
        """^(?:can|could|would|will|do|does|should|may|might)(?:\s+you)?(?:\s+(?:tell|inform|let me know|share|explain|describe|talk about))(?:\s+(?:me|us))?(?:\s+(?:about|regarding))?""",
        """^(?:what|who|where|when|why|how)(?:\s+(?:is|are|did|does|will))?""",
        """^(?:i want|i'd like|i need|i wanna|i wish|gimme|lemme get|hook me up with|i'm interested in|i'm looking for)(?:\s+(?:some|more))?(?:\s+(?:info|information|details|facts|knowledge))?(?:\s+(?:about|on|regarding|concerning))?""",
        """^(?:what)(?:\s+(?:do you))?(?:\s+know)(?:\s+(?:about))?""",
        """^(?:what)(?:\s+can)(?:\s+you)(?:\s+(?:tell|inform|let me know|share|explain|describe|talk about))(?:\s+(?:me|us))?(?:\s+(?:about))?""",
        """^(?:what's|who's|where's|how's|when's|why's|that's|there's|it's)(?:\s+(?:up with|the deal with|going on with|the story with))?"""
      )

      // Remove question suffixes
      val suffixPatterns = List(
        """(?:\?|\!)+$""",
        """(?:\s+(?:info|facts|details|please|search|look up|lookup))$"""
      )

      // Apply cleanup patterns to extract the topic
      var topic = queryText
      cleanupPatterns.foreach { pattern =>
        topic = topic.replaceAll(pattern, "").trim
      }

      suffixPatterns.foreach { pattern =>
        topic = topic.replaceAll(pattern, "").trim
      }

      if (topic.isEmpty) Unknown(queryText)
      else AskAbout(topic)
    }
  }

  // Unknown command - catch all
  private def unknownCommand: Parser[Command] = {
    ".+".r ^^ (input => Unknown(input))
  }
}
