package chatbot.analytics

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.mutable

/** Enhanced Analytics for tracking comprehensive user interaction with the chatbot */
class Analytics {
  private var totalInteractions = 0
  private var commandCounts     = Map[String, Int]()
  private var quizzesStarted    = 0
  private var questionsAnswered = 0
  private var correctAnswers    = 0
  private var planetSearches    = Map[String, Int]()
  private var comparisonPairs   = Map[(String, String), Int]()

  // New analytics features
  private var sessionDurations: mutable.ListBuffer[Long]          = mutable.ListBuffer.empty // in seconds
  private var interactionTimes: mutable.ListBuffer[LocalDateTime] = mutable.ListBuffer.empty
  private var quizStats: Map[String, Int] = Map(
    "quizzes_started"    -> 0,
    "questions_answered" -> 0,
    "correct_answers"    -> 0
  )
  private var topSearchedPlanets: Map[String, Int]         = Map.empty
  private var topComparedPairs: Map[(String, String), Int] = Map.empty
  private var consecutiveInteractions: Int                 = 0
  private var maxConsecutiveInteractions: Int              = 0

  // Time of last interaction for measuring engagement
  private var lastInteractionTime: LocalDateTime = LocalDateTime.now()

  /** Logs a user interaction with the chatbot
    * @param command
    *   The command string issued by the user
    * @param isCorrectQuizAnswer
    *   Optional parameter to track quiz performance
    */

  def logQuizStart(quizType: String = "general"): Int = {
    // Log the interaction using existing method
    logInteraction("startquiz")

    // Additional quiz-specific tracking could be done here
    // For example, if we want to track different types of quizzes

    // Return the current count of quizzes started
    quizStats("quizzes_started")
  }

  /** Explicitly logs a quiz answer
    * @param questionId
    *   Identifier for the specific question
    * @param isCorrect
    *   Whether the answer was correct
    * @param answerTime
    *   Time taken to answer in seconds (optional)
    * @return
    *   Boolean indicating if the answer was correct
    */
  def logQuizAnswer(questionId: String, isCorrect: Boolean, answerTime: Option[Long] = None): Boolean = {
    // Log the interaction using existing method
    logInteraction(s"answerquiz_$questionId", isCorrect)

    // Additional answer-specific tracking could be added here
    // For example, tracking performance on specific questions

    // Return whether the answer was correct
    isCorrect
  }

  def logInteraction(command: String, isCorrectQuizAnswer: Boolean = false): Unit = {
    totalInteractions += 1
    val now = LocalDateTime.now()

    // Track time between interactions
    val timeSinceLastInteraction = java.time.Duration.between(lastInteractionTime, now).getSeconds
    if (timeSinceLastInteraction < 300) { // 5 minutes threshold for consecutive interactions
      consecutiveInteractions += 1
      maxConsecutiveInteractions = Math.max(maxConsecutiveInteractions, consecutiveInteractions)
    } else {
      consecutiveInteractions = 1
    }

    // Update last interaction time
    lastInteractionTime = now
    interactionTimes += now

    // Update command frequency
    commandCounts = commandCounts.updated(
      command.split("_").headOption.getOrElse("unknown"),
      commandCounts.getOrElse(command.split("_").headOption.getOrElse("unknown"), 0) + 1
    )

    // Log specific command types
    command match {
      case cmd if cmd.startsWith("askabout_") =>
        val topic = cmd.drop("askabout_".length).toLowerCase
        if (isPlanet(topic)) {
          topSearchedPlanets = topSearchedPlanets.updated(
            topic,
            topSearchedPlanets.getOrElse(topic, 0) + 1
          )
        }

      case cmd if cmd.startsWith("compare_") =>
        val parts = cmd.drop("compare_".length).split("_")
        if (parts.length >= 2) {
          val topic1 = parts(0).toLowerCase
          val topic2 = parts(1).toLowerCase
          val pair   = if (topic1 < topic2) (topic1, topic2) else (topic2, topic1)
          topComparedPairs = topComparedPairs.updated(
            pair,
            topComparedPairs.getOrElse(pair, 0) + 1
          )
        }

      case "startquiz" =>
        quizStats = quizStats.updated(
          "quizzes_started",
          quizStats("quizzes_started") + 1
        )

      case cmd if cmd.startsWith("answerquiz_") =>
        quizStats = quizStats.updated(
          "questions_answered",
          quizStats("questions_answered") + 1
        )
        if (isCorrectQuizAnswer) {
          quizStats = quizStats.updated(
            "correct_answers",
            quizStats("correct_answers") + 1
          )
        }

      case _ => // No special handling for other commands
    }

    // In a real implementation, you might want to log this to a file or database
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    println(s"[DEBUG] $timestamp: User issued command: $command")
  }

  /** Records the end of a user session Useful for tracking how long users typically engage with the chatbot
    */
  def endSession(): Unit = {
    val sessionDuration = java.time.Duration.between(lastInteractionTime, LocalDateTime.now()).getSeconds
    sessionDurations += sessionDuration
  }

  /** Gets comprehensive statistics about user interactions
    * @return
    *   Map containing detailed usage statistics
    */
  def getStats: Map[String, String] = {
    val runTime = java.time.Duration.between(LocalDateTime.now(), LocalDateTime.now())
    val hours   = runTime.toHours
    val minutes = runTime.toMinutesPart

    // Calculate engagement metrics
    val avgTimeBetweenCommands = calculateAverageTimeBetweenCommands()
    val peakUsageHour          = calculatePeakUsageHour()
    val quizCompletionRate =
      if (quizStats("quizzes_started") > 0)
        quizStats("questions_answered").toDouble / quizStats("quizzes_started")
      else 0.0
    val quizSuccessRate =
      if (quizStats("questions_answered") > 0)
        quizStats("correct_answers").toDouble / quizStats("questions_answered") * 100
      else 0.0

    Map(
      "total_interactions"        -> totalInteractions.toString,
      "runtime"                   -> f"${hours}h ${minutes}m",
      "most_used_command"         -> getMostUsedCommand,
      "command_frequencies"       -> commandCounts.toString,
      "longest_engagement_streak" -> maxConsecutiveInteractions.toString,
      "avg_time_between_commands" -> f"${avgTimeBetweenCommands}%.1f seconds",
      "peak_usage_hour"           -> peakUsageHour.toString,
      "quizzes_started"           -> quizStats("quizzes_started").toString,
      "quiz_questions_answered"   -> quizStats("questions_answered").toString,
      "quiz_completion_rate"      -> f"${quizCompletionRate}%.2f questions per quiz",
      "quiz_success_rate"         -> f"${quizSuccessRate}%.1f%%",
      "most_searched_planet"      -> getMostSearchedPlanet,
      "most_compared_pair"        -> getMostComparedPair
    )
  }

  /** Gets the dashboard string in the format expected by WebServer.scala */
  def getDashboard: String = {
    val stats = getStats
    s"""Analytics Dashboard
=================
Total Interactions: ${stats("total_interactions")}
Most Used Command: ${stats("most_used_command")}
Quizzes Started: ${stats("quizzes_started")}
Questions Answered: ${stats("quiz_questions_answered")}
Quiz Success Rate: ${stats("quiz_success_rate")}
Most Searched Planet: ${stats("most_searched_planet")}
Most Compared Pair: ${stats("most_compared_pair")}
================="""
  }

  /** Gets the most commonly used command
    * @return
    *   String representing the most used command
    */
  private def getMostUsedCommand: String = {
    if (commandCounts.isEmpty) {
      "None"
    } else {
      commandCounts.maxBy(_._2)._1
    }
  }

  /** Gets the most searched planet
    * @return
    *   String representing the most searched planet
    */
  private def getMostSearchedPlanet: String = {
    if (topSearchedPlanets.isEmpty) {
      "None"
    } else {
      topSearchedPlanets.maxBy(_._2)._1.capitalize
    }
  }

  /** Gets the most compared planet pair
    * @return
    *   String representing the most compared pair
    */
  private def getMostComparedPair: String = {
    if (topComparedPairs.isEmpty) {
      "None"
    } else {
      val (pair, count) = topComparedPairs.maxBy(_._2)
      s"${pair._1} vs ${pair._2} ($count times)"
    }
  }

  /** Calculates the average time between commands in seconds
    * @return
    *   Double representing average time in seconds
    */
  private def calculateAverageTimeBetweenCommands(): Double = {
    if (interactionTimes.size < 2) return 0.0

    var totalSeconds = 0.0
    for (i <- 1 until interactionTimes.size) {
      val duration = java.time.Duration.between(interactionTimes(i - 1), interactionTimes(i)).getSeconds
      totalSeconds += duration
    }

    totalSeconds / (interactionTimes.size - 1)
  }

  /** Determines the hour of day with most interactions
    * @return
    *   Int representing hour (0-23)
    */
  private def calculatePeakUsageHour(): Int = {
    if (interactionTimes.isEmpty) return 0

    val hourCounts = interactionTimes.groupBy(_.getHour).map { case (hour, times) => (hour, times.size) }
    if (hourCounts.isEmpty) 0 else hourCounts.maxBy(_._2)._1
  }

  /** Checks if a topic is a planet
    * @param topic
    *   The topic to check
    * @return
    *   Boolean indicating if the topic is a planet
    */
  private def isPlanet(topic: String): Boolean = {
    val planets = Set("mars", "jupiter", "saturn", "uranus", "neptune", "venus", "mercury", "earth", "pluto")
    planets.contains(topic.toLowerCase)
  }
}
