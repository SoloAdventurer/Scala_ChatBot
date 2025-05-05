package chatbot.analytics

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Pure functional analytics module for tracking user interaction with the chatbot */
object Analytics {
  // Define our state type
  type AnalyticsState = (
    Int,                        // totalInteractions
    Map[String, Int],           // commandCounts
    Map[String, Int],           // quizStats
    Map[String, Int],           // topSearchedPlanets
    Map[(String, String), Int], // topComparedPairs
    Int,                        // consecutiveInteractions
    Int,                        // maxConsecutiveInteractions
    LocalDateTime,              // lastInteractionTime
    List[LocalDateTime],        // interactionTimes
    List[Long]                  // sessionDurations
  )

  // Create initial state
  def initialState: AnalyticsState = (
    0,                      // totalInteractions
    Map.empty[String, Int], // commandCounts
    Map(
      "quizzes_started"    -> 0,
      "questions_answered" -> 0,
      "correct_answers"    -> 0
    ),                                // quizStats
    Map.empty[String, Int],           // topSearchedPlanets
    Map.empty[(String, String), Int], // topComparedPairs
    0,                                // consecutiveInteractions
    0,                                // maxConsecutiveInteractions
    LocalDateTime.now(),              // lastInteractionTime
    List.empty[LocalDateTime],        // interactionTimes
    List.empty[Long]                  // sessionDurations
  )

  /** Logs a user interaction, returning a new state
    * @param state
    *   Current analytics state
    * @param command
    *   The command string issued by the user
    * @param isCorrectQuizAnswer
    *   Optional parameter to track quiz performance
    * @return
    *   Updated analytics state
    */
  def logInteraction(state: AnalyticsState, command: String, isCorrectQuizAnswer: Boolean = false): AnalyticsState = {
    val (
      totalInteractions,
      commandCounts,
      quizStats,
      topSearchedPlanets,
      topComparedPairs,
      _, // consecutiveInteractions (will be recalculated)
      maxConsecutiveInteractions,
      lastInteractionTime,
      interactionTimes,
      sessionDurations
    ) = state

    val now = LocalDateTime.now()

    // Track time between interactions
    val timeSinceLastInteraction = java.time.Duration.between(lastInteractionTime, now).getSeconds
    val (newConsecutiveInteractions, newMaxConsecutiveInteractions) =
      if (timeSinceLastInteraction < 300) { // 5 minutes threshold for consecutive interactions
        val consecutiveCount = state._6 + 1
        (consecutiveCount, Math.max(maxConsecutiveInteractions, consecutiveCount))
      } else {
        (1, maxConsecutiveInteractions)
      }

    // Update command frequency
    val commandType          = command.split("_").headOption.getOrElse("unknown")
    val updatedCommandCounts = commandCounts.updated(commandType, commandCounts.getOrElse(commandType, 0) + 1)

    // Process command-specific updates using pattern matching
    val (updatedTopSearchedPlanets, updatedTopComparedPairs, updatedQuizStats) = command match {
      case cmd if cmd.startsWith("askabout_") =>
        val topic = cmd.drop("askabout_".length).toLowerCase
        if (isPlanet(topic)) {
          (
            topSearchedPlanets.updated(topic, topSearchedPlanets.getOrElse(topic, 0) + 1),
            topComparedPairs,
            quizStats
          )
        } else {
          (topSearchedPlanets, topComparedPairs, quizStats)
        }

      case cmd if cmd.startsWith("compare_") =>
        val parts = cmd.drop("compare_".length).split("_")
        if (parts.length >= 2) {
          val topic1 = parts(0).toLowerCase
          val topic2 = parts(1).toLowerCase
          val pair   = if (topic1 < topic2) (topic1, topic2) else (topic2, topic1)
          (
            topSearchedPlanets,
            topComparedPairs.updated(pair, topComparedPairs.getOrElse(pair, 0) + 1),
            quizStats
          )
        } else {
          (topSearchedPlanets, topComparedPairs, quizStats)
        }

      case "startquiz" =>
        (
          topSearchedPlanets,
          topComparedPairs,
          quizStats.updated("quizzes_started", quizStats("quizzes_started") + 1)
        )

      case cmd if cmd.startsWith("answerquiz_") =>
        val updatedAnsweredStats = quizStats.updated(
          "questions_answered",
          quizStats("questions_answered") + 1
        )

        val finalQuizStats = if (isCorrectQuizAnswer) {
          updatedAnsweredStats.updated(
            "correct_answers",
            updatedAnsweredStats("correct_answers") + 1
          )
        } else {
          updatedAnsweredStats
        }

        (topSearchedPlanets, topComparedPairs, finalQuizStats)

      case _ =>
        (topSearchedPlanets, topComparedPairs, quizStats)
    }

    // Log in a real implementation
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    println(s"[DEBUG] $timestamp: User issued command: $command")

    // Return new state
    (
      totalInteractions + 1,
      updatedCommandCounts,
      updatedQuizStats,
      updatedTopSearchedPlanets,
      updatedTopComparedPairs,
      newConsecutiveInteractions,
      newMaxConsecutiveInteractions,
      now,
      now :: interactionTimes,
      sessionDurations
    )
  }

  /** Records the end of a user session, returning a new state
    * @param state
    *   Current analytics state
    * @return
    *   Updated analytics state
    */
  def endSession(state: AnalyticsState): AnalyticsState = {
    val sessionDuration = java.time.Duration.between(state._8, LocalDateTime.now()).getSeconds
    (
      state._1,                    // totalInteractions
      state._2,                    // commandCounts
      state._3,                    // quizStats
      state._4,                    // topSearchedPlanets
      state._5,                    // topComparedPairs
      state._6,                    // consecutiveInteractions
      state._7,                    // maxConsecutiveInteractions
      state._8,                    // lastInteractionTime
      state._9,                    // interactionTimes
      sessionDuration :: state._10 // sessionDurations
    )
  }

  /** Gets comprehensive statistics about user interactions
    * @param state
    *   Current analytics state
    * @return
    *   Map containing detailed usage statistics
    */
  def getStats(state: AnalyticsState): Map[String, String] = {
    val (
      totalInteractions,
      commandCounts,
      quizStats,
      topSearchedPlanets,
      topComparedPairs,
      _,
      maxConsecutiveInteractions,
      _,
      interactionTimes,
      _
    ) = state

    val runTime = java.time.Duration.between(LocalDateTime.now(), LocalDateTime.now())
    val hours   = runTime.toHours
    val minutes = runTime.toMinutesPart

    // Calculate engagement metrics
    val avgTimeBetweenCommands = calculateAverageTimeBetweenCommands(interactionTimes)
    val peakUsageHour          = calculatePeakUsageHour(interactionTimes)
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
      "most_used_command"         -> getMostUsedCommand(commandCounts),
      "command_frequencies"       -> commandCounts.toString,
      "longest_engagement_streak" -> maxConsecutiveInteractions.toString,
      "avg_time_between_commands" -> f"${avgTimeBetweenCommands}%.1f seconds",
      "peak_usage_hour"           -> peakUsageHour.toString,
      "quizzes_started"           -> quizStats("quizzes_started").toString,
      "quiz_questions_answered"   -> quizStats("questions_answered").toString,
      "quiz_completion_rate"      -> f"${quizCompletionRate}%.2f questions per quiz",
      "quiz_success_rate"         -> f"${quizSuccessRate}%.1f%%",
      "most_searched_planet"      -> getMostSearchedPlanet(topSearchedPlanets),
      "most_compared_pair"        -> getMostComparedPair(topComparedPairs)
    )
  }

  /** Gets the dashboard string in the format expected by WebServer
    * @param state
    *   Current analytics state
    * @return
    *   Formatted dashboard string
    */
  def getDashboard(state: AnalyticsState): String = {
    val stats = getStats(state)
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

  /** Helper functions */

  private def getMostUsedCommand(commandCounts: Map[String, Int]): String = {
    if (commandCounts.isEmpty) {
      "None"
    } else {
      commandCounts.maxBy(_._2)._1
    }
  }

  private def getMostSearchedPlanet(topSearchedPlanets: Map[String, Int]): String = {
    if (topSearchedPlanets.isEmpty) {
      "None"
    } else {
      topSearchedPlanets.maxBy(_._2)._1.capitalize
    }
  }

  private def getMostComparedPair(topComparedPairs: Map[(String, String), Int]): String = {
    if (topComparedPairs.isEmpty) {
      "None"
    } else {
      val (pair, count) = topComparedPairs.maxBy(_._2)
      s"${pair._1} vs ${pair._2} ($count times)"
    }
  }

  private def calculateAverageTimeBetweenCommands(interactionTimes: List[LocalDateTime]): Double = {
    val sortedTimes = interactionTimes.sortBy(_.toEpochSecond(java.time.ZoneOffset.UTC))
    if (sortedTimes.size < 2) return 0.0

    val timePairs = sortedTimes.zip(sortedTimes.tail)
    val totalSeconds = timePairs.map { case (t1, t2) =>
      java.time.Duration.between(t1, t2).abs().getSeconds.toDouble
    }.sum

    totalSeconds / timePairs.size
  }

  private def calculatePeakUsageHour(interactionTimes: List[LocalDateTime]): Int = {
    if (interactionTimes.isEmpty) return 0

    val hourCounts = interactionTimes.groupBy(_.getHour).map { case (hour, times) => (hour, times.size) }
    if (hourCounts.isEmpty) 0 else hourCounts.maxBy(_._2)._1
  }

  private def isPlanet(topic: String): Boolean = {
    val planets = Set("mars", "jupiter", "saturn", "uranus", "neptune", "venus", "mercury", "earth", "pluto")
    planets.contains(topic.toLowerCase)
  }
}
