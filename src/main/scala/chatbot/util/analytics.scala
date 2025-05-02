package chatbot.analytics

import chatbot.parser.AST.Command
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Tracks user interaction with the chatbot
  */
class Analytics {
  private var interactionCount: Int           = 0
  private var commandCounts: Map[String, Int] = Map.empty
  private var startTime: LocalDateTime        = LocalDateTime.now()

  /** Logs a user interaction with the chatbot
    * @param command
    *   The command the user issued
    */
  def logInteraction(command: Command): Unit = {
    interactionCount += 1

    // Extract the command type name
    val commandType = command.getClass.getSimpleName.replace("$", "")

    // Update command frequency
    commandCounts = commandCounts.updated(
      commandType,
      commandCounts.getOrElse(commandType, 0) + 1
    )

    // In a real implementation, you might want to log this to a file or database
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    println(s"[DEBUG] $timestamp: User issued command: $commandType")
  }

  /** Gets statistics about user interactions
    * @return
    *   Map containing usage statistics
    */
  def getStats: Map[String, String] = {
    val runTime = java.time.Duration.between(startTime, LocalDateTime.now())
    val hours   = runTime.toHours
    val minutes = runTime.toMinutesPart

    Map(
      "total_interactions"  -> interactionCount.toString,
      "runtime"             -> f"${hours}h ${minutes}m",
      "most_used_command"   -> getMostUsedCommand,
      "command_frequencies" -> commandCounts.toString
    )
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
}
