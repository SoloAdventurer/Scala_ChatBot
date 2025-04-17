package chatbot.util

object Logging {
  def info(msg: String): Unit  = println(s"INFO: $msg")
  def error(msg: String): Unit = println(s"ERROR: $msg")
  // TODO (Member C): Replace println with a proper logging library (e.g., Logback).
  // TODO (Member C): Add log levels (debug, warn).
  // TODO (Member C): Log to file for debugging on Render.
}
