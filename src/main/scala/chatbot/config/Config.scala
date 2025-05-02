package chatbot.config

case class Config(dataContent: String, serverPort: Int)

object Config {
  def load: Config = {
    val serverPort   = 8080
    val resourceName = "astronomy.json"
    try {
      val content = os.read(os.resource / resourceName)
      Config(dataContent = content, serverPort = serverPort)
    } catch {
      case e: Exception =>
        throw new RuntimeException(s"Failed to load $resourceName: ${e.getMessage}")
    }
  }
}
