package chatbot.config

case class Config(dataPath: String, serverPort: Int)

object Config {
  def load: Config = {
    // Use ClassLoader to load from src/main/resources
    val resourcePath = Option(getClass.getClassLoader.getResource("astronomy.json"))
      .map(_.getPath)
      .getOrElse("resources/astronomy.json") // Fallback path
    Config(dataPath = resourcePath, serverPort = 8080)
  }
}
