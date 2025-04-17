package chatbot.config
import com.typesafe.config.ConfigFactory
import java.io.File

case class AppConfig(
  scraperTimeout: Int,
  serverPort: Int,
  logLevel: String,
  jsonFilePath: String,
  maxScrapeResults: Int
)

object Config {
  def load: AppConfig = {
    val config = ConfigFactory.load().getConfig("chatbot")
    val appConfig = AppConfig(
      scraperTimeout = config.getInt("scraperTimeout"),
      serverPort = config.getInt("serverPort"),
      logLevel = config.getString("logLevel"),
      jsonFilePath = config.getString("jsonFilePath"),
      maxScrapeResults = config.getInt("maxScrapeResults")
    )
    validate(appConfig)
    appConfig
  }

  val default: AppConfig = validate(
    AppConfig(
      scraperTimeout = 10000,
      serverPort = 8080,
      logLevel = "INFO",
      jsonFilePath = "src/main/resources/astronomy.json",
      maxScrapeResults = 5
    )
  )

  private def validate(config: AppConfig): AppConfig = {
    require(config.scraperTimeout > 0, "scraperTimeout must be positive")
    require(config.serverPort > 0 && config.serverPort <= 65535, "serverPort must be 1-65535")
    require(Seq("INFO", "DEBUG", "ERROR").contains(config.logLevel), "logLevel must be INFO, DEBUG, or ERROR")
    require(new File(config.jsonFilePath).exists(), s"JSON file ${config.jsonFilePath} not found")
    require(config.maxScrapeResults > 0, "maxScrapeResults must be positive")
    config
  }
}
