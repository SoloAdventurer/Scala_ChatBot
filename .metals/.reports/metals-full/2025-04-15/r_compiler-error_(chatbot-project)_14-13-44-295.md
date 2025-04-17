file:///D:/chatbot-project/src/main/scala/chatbot/server/WebServer.scala
### dotty.tools.dotc.MissingCoreLibraryException: Could not find package scala from compiler core libraries.
Make sure the compiler core libraries are on the classpath.
   

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: file:///D:/chatbot-project/src/main/scala/chatbot/server/WebServer.scala
text:
```scala
package chatbot.server

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.HttpEntity
import scala.io.StdIn
import akka.http.scaladsl.model.ContentTypes

object WebServer {
  def main(args: Array[String]): Unit = {
    implicit val system           = ActorSystem(Behaviors.empty, "ChatbotSystem")
    implicit val executionContext = system.executionContext

    val route =
      path("chat") {
        get {
          parameter("input") { input =>
            complete(
              HttpEntity(ContentTypes.`text/plain(UTF-8)`, s"Received input: $input")
            ) // Placeholder for Responder integration
          }
        }
      } ~
        path("") {
          get {
            complete(
              HttpEntity(
                ContentTypes.`text/plain(UTF-8)`,
                "Welcome to the Astronomy Chatbot! Use /chat?input=your_command"
              )
            )
          }
        }

    val bindingFuture = Http().newServerAt("localhost", 8080).bind(route)
    println(s"Server online at http://localhost:8080/\nPress ENTER to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())(system.executionContext)
      .onComplete(_ => system.terminate())(system.executionContext)
  }
}

```



#### Error stacktrace:

```
dotty.tools.dotc.core.Denotations$.select$1(Denotations.scala:1326)
	dotty.tools.dotc.core.Denotations$.recurSimple$1(Denotations.scala:1354)
	dotty.tools.dotc.core.Denotations$.recur$1(Denotations.scala:1356)
	dotty.tools.dotc.core.Denotations$.staticRef(Denotations.scala:1360)
	dotty.tools.dotc.core.Symbols$.requiredPackage(Symbols.scala:944)
	dotty.tools.dotc.core.Definitions.ScalaPackageVal(Definitions.scala:215)
	dotty.tools.dotc.core.Definitions.ScalaPackageClass(Definitions.scala:218)
	dotty.tools.dotc.core.Definitions.AnyClass(Definitions.scala:281)
	dotty.tools.dotc.core.Definitions.syntheticScalaClasses(Definitions.scala:2184)
	dotty.tools.dotc.core.Definitions.syntheticCoreClasses(Definitions.scala:2199)
	dotty.tools.dotc.core.Definitions.init(Definitions.scala:2215)
	dotty.tools.dotc.core.Contexts$ContextBase.initialize(Contexts.scala:921)
	dotty.tools.dotc.core.Contexts$Context.initialize(Contexts.scala:544)
	dotty.tools.dotc.interactive.InteractiveDriver.<init>(InteractiveDriver.scala:41)
	dotty.tools.pc.CachingDriver.<init>(CachingDriver.scala:30)
	dotty.tools.pc.ScalaPresentationCompiler.$init$$$anonfun$1(ScalaPresentationCompiler.scala:85)
```
#### Short summary: 

dotty.tools.dotc.MissingCoreLibraryException: Could not find package scala from compiler core libraries.
Make sure the compiler core libraries are on the classpath.
   