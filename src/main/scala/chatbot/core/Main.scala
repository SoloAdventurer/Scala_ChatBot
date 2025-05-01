package chatbot.main

import chatbot.parser.InputParser
import chatbot.parser.AST.Command
import chatbot.responder.Responder
import chatbot.data.{AstronomyData, PlanetApiClient}
import chatbot.analytics.Analytics
import chatbot.config.Config
import scala.io.StdIn

object Main {
  // ANSI color codes
  private val Cyan   = "\u001b[36m"
  private val Yellow = "\u001b[33m"
  private val Reset  = "\u001b[0m"
  private val Blink  = "\u001b[5m" // Optional blinking effect

  def greetUser(name: String): String = {
    s"Hello, $name! I'm CHATURN, your astronomy chatbot. Ask me about planets, stars, or try 'help' for commands!"
  }

  def main(args: Array[String]): Unit = {
    val config     = Config.load
    val dataSource = new AstronomyData(config)
    val apiClient  = new PlanetApiClient()
    val analytics  = new Analytics()
    val responder  = new Responder(dataSource, apiClient, analytics)
    val parser     = new InputParser()

    // Print ASCII art with colors
    println(s"""
               |${Cyan}=======================================${Reset}
               |${Yellow}${Blink}    CHATURN Astronomy Chatbot${Reset}
               |${Cyan}=======================================${Reset}
               |${Yellow}Team:${Reset} Chaturn
               |${Yellow}Members:${Reset} Mohamed, Dania, Maroska, Jana
               |${Cyan}
               |                                                                    ..;===+.
               |                                                                .:=iiiiii=+=
               |                                                             .=i))=;::+)i=+,
               |                                                          ,=i);)I)))I):=i=;
               |                                                       .=i==))))ii)))I:i++
               |                                                     +)+))iiiiiiii))I=i+:''
               |                                .,:;;++++++;:,.       )iii+:::;iii))+i='
               |                             .:;++=iiiiiiiiii=++;.    =::,,,:::=i));=+''
               |                           ,;+==ii)))))))))))ii==+;,      ,,,:=i))+=:
               |                         ,;+=ii))))))IIIIII))))ii===;.    ,,:=i)=i+
               |                        ;+=ii)))IIIIITIIIIII))))iiii=+,   ,:=));=,
               |                      ,+=i))IIIIIITTTTTITIIIIII)))I)i=+,,:+i)=i+
               |                     ,+i))IIIIIITTTTTTTTTTTTI))IIII))i=::i))i='
               |                    ,=i))IIIIITLLTTTTTTTTTTIITTTTIII)+;+i)+i`
               |                    =i))IIITTLTLTTTTTTTTTIITTLLTTTII+:i)ii:''
               |                   +i))IITTTLLLTTTTTTTTTTTTLLLTTTT+:i)))=,
               |                   =))ITTTTTTTTTTTLTTTTTTLLLLLLTi:=)IIiii;
               |                  .i)IIITTTTTTTTLTTTITLLLLLLLT);=)I)))))i;
               |                  :))IIITTTTTLTTTTTTLLHLLLLL);=)II)IIIIi=:
               |                  :i)IIITTTTTTTTTLLLHLLHLL)+=)II)ITTTI)i=
               |                  .i)IIITTTTITTLLLHHLLLL);=)II)ITTTTII)i+
               |                  =i)IIIIIITTLLLLLLHLL=:i)II)TTTTTTIII)i''
               |                +i)i)))IITTLLLLLLLLT=:i)II)TTTTLTTIII)i;
               |              +ii)i:)IITTLLTLLLLT=;+i)I)ITTTTLTTTII))i;
               |             =;)i=:,=)ITTTTLTTI=:i))I)TTTLLLTTTTTII)i;
               |           +i)ii::,  +)IIITI+:+i)I))TTTTLLTTTTTII))=,
               |         :=;)i=:,,    ,i++::i))I)ITTTTTTTTTTIIII)=+''
               |       .+ii)i=::,,   ,,::=i)))iIITTTTTTTTIIIII)=+
               |      ,==)ii=;:,,,,:::=ii)i)iIIIITIIITIIII))i+:'
               |     +=:))i==;:::;=iii)+)=  `:i)))IIIII)ii+'
               |   .+=:))iiiiiiii)))+ii;
               |  .+=;))iiiiii)));ii+
               | .+=i:)))))))=+ii+
               |.+==i+::::=)i=;
               |,+==iiiiii+,
               |`+=+++;`
               |${Reset}
               |${greetUser("Space Explorer")}
    """.stripMargin)

    // CLI REPL loop
    var running = true
    while (running) {
      print("Enter your query (or 'exit' to quit): ")
      val input = StdIn.readLine()
      if (input == null || input.trim.toLowerCase == "exit") {
        running = false
        println("Goodbye! Come back to explore the cosmos!")
      } else if (input.trim.isEmpty) {
        println("Please type something!")
      } else {
        parser.parseInput(input) match {
          case Right(command) =>
            analytics.logInteraction(command)
            val response = responder.respond(command).getOrElse("message", "Error!")
            println(s"${Yellow}Response:${Reset} $response")
          case Left(error) =>
            println(s"${Yellow}Error:${Reset} $error")
        }
      }
    }
  }
}
