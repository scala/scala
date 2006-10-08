package examples.actors

import scala.actors.Actor._

object Loop extends Application {

  case object A

  val a = reactor {
    loop {
      react {
        case A => scala.Console.println("got A")
      }
    }
  }

  for (val i <- 0 until 10) {
    a ! A
  }
}
