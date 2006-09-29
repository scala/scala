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

  for (val i <- List.range(0, 10)) {
    a ! A
  }
}
