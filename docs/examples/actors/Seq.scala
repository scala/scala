package examples.actors

import scala.actors.Actor._

object Seq extends Application {

  case object A

  val a = actor {
    {
      react {
        case A => scala.Console.println("got A")
      }
      ()
    } andThen {
      react {
        case A => scala.Console.println("2nd reactor got A")
      }
    }
  }
  a ! A
  a ! A
}
