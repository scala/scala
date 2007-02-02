package examples.actors

import scala.actors.Actor._

object seq extends Application {

  case object A

  val a = actor {
    {
      react {
        case A => Console.println("received A")
      }
      ()
    } andThen {
      react {
        case A => Console.println("received 2nd A")
      }
    }
  }
  a ! A
  a ! A
}
