package examples.actors

object seq extends Application {
  scala.actors.Debug.level = 3
  import scala.actors.Actor._
  val a = actor {
    { react {
        case 'A => println("received 1st message")
      }; ()
    } andThen react {
      case 'A => println("received 2nd message")
    }
  }
  a ! 'A
  a ! 'A
}
