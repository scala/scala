import scala.actors.Actor._

object Test {
  val impl: Actor = actor {
    loop {
      react { 
        case 1 => impl ! 2
      }
    }
  }
}
