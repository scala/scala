import scala.actors.Actor

object Test {

  def foo() = {
    val x = 1;
    Actor.receive {
      case "abc" if x == 2 =>
        Console.println("hi!")
    }
  }
}
