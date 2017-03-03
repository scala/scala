object Actor {
  def receive[A](f: PartialFunction[Any, A]): A = ???
}

object Test {

  def foo() = {
    val x = 1;
    Actor.receive {
      case "abc" if x == 2 =>
        Console.println("hi!")
    }
  }
}
