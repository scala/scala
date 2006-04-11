import scala.concurrent.Process

object Test {

  def foo() = {
    val x = 1;
    Process.receive {
      case "abc" if x == 2 =>
        System.out.println("hi!")
    }
  }
}
