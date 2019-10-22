package tastytest

object TestGreeting {

  trait Hello extends Greeting {
    final val greeting = "Hello, World!"
  }

  def test1 = assert(new Greeter with Hello().accessGreeting === "Hello, World!")

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}
