package tastytest

object TestGreeting extends Suite("TestGreeting") {

  trait Hello extends Greeting {
    final val greeting = "Hello, World!"
  }

  test(assert(new Greeter with Hello().accessGreeting === "Hello, World!"))

}
