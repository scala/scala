object Test {
  class MyClass extends scala.util.logging.Logged { }
  val x = new MyClass with scala.util.logging.ConsoleLogger
}
