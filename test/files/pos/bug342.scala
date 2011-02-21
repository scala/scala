object Main extends App {

  object Foo extends Enumeration(0, "Bar") {  // 2
    val Bar = Value
  }
  import Foo._;
  Console.println(Bar)
}
