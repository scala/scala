object Main extends Application {

//object Foo extends Enumeration {            // 1: OK !
  object Foo extends Enumeration(0, "Bar") {  // 2
    val Bar = Value
  }
  import Foo._;
  Console.println(Bar)
}
