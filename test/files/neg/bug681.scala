package test;

class foo {
  val x : Int = 42;
}
object foo {
  val x : String = "hello";
}
object test extends foo with Application {
    import foo.{x};
    Console.println(x)
}
