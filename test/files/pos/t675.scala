package test;

trait T {
  abstract class Foo;
  private object FOO_0 extends Foo {
    Console.println("FOO_0 initialized")
  }
  trait X {
    def foo : Foo = FOO_0;
  }  
}

object Test extends App {
  val t = new T{}
  val x = new t.X{}
  Console.println(x.foo)
}
  
  
