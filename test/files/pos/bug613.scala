class Outer extends Application {
  val y: int = 1
  abstract class C {
    val x: int
  }
  val foo = new C {
    class I {
      val z = y
    }
    val x = (new I).z
  }
}

object Test extends Application {
  val o = new Outer
  Console.println(o.foo.x)
}
