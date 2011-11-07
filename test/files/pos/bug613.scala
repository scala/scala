class Outer extends Application {
  val y: Int = 1
  abstract class C {
    val x: Int
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
  println(o.foo.x)
}
