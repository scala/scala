object A {
  object B {
    implicit def int2string(x: Int) = "["+x.toString+"]"
  }
}

class C(x: String) {

  class Inner {
  }

  object Inner {
    val s: String = x
    implicit def Inner2String(x: Inner): String = s
  }
}

object Test extends App {
  import A.B._
  val c = new C("OK")
  val i = new c.Inner
  val s: String = i
  Console.println(s)
  Console.println(2: String)
}

object TestPriority {

  class C(x: Int) { def foo: Int = x + 1 }

  class D(x: Int) { def foo: Int = x + 2 }

  class IMPL {
    implicit def Int2C(x: Int): C = new C(x)
  }

  object impl extends IMPL {
    implicit def Int2D(x: Int): D = new D(x)
  }

  import impl._

  val x: C = 2
  val y: D = 2
  assert(x.foo == 3, x.foo)
  assert(y.foo == 4, y.foo)
  assert((2).foo == 4, (2).foo)
}
