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

object Test extends Application {
  import A.B._
  val c = new C("OK")
  val i = new c.Inner
  val s: String = i
  Console.println(s)
  Console.println(2: String)
}
