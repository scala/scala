package dingus {
  class Foo1() { protected def this(name: String) = this() }
  class Foo2 protected (name: String) { }
  object Ding {
    protected class Foo3(name: String) { }
  }
}

package hungus {
  import dingus._

  object P {
    class Bar1 extends Foo1("abc")
    class Bar2 extends Foo2("abc")
    class Bar3 extends Ding.Foo3("abc")

    val foo1 = new Foo1("abc")
    val foo2 = new Foo2("abc")
    val foo3 = new Ding.Foo3("abc")
  }
}
