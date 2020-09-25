class C0(private[this] var c: String) {
  private[this] var x: String = _
  c = "good"
  x = c + " boy!"
  override def toString = x
}

class A(x: Int) { assert(x == 1) } // elided
class B(private[this] val x: Int) { assert(x == 1) } // elided
class C(private[this] var x: Int) { } // kept
class D(private[this] var x: Int) { assert(x == 1) } // kept
class E(private[this] var x: Int) { x = 2 } // kept
class F(private[this] var x: Int) { x = 2; assert(x == 2) } // kept
class G(private[this] var x: Int) { x = 2; assert(x == 2); def m() = assert(x == 2, "m" + x); m() } // kept
class H(private val x: Int) { } // kept
class I(private var x: Int) { } // kept

object Test {
  def fs(o: AnyRef) = println(o.getClass.getDeclaredFields.toList)
  def main(args: Array[String]): Unit = {
    println(new C0("bad"))
    List(new A(1), new B(1), new C(1), new D(1), new E(1), new F(1), new G(1), new H(1), new I(1)).foreach(fs)
  }
}
