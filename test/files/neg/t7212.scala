
//> using options -Xsource:3

trait T { def f: Object }
class K extends T { def f = "" }
object K {
  val k = new K
  val s: String = k.f
}

class F extends T { val f = "" }
object F {
  val f = new F
  val s: String = f.f
}

trait V extends T { var f = "" }
class W extends V
object W {
  val w = new W
  val s: String = w.f
}

object refinement {
  trait X { def f: Int }
  trait T { def f: X }
  // inferred:    RefinedType(List(T, AnyRef), Nil)
  // parent type: TypeRef(T)
  // `=:=` is false, but `<:<` is true in both directions
  class C extends T { def f = new X { def f = 1 } }
}
