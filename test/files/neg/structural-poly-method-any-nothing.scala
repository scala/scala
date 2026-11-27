class C { def f[X](x: X): X = x; def h(x: Int): Int = x }
class D { def g: Nothing = throw new RuntimeException("g"); def k[X]: Nothing = g }

object Test {
  val s1: { def f: Any } = new C                // error: a polymorphic method is not a parameterless one
  val s2: { def g[X](x: X): X } = new D         // error: nor the other way around
  val s3: { def h[X]: Any } = new C             // error
  val s4: { def k(x: Int): Int } = new D        // error

  val ok1: { def g: Any } = new D
  val ok2: { def f[X](x: X): Any } = new C
  val ok3: { def k[X]: Any } = new D
  val ok4: { def h(x: Int): Any } = new C
}
