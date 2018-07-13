// scalac: -Xsource:2.14
//
object Test {
  def f[A <% Int](a: A) = null
  def g[C, B <: C, A <% B : Numeric](a: A) = null
}
