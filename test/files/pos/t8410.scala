
object Test extends App {
  @deprecated("","") def f = 42
  @deprecated("","") def z = f
  def g = { @deprecated("","") def _f = f ; _f }                   // warns in 2.11.0-M8
  def x = { @deprecated("","") class X { def x = f } ; new X().x } // warns in 2.11.0-M8
  Console println g
  Console println f  // warns

  @deprecated("","") trait T
  object T extends T { def t = f }
  Console println T.t

  def k = List(0).dropWhile(_ < 1)   // inlining warns doubly
}
