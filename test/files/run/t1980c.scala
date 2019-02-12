//scalac: -Xsource:3

import annotation.*

@deprecated("Uses Stream in test", since="warnings are annoying")
object X {
  var i = 0
  def bump: Int = { i += 1; i }
  def f: Int = bump
  def g: Int = bump
  def h: Int = bump
  val xs = f #:: g #:: h #:: Stream.empty
}
@nowarn
object Test extends App {
  import X.*
  assert(1 == i, s"Expected 1, got $i")
  xs.force
  assert(3 == i, s"Expected 3, got $i")
  assert("Stream(1, 2, 3)" == xs.toString(), s"Expected Stream(1, 2, 3), got $xs")
}
