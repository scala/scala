// scalac: -Xfatal-warnings -Ywarn-unused

import annotation.unused

class X {
  def f(@unused x: Int) = 42       // no warn

  def control(x: Int) = 42         // warn to verify control

  private class C                  // warn
  @unused private class D          // no warn

  private val Some(y) = Option(42) // warn
  @unused private val Some(z) = Option(42) // no warn

  @unused("not updated") private var i = 42       // no warn
  def g = i

  @unused("not read") private var j = 42       // no warn
  def update() = j = 17
}
