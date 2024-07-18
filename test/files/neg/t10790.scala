//> using options -Werror -Wunused

import annotation.unused

class X {
  def f(@unused x: Int) = answer       // no warn

  def control(x: Int) = answer         // warn to verify control

  private class C                  // warn
  @unused private class D          // no warn

  private val Some(y) = Option(answer) // warn
  @unused private val Some(z) = Option(answer) // no warn

  @unused("not updated") private var i = answer       // no warn
  def g = i

  @unused("not read") private var j = answer       // no warn
  def update() = j = 17

  def answer: Int = 42
}
