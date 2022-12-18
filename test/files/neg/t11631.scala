
// scalac: -Xsource:3 -Werror -Wunused

import annotation.*

class C {
  def n: Int = 42
  def f(@unused i: Int) = n + 1
  def g(@unused i: Int) = i + 1   // unused unused

  @unused private def x = 42      // unused unused
  @unused private def y = 27
  private def z = 37              // unused
  def p() = println(x)

  def q() = {
    @unused var v = 42            // unused unused
    var w = 27                    // unused
    v += 1      // write it
    println(v)  // read it
  }
}
