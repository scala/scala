//> using options -Xsource:3

import annotation.*

trait T {
  @varargs def d(n: Int*) = 42 // ok
  @varargs val x = 42          // nok
  def f(@varargs y: Int) = 42  // nok
  def g(z: Int @varargs) = 42  // nok
  def h(z: Int) = 42: @varargs // nok

  lazy val VarargsClass = List.empty[varargs]  // good one
}
