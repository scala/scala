
//scalac: -Xlint -Werror

import annotation.nowarn

trait Test {

  trait T[X]

  class C {
    def -:(x: AnyRef): T[x.type] = ???
    def +:(x: AnyRef)(i: Int): T[x.type] = ???
  }

  val c = new C
  val x: AnyRef = ???

  def ok: T[x.type] = c.-:(x)
  def no: T[x.type] = x -: c

  def ok2: (Int => T[x.type]) = c.+:(x) _
  def no2: (Int => T[x.type]) = (x +: c) _
  def no3: (Int => T[x.type]) = (x +: c)(_)
}

class A {
  def /: (z: A)(op: Int): A = ???
  def self = this
  def bar(x: A, y: A) = (x.self /: y.self)(1)
}

class B {
  @nowarn
  def f(xs: List[Int]) = (0 /: xs) _
  def g = f(List(1,2,3,4))
  def test = g(_ + _)
}

// issue 11117
class A2[B2](val b: B2) { def c: List[b.type] = b :: Nil }

// when extracting user arg to temp val, must include adapted application
class EmptyApplication {
  def java() = 42
  @nowarn("cat=deprecation")
  def test = java :: Nil
}
