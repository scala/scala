abstract class A {
  def f: Int
  def g: Int

  val x: Int
  val y: Int
}

class C extends A {}

object Q extends A { }