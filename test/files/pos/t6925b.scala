// code *generated* by test/scaladoc/run/SI-5933.scala
// duplicated here because it's related to SI-6925

import language.higherKinds

abstract class Base[M[_, _]] {
  def foo[A, B]: M[(A, B), Any]
}

class Derived extends Base[PartialFunction] {
  def foo[AA, BB] /*: PartialFunction[(A, B) => Any]*/ = { case (a, b) => (a: AA, b: BB) }
}

object Test {
  lazy val lx = { println("hello"); 3 }
  def test1(x: Int = lx) = ???
  def test2(x: Int = lx match { case 0 => 1; case 3 => 4 }) = ???
}