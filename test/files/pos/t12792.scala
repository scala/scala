
// scalac: -Werror -Xlint

import annotation._

object Foo {
  final val w = 1 << (java.lang.Integer.SIZE - 1)
  final val x = 1 << (java.lang.Integer.SIZE - 2)
  final val y = 1 << (java.lang.Integer.SIZE - 3)
  final val z = 1 << (java.lang.Integer.SIZE - 4)
  final val c = 0xffffffff & ~w & ~x & ~y & ~z

  final val i = +42   // 42.unary_+
  final val j = -27   // literal -42
}

class Ann(value: Int) extends ConstantAnnotation
class Byt(value: Byte) extends ConstantAnnotation

class Test {
  import Foo._
  @Ann(w) def fw = 42
  @Ann(x) def fx = 42
  @Ann(c) def fc = 42
  @Ann(i) def fi = 42
  @Ann(j) def fj = 42
  @Byt(42) def byteMe = 42
}
