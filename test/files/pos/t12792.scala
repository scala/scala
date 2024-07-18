
//> using options -Werror -Xlint

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

class AnnL(value: Long) extends ConstantAnnotation
class AnnD(value: Double) extends ConstantAnnotation

object i17446Types {

  final val myInt = 1 << 6

  // toLong
  final val char2Long: 99L = 'c'.toLong
  final val int2Long: 0L = 0.toLong
  final val long2Long: 0L = 0L.toLong
  final val int2LongPropagated: 64L = myInt.toLong

  // toInt
  final val char2Int: 99 = 'c'.toInt
  final val int2Int: 0 = 0.toInt
  final val long2Int: 0 = 0L.toInt
  final val long2IntWrapped: -2147483648 = 2147483648L.toInt
  final val int2IntPropagated: 64 = myInt.toInt

  // toChar
  final val char2Char: 'c' = 'c'.toChar
  final val int2Char: 'c' = 99.toChar
  final val long2Char: 'c' = 99L.toChar
  final val int2CharPropagated: '@' = myInt.toChar

  // chain everything
  final val wow: 1.0 = 1.toChar.toInt.toLong.toFloat.toDouble
}
object i17446 {

  final val myInt = 1 << 6

  // toLong
  final val char2Long = 'c'.toLong
  final val int2Long = 0.toLong
  final val long2Long = 0L.toLong
  final val int2LongPropagated = myInt.toLong

  // toInt
  final val char2Int = 'c'.toInt
  final val int2Int = 0.toInt
  final val long2Int = 0L.toInt
  final val long2IntWrapped = 2147483648L.toInt
  final val int2IntPropagated = myInt.toInt

  // toChar
  final val char2Char = 'c'.toChar
  final val int2Char = 99.toChar
  final val long2Char = 99L.toChar
  final val int2CharPropagated = myInt.toChar

  // chain everything
  final val wow = 1.toChar.toInt.toLong.toFloat.toDouble
}
class i17446 {
  import i17446._
  @Ann(char2Int) def a = 42
  @Ann(int2Int) def b = 42
  @Ann(long2Int) def c = 42
  @Ann(long2IntWrapped) def d = 42
  @Ann(int2IntPropagated) def e = 42
  @Ann(char2Char) def f = 42
  @Ann(int2Char) def g = 42
  @Ann(long2Char) def h = 42
  @Ann(int2CharPropagated) def i = 42

  @AnnL(char2Long) def j = 42
  @AnnL(int2Long) def k = 42
  @AnnL(long2Long) def l = 42
  @AnnL(int2LongPropagated) def m = 42

  @AnnD(wow) def n = 42
}
