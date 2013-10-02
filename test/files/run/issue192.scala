import scala.language.reflectiveCalls

object Test extends App {

  def f1(p: Any{def unary_+ : Int}) = +p
  def f2(p: Any{def unary_- : Int}) = -p
  def f3(p: Any{def unary_~ : Int}) = ~p
  def f4(p: Any{def unary_! : Boolean}) = !p

  def f5(p: Any{def +(q: Int): Int}) = p + 7
  def f6(p: Any{def -(q: Int): Int}) = p - 7
  def f7(p: Any{def *(q: Int): Int}) = p * 7
  def f8(p: Any{def /(q: Int): Int}) = p / 7
  def f9(p: Any{def %(q: Int): Int}) = p % 7

  def f10(p: Any{def |(q: Int): Int}) = p | 7
  def f11(p: Any{def |(q: Boolean): Boolean}) = p | true
  def f12(p: Any{def ^(q: Int): Int}) = p ^ 7
  def f13(p: Any{def ^(q: Boolean): Boolean}) = p ^ true
  def f14(p: Any{def &(q: Int): Int}) = p & 7
  def f15(p: Any{def &(q: Boolean): Boolean}) = p & true
  def f16(p: Any{def ||(q: Boolean): Boolean}) = p || true
  def f17(p: Any{def &&(q: Boolean): Boolean}) = p && true

  def f18(p: Any{def <<(q: Int): Int}) = p << 7
  def f19(p: Any{def >>(q: Int): Int}) = p >> 7
  def f20(p: Any{def >>>(q: Int): Int}) = p >>> 7

  def f21(p: Any{def toByte: Byte}) = p.toByte
  def f22(p: Any{def toShort: Short}) = p.toShort
  def f23(p: Any{def toChar: Char}) = p.toChar
  def f24(p: Any{def toInt: Int}) = p.toInt
  def f25(p: Any{def toLong: Long}) = p.toLong
  def f26(p: Any{def toFloat: Float}) = p.toFloat
  def f27(p: Any{def toDouble: Double}) = p.toDouble

  def f28(p: Any{def ==(q: Int): Boolean}) = p == 7
  def f29(p: Any{def !=(q: Int): Boolean}) = p != 7
  def f30(p: Any{def ==(q: Boolean): Boolean}) = p == true
  def f31(p: Any{def !=(q: Boolean): Boolean}) = p != true

  def f32(p: Any{def <(q: Int): Boolean}) = p < 7
  def f33(p: Any{def <=(q: Int): Boolean}) = p <= 7
  def f34(p: Any{def >=(q: Int): Boolean}) = p >= 7
  def f35(p: Any{def >(q: Int): Boolean}) = p > 7

  print("f1 =  "); println(f1(1) == +1)
  print("f2 =  "); println(f2(1) == -1)
  print("f3 =  "); println(f3(1) == ~1)
  print("f4 =  "); println(f4(true) == !true)

  print("f5 =  "); println(f5(4) == (4 + 7))
  print("f6 =  "); println(f6(4) == (4 - 7))
  print("f7 =  "); println(f7(4) == (4 * 7))
  print("f8 =  "); println(f8(4) == (4 / 7))
  print("f9 =  "); println(f9(4) == (4 % 7))

  print("f10 = "); println(f10(4) == (4 | 7))
  print("f11 = "); println(f11(false) == (false | true))
  print("f12 = "); println(f12(4) == (4 ^ 7))
  print("f13 = "); println(f13(false) == (false ^ true))
  print("f14 = "); println(f14(4) == (4 & 7))
  print("f15 = "); println(f15(false) == (false & true))
  print("f16 = "); println(f16(false) == (false || true))
  print("f17 = "); println(f17(false) == (false && true))

  print("f18 = "); println(f18(4) == (4 << 7))
  print("f19 = "); println(f19(-4) == (-4 >> 7))
  print("f20 = "); println(f20(-4) == (-4 >>> 7))

  print("f21 = "); println(f21(4.2) == (4.2.toByte))
  print("f22 = "); println(f22(4.2) == (4.2.toShort))
  print("f23 = "); println(f23(4.2) == (4.2.toChar))
  print("f24 = "); println(f24(4.2) == (4.2.toInt))
  print("f25 = "); println(f25(4.2) == (4.2.toLong))
  print("f26 = "); println(f26(4.2) == (4.2.toFloat))
  print("f27 = "); println(f27(4.2) == (4.2.toDouble))

  print("f28 = "); println(f28(4) == (4 == 7))
  print("f29 = "); println(f29(4) == (4 != 7))
  print("f30 = "); println(f30(false) == (false == true))
  print("f31 = "); println(f31(false) == (false != true))

  print("f32 = "); println(f32(4) == (4 < 7))
  print("f33 = "); println(f33(4) == (4 <= 7))
  print("f34 = "); println(f34(4) == (4 >= 7))
  print("f35 = "); println(f35(4) == (4 > 7))

  println("ok")

}
