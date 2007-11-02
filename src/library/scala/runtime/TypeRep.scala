/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $


package scala.runtime

/**
 *  @author Stephane Micheloud
 *  @version 1.0
 */
abstract class TypeRep[A]

object TypeRep {

  case object BooleanRep extends TypeRep[Boolean] {
    override def toString = "Boolean"
  }
  case object ByteRep extends TypeRep[Byte] {
    override def toString = "Byte"
  }
  case object CharRep extends TypeRep[Char] {
    override def toString = "Char"
  }
  case object ShortRep extends TypeRep[Short] {
    override def toString = "Short"
  }
  case object IntRep extends TypeRep[Int] {
    override def toString = "Int"
  }
  case object LongRep extends TypeRep[Long] {
    override def toString = "Long"
  }
  case object FloatRep extends TypeRep[Float] {
    override def toString = "Float"
  }
  case object DoubleRep extends TypeRep[Double] {
    override def toString = "Double"
  }

  case object UnitRep extends TypeRep[Unit] {
    override def toString = "Unit"
  }
  case object ClassRep extends TypeRep[Class] {
    override def toString = "Class"
  }
  case object StringRep extends TypeRep[String] {
    override def toString = "String"
  }
  case object NoneRep extends TypeRep[None.type] {
    override def toString = "None"
  }
  case object NilRep extends TypeRep[Nil.type] {
    override def toString = "Nil"
  }
  case object AnyRep extends TypeRep[Any] {
    override def toString = "Any"
  }

  @serializable
  case class SomeRep[A](elemRep: TypeRep[A]) extends TypeRep[Some[A]] {
    override def toString = "Some[" + elemRep + "]"
  }

  @serializable
  case class ListRep[A](elemRep: TypeRep[A]) extends TypeRep[List[A]] {
    override def toString = "List[" + elemRep + "]"
  }

  @serializable
  case class ArrayRep[A](elemRep: TypeRep[A]) extends TypeRep[Array[A]] {
    override def toString = "Array[" + elemRep + "]"
  }

  @serializable
  case class Tuple2Rep[A1, A2](_1: TypeRep[A1], _2: TypeRep[A2]) extends TypeRep[Tuple2[A1, A2]] {
    override def toString = "Tuple2[" + _1 + ", " + _2 + "]"
  }
  @serializable
  case class Tuple3Rep[A1, A2, A3](_1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3]) extends TypeRep[Tuple3[A1, A2, A3]] {
    override def toString = "Tuple3[" + _1 + ", " + _2 + ", " + _3 + "]"
  }
  @serializable
  case class Tuple4Rep[A1, A2, A3, A4](_1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4]) extends TypeRep[Tuple4[A1, A2, A3, A4]] {
    override def toString = "Tuple4[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + "]"
  }
  @serializable
  case class Tuple5Rep[A1, A2, A3, A4, A5](_1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4], _5: TypeRep[A5]) extends TypeRep[Tuple5[A1, A2, A3, A4, A5]] {
    override def toString = "Tuple5[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + "]"
  }
  @serializable
  case class Tuple6Rep[A1, A2, A3, A4, A5, A6](val _1: TypeRep[A1], val _2: TypeRep[A2], val _3: TypeRep[A3], val _4: TypeRep[A4], val _5: TypeRep[A5], val _6: TypeRep[A6]) extends TypeRep[Tuple6[A1, A2, A3, A4, A5, A6]] {
    override def toString = "Tuple6[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + ", " + _6 + "]"
  }
  @serializable
  case class Tuple7Rep[A1, A2, A3, A4, A5, A6, A7](val _1: TypeRep[A1], val _2: TypeRep[A2], val _3: TypeRep[A3], val _4: TypeRep[A4], val _5: TypeRep[A5], val _6: TypeRep[A6], val _7: TypeRep[A7]) extends TypeRep[Tuple7[A1, A2, A3, A4, A5, A6, A7]] {
    override def toString = "Tuple7[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + ", " + _6 + ", " + _7 + "]"
  }
  @serializable
  case class Tuple8Rep[A1, A2, A3, A4, A5, A6, A7, A8](val _1: TypeRep[A1], val _2: TypeRep[A2], val _3: TypeRep[A3], val _4: TypeRep[A4], val _5: TypeRep[A5], val _6: TypeRep[A6], val _7: TypeRep[A7], val _8: TypeRep[A8]) extends TypeRep[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] {
    override def toString = "Tuple8[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + ", " + _6 + ", " + _7 + ", " + _8 + "]"
  }
  @serializable
  case class Tuple9Rep[A1, A2, A3, A4, A5, A6, A7, A8, A9](val _1: TypeRep[A1], val _2: TypeRep[A2], val _3: TypeRep[A3], val _4: TypeRep[A4], val _5: TypeRep[A5], val _6: TypeRep[A6], val _7: TypeRep[A7], val _8: TypeRep[A8], val _9: TypeRep[A9]) extends TypeRep[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
    override def toString = "Tuple9[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + ", " + _6 + ", " + _7 + ", " + _8 + ", " + _9 + "]"
  }

  @serializable
  case class Function1Rep[A1, B](a1: TypeRep[A1], b: TypeRep[B]) extends TypeRep[Function1[A1, B]] {
    override def toString = "Function1[" + a1 + ", " + b + "]"
  }
  @serializable
  case class Function2Rep[A1, A2, B](a1: TypeRep[A1], a2: TypeRep[A2], b: TypeRep[B]) extends TypeRep[Function2[A1, A2, B]] {
    override def toString = "Function2[" + a1 + ", " + a2 + ", " + b + "]"
  }
  @serializable
  case class Function3Rep[A1, A2, A3, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], b: TypeRep[B]) extends TypeRep[Function3[A1, A2, A3, B]] {
    override def toString = "Function3[" + a1 + ", " + a2 + ", " + a3 + ", " + b + "]"
  }
  @serializable
  case class Function4Rep[A1, A2, A3, A4, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], b: TypeRep[B]) extends TypeRep[Function4[A1, A2, A3, A4, B]] {
    override def toString = "Function4[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + b + "]"
  }
  @serializable
  case class Function5Rep[A1, A2, A3, A4, A5, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], b: TypeRep[B]) extends TypeRep[Function5[A1, A2, A3, A4, A5, B]] {
    override def toString = "Function5[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + b + "]"
  }
  @serializable
  case class Function6Rep[A1, A2, A3, A4, A5, A6, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], b: TypeRep[B]) extends TypeRep[Function6[A1, A2, A3, A4, A5, A6, B]] {
    override def toString = "Function6[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + a6 + ", " + b + "]"
  }
  @serializable
  case class Function7Rep[A1, A2, A3, A4, A5, A6, A7, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], a7: TypeRep[A7], b: TypeRep[B]) extends TypeRep[Function7[A1, A2, A3, A4, A5, A6, A7, B]] {
    override def toString = "Function7[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + a6 + ", " + a7 + ", " + b + "]"
  }
  @serializable
  case class Function8Rep[A1, A2, A3, A4, A5, A6, A7, A8, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], a7: TypeRep[A7], a8: TypeRep[A8], b: TypeRep[B]) extends TypeRep[Function8[A1, A2, A3, A4, A5, A6, A7, A8, B]] {
    override def toString = "Function8[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + a6 + ", " + a7 + ", " + a8 + b + "]"
  }
  @serializable
  case class Function9Rep[A1, A2, A3, A4, A5, A6, A7, A8, A9, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], a7: TypeRep[A7], a8: TypeRep[A8], a9: TypeRep[A9], b: TypeRep[B]) extends TypeRep[Function9[A1, A2, A3, A4, A5, A6, A7, A8, A9, B]] {
    override def toString = "Function9[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + a6 + ", " + a7 + ", " + a8 + ", " + b + "]"
  }
/*
  @serializable
  case class ObjectRep[A](c: Class) extends TypeRep[A] {
    override def toString = c.getName
  }
*/
}
