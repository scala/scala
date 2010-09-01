/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



/**
 * Core Scala types. They are always available without an explicit import.
 */
package object scala {
  type Throwable = java.lang.Throwable
  type Exception = java.lang.Exception
  type Error     = java.lang.Error

  type RuntimeException                = java.lang.RuntimeException
  type NullPointerException            = java.lang.NullPointerException
  type ClassCastException              = java.lang.ClassCastException
  type IndexOutOfBoundsException       = java.lang.IndexOutOfBoundsException
  type ArrayIndexOutOfBoundsException  = java.lang.ArrayIndexOutOfBoundsException
  type StringIndexOutOfBoundsException = java.lang.StringIndexOutOfBoundsException
  type UnsupportedOperationException   = java.lang.UnsupportedOperationException
  type IllegalArgumentException        = java.lang.IllegalArgumentException
  type NoSuchElementException          = java.util.NoSuchElementException
  type NumberFormatException           = java.lang.NumberFormatException
  type AbstractMethodError             = java.lang.AbstractMethodError

  type TraversableOnce[+A] = scala.collection.TraversableOnce[A]

  type Traversable[+A] = scala.collection.Traversable[A]
  val Traversable = scala.collection.Traversable

  type Iterable[+A] = scala.collection.Iterable[A]
  val Iterable = scala.collection.Iterable

  type Seq[+A] = scala.collection.Seq[A]
  val Seq = scala.collection.Seq

  type IndexedSeq[+A] = scala.collection.IndexedSeq[A]
  val IndexedSeq = scala.collection.IndexedSeq

  type Iterator[+A] = scala.collection.Iterator[A]
  val Iterator = scala.collection.Iterator

  type BufferedIterator[+A] = scala.collection.BufferedIterator[A]

  type List[+A] = scala.collection.immutable.List[A]
  val List = scala.collection.immutable.List

  val Nil = scala.collection.immutable.Nil

  type ::[A] = scala.collection.immutable.::[A]
  val :: = scala.collection.immutable.::

  type Stream[+A] = scala.collection.immutable.Stream[A]
  val Stream = scala.collection.immutable.Stream
  val #:: = scala.collection.immutable.Stream.#::

  type Vector[+A] = scala.collection.immutable.Vector[A]
  val Vector = scala.collection.immutable.Vector

  type StringBuilder = scala.collection.mutable.StringBuilder

  type Range = scala.collection.immutable.Range
  val Range = scala.collection.immutable.Range

  // Migrated from Predef

  val $scope = scala.xml.TopScope
  def currentThread = java.lang.Thread.currentThread()

  // Numeric types which were moved into scala.math.*

  type BigDecimal = scala.math.BigDecimal
  val BigDecimal = scala.math.BigDecimal

  type BigInt = scala.math.BigInt
  val BigInt = scala.math.BigInt

  type Equiv[T] = scala.math.Equiv[T]
  type Fractional[T] = scala.math.Fractional[T]
  type Integral[T] = scala.math.Integral[T]

  type Numeric[T] = scala.math.Numeric[T]
  val Numeric = scala.math.Numeric

  type Ordered[T] = scala.math.Ordered[T]
  val Ordered = scala.math.Ordered

  type Ordering[T] = scala.math.Ordering[T]
  val Ordering = scala.math.Ordering

  type PartialOrdering[T] = scala.math.PartialOrdering[T]
  type PartiallyOrdered[T] = scala.math.PartiallyOrdered[T]

  @deprecated("Use Tuple1(x) to create a 1-tuple.")
  def Tuple[A1](x1: A1) = Tuple1(x1)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2](x1: A1, x2: A2) = Tuple2(x1, x2)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3](x1: A1, x2: A2, x3: A3) = Tuple3(x1, x2, x3)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4](x1: A1, x2: A2, x3: A3, x4: A4) = Tuple4(x1, x2, x3, x4)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5) = Tuple5(x1, x2, x3, x4, x5)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6) = Tuple6(x1, x2, x3, x4, x5, x6)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7) = Tuple7(x1, x2, x3, x4, x5, x6, x7)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8) = Tuple8(x1, x2, x3, x4, x5, x6, x7, x8)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9) = Tuple9(x1, x2, x3, x4, x5, x6, x7, x8, x9)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10) = Tuple10(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11) = Tuple11(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12) = Tuple12(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13) = Tuple13(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13, x14: A14) = Tuple14(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13, x14: A14, x15: A15) = Tuple15(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13, x14: A14, x15: A15, x16: A16) = Tuple16(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13, x14: A14, x15: A15, x16: A16, x17: A17) = Tuple17(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13, x14: A14, x15: A15, x16: A16, x17: A17, x18: A18) = Tuple18(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13, x14: A14, x15: A15, x16: A16, x17: A17, x18: A18, x19: A19) = Tuple19(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13, x14: A14, x15: A15, x16: A16, x17: A17, x18: A18, x19: A19, x20: A20) = Tuple20(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13, x14: A14, x15: A15, x16: A16, x17: A17, x18: A18, x19: A19, x20: A20, x21: A21) = Tuple21(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)
  @deprecated("Use ((x1, x2, ...)) syntax to create Tuples")
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9, x10: A10, x11: A11, x12: A12, x13: A13, x14: A14, x15: A15, x16: A16, x17: A17, x18: A18, x19: A19, x20: A20, x21: A21, x22: A22) = Tuple22(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)

  @deprecated("use <code>java.lang.Integer</code> instead")
  type Integer = java.lang.Integer
  @deprecated("use <code>java.lang.Character</code> instead")
  type Character = java.lang.Character

  @deprecated("use Iterable instead") type Collection[+A] = Iterable[A]
  @deprecated("use Iterable instead") val Collection = Iterable

  @deprecated("use Seq instead") type Sequence[+A] = scala.collection.Seq[A]
  @deprecated("use Seq instead") val Sequence = scala.collection.Seq

  @deprecated("use IndexedSeq instead") type RandomAccessSeq[+A] = scala.collection.IndexedSeq[A]
  @deprecated("use IndexedSeq instead") val RandomAccessSeq = scala.collection.IndexedSeq
}
