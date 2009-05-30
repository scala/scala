/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


/** The <code>Predef</code> object provides definitions that are
 *  accessible in all Scala compilation units without explicit
 *  qualification.
 */
object Predef {

  // classOf dummy ------------------------------------------------------

  /** Return the runtime representation of a class type. */
  def classOf[T]: Class[T] = null

  // aliases ------------------------------------------------------------

  @deprecated("lower-case type aliases will be removed") type byte    = scala.Byte
  @deprecated("lower-case type aliases will be removed") type short   = scala.Short
  @deprecated("lower-case type aliases will be removed") type char    = scala.Char
  @deprecated("lower-case type aliases will be removed") type int     = scala.Int
  @deprecated("lower-case type aliases will be removed") type long    = scala.Long
  @deprecated("lower-case type aliases will be removed") type float   = scala.Float
  @deprecated("lower-case type aliases will be removed") type double  = scala.Double
  @deprecated("lower-case type aliases will be removed") type boolean = scala.Boolean
  @deprecated("lower-case type aliases will be removed") type unit    = scala.Unit

  @deprecated("use <code>java.lang.Integer</code> instead")
  type Integer = java.lang.Integer
  @deprecated("use <code>java.lang.Character</code> instead")
  type Character = java.lang.Character

  type String        = java.lang.String
  type Class[T]      = java.lang.Class[T]
  type Runnable      = java.lang.Runnable

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

  // miscelleaneous -----------------------------------------------------

  private val P = scala.`package`  // to force scala package object to be seen.
  private val L = scala.collection.immutable.List // to force Nil, :: to be seen.
  private val S = scala.collection.mutable.StringBuilder // to force StringBuilder to be seen.

  val $scope = scala.xml.TopScope

  type Function[-A, +B] = Function1[A, B]

  type Map[A, +B] = collection.immutable.Map[A, B]
  type Set[A] = collection.immutable.Set[A]

  val Map = collection.immutable.Map
  val Set = collection.immutable.Set

  // errors and asserts -------------------------------------------------

  def error(message: String): Nothing = throw new RuntimeException(message)

  def exit(): Nothing = exit(0)

  def exit(status: Int): Nothing = {
    java.lang.System.exit(status)
    throw new Throwable()
  }

  def assert(assertion: Boolean) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }

  def assert(assertion: Boolean, message: => Any) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: "+ message)
  }

  def assume(assumption: Boolean) {
    if (!assumption)
      throw new java.lang.AssertionError("assumption failed")
  }

  def assume(assumption: Boolean, message: => Any) {
    if (!assumption)
      throw new java.lang.AssertionError("assumption failed: "+ message)
  }

  def require(requirement: Boolean) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed")
  }

  def require(requirement: Boolean, message: => Any) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed: "+ message)
  }

  // tupling ------------------------------------------------------------

  type Pair[+A, +B] = Tuple2[A, B]
  object Pair {
    def apply[A, B](x: A, y: B) = Tuple2(x, y)
    def unapply[A, B](x: Tuple2[A, B]): Option[Tuple2[A, B]] = Some(x)
  }

  type Triple[+A, +B, +C] = Tuple3[A, B, C]
  object Triple {
    def apply[A, B, C](x: A, y: B, z: C) = Tuple3(x, y, z)
    def unapply[A, B, C](x: Tuple3[A, B, C]): Option[Tuple3[A, B, C]] = Some(x)
  }

  class Ensuring[A](x: A) {
    def ensuring(cond: Boolean): A = { assert(cond); x }
    def ensuring(cond: Boolean, msg: Any): A = { assert(cond, msg); x }
    def ensuring(cond: A => Boolean): A = { assert(cond(x)); x }
    def ensuring(cond: A => Boolean, msg: Any): A = { assert(cond(x), msg); x }
  }
  implicit def any2Ensuring[A](x: A): Ensuring[A] = new Ensuring(x)

  class ArrowAssoc[A](x: A) {
    def -> [B](y: B): Tuple2[A, B] = Tuple2(x, y)
    def →[B](y: B): Tuple2[A, B] = ->(y)
  }
  implicit def any2ArrowAssoc[A](x: A): ArrowAssoc[A] = new ArrowAssoc(x)

  def Tuple[A1](x1: A1) = Tuple1(x1)
  def Tuple[A1, A2](x1: A1, x2: A2) = Tuple2(x1, x2)
  def Tuple[A1, A2, A3](x1: A1, x2: A2, x3: A3) = Tuple3(x1, x2, x3)
  def Tuple[A1, A2, A3, A4](x1: A1, x2: A2, x3: A3, x4: A4) = Tuple4(x1, x2, x3, x4)
  def Tuple[A1, A2, A3, A4, A5](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5) = Tuple5(x1, x2, x3, x4, x5)
  def Tuple[A1, A2, A3, A4, A5, A6](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6) = Tuple6(x1, x2, x3, x4, x5, x6)
  def Tuple[A1, A2, A3, A4, A5, A6, A7](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7) = Tuple7(x1, x2, x3, x4, x5, x6, x7)
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8) = Tuple8(x1, x2, x3, x4, x5, x6, x7, x8)
  def Tuple[A1, A2, A3, A4, A5, A6, A7, A8, A9](x1: A1, x2: A2, x3: A3, x4: A4, x5: A5, x6: A6, x7: A7, x8: A8, x9: A9) = Tuple9(x1, x2, x3, x4, x5, x6, x7, x8, x9)

  // printing and reading -----------------------------------------------

  def print(x: Any) = Console.print(x)
  def println() = Console.println()
  def println(x: Any) = Console.println(x)
  def printf(text: String, xs: Any*) = Console.printf(text, xs: _*)
  def format(text: String, xs: Any*) = stringWrapper(text).format(xs: _*)

  def readLine(): String = Console.readLine()
  def readLine(text: String, args: Any*) = Console.readLine(text, args)
  def readBoolean() = Console.readBoolean()
  def readByte() = Console.readByte()
  def readShort() = Console.readShort()
  def readChar() = Console.readChar()
  def readInt() = Console.readInt()
  def readLong() = Console.readLong()
  def readFloat() = Console.readFloat()
  def readDouble() = Console.readDouble()
  def readf(format: String) = Console.readf(format)
  def readf1(format: String) = Console.readf1(format)
  def readf2(format: String) = Console.readf2(format)
  def readf3(format: String) = Console.readf3(format)

  // views --------------------------------------------------------------

  implicit def identity[A](x: A): A = x

  implicit def byteWrapper(x: Byte)     = new runtime.RichByte(x)
  implicit def shortWrapper(x: Short)   = new runtime.RichShort(x)
  implicit def intWrapper(x: Int)       = new runtime.RichInt(x)
  implicit def charWrapper(c: Char)     = new runtime.RichChar(c)
  implicit def longWrapper(x: Long)     = new runtime.RichLong(x)
  implicit def floatWrapper(x: Float)   = new runtime.RichFloat(x)
  implicit def doubleWrapper(x: Double) = new runtime.RichDouble(x)

  implicit def booleanWrapper(x: Boolean)  = new runtime.RichBoolean(x)
  implicit def unitWrapper(x: Boolean)  = new runtime.RichUnit

  implicit def stringWrapper(x: String) = new runtime.RichString(x)

  implicit def any2stringadd(x: Any) = new runtime.StringAdd(x)

  implicit def exceptionWrapper(exc: Throwable) = new runtime.RichException(exc)

  /** Lens from Ordering[T] to Ordered[T] */
  implicit def orderingToOrdered[T](x: T)(implicit ord: Ordering[T]): Ordered[T] =
    new Ordered[T] { def compare(that: T): Int = ord.compare(x, that) }

  /** Temporarily leaving this conversion to Ordered - see Ordering.scala for reasoning */
  implicit def unit2ordered(x: Unit): Ordered[Unit] = new Ordered[Unit] with Proxy {
    def self: Any = x
    def compare(y: Unit): Int = 0
  }

  implicit def byte2short(x: Byte): Short = x.toShort
  implicit def byte2int(x: Byte): Int = x.toInt
  implicit def byte2long(x: Byte): Long = x.toLong
  implicit def byte2float(x: Byte): Float = x.toFloat
  implicit def byte2double(x: Byte): Double = x.toDouble

  implicit def short2int(x: Short): Int = x.toInt
  implicit def short2long(x: Short): Long = x.toLong
  implicit def short2float(x: Short): Float = x.toFloat
  implicit def short2double(x: Short): Double = x.toDouble

  implicit def char2int(x: Char): Int = x.toInt
  implicit def char2long(x: Char): Long = x.toLong
  implicit def char2float(x: Char): Float = x.toFloat
  implicit def char2double(x: Char): Double = x.toDouble

  implicit def int2long(x: Int): Long = x.toLong
  implicit def int2float(x: Int): Float = x.toFloat
  implicit def int2double(x: Int): Double = x.toDouble

  implicit def long2float(x: Long): Float = x.toFloat
  implicit def long2double(x: Long): Double = x.toDouble

  implicit def float2double(x: Float): Double = x.toDouble

  implicit def byte2Byte(x: Byte)           = java.lang.Byte.valueOf(x)
  implicit def short2Short(x: Short)        = java.lang.Short.valueOf(x)
  implicit def char2Character(x: Char)      = java.lang.Character.valueOf(x)
  implicit def int2Integer(x: Int)          = java.lang.Integer.valueOf(x)
  implicit def long2Long(x: Long)           = java.lang.Long.valueOf(x)
  implicit def float2Float(x: Float)        = java.lang.Float.valueOf(x)
  implicit def double2Double(x: Double)     = java.lang.Double.valueOf(x)
  implicit def boolean2Boolean(x: Boolean)  = java.lang.Boolean.valueOf(x)

  /** any array projection can be automatically converted into an array */
  //implicit def forceArrayProjection[A](x: Array.Projection[A]): Array[A] = x.force !!! re-enable?
  /** any random access character seq (including rich string can be converted into a string */
  implicit def richString2String(x: runtime.RichString): String = if (x eq null) null else x.toString
  //implicit def lazyStreamToConsable[A](xs: => Stream[A]) = new runtime.StreamCons(xs)

  implicit def seqToCharSequence(xs: collection.Vector[Char]): CharSequence = new CharSequence {
    def length: Int = xs.length
    def charAt(index: Int): Char = xs(index)
    def subSequence(start: Int, end: Int): CharSequence = seqToCharSequence(xs.slice(start, end))
    override def toString: String = xs.mkString("")
  }

  def currentThread = java.lang.Thread.currentThread()
}
