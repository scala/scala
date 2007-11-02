/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
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
  def classOf[T]: Class = null

  // aliases ------------------------------------------------------------

  type byte    = scala.Byte
  type short   = scala.Short
  type char    = scala.Char
  type int     = scala.Int
  type long    = scala.Long
  type float   = scala.Float
  type double  = scala.Double
  type boolean = scala.Boolean
  type unit    = scala.Unit

  /** @deprecated use <code>Nothing</code> instead */
  @deprecated type All = Nothing
  /** @deprecated use <code>Null</code> instead */
  @deprecated type AllRef = Null
  /** @deprecated use <code>Int</code> instead */
  @deprecated type Integer = java.lang.Integer
  /** @deprecated use <code>Char</code> instead */
  @deprecated type Character = java.lang.Character

  type String        = java.lang.String
  type StringBuilder = compat.StringBuilder
  type Class         = java.lang.Class
  type Runnable      = java.lang.Runnable

  type Throwable = java.lang.Throwable
  type Exception = java.lang.Exception
  type Error     = java.lang.Error

  type RuntimeException               = java.lang.RuntimeException
  type NullPointerException           = java.lang.NullPointerException
  type ClassCastException             = java.lang.ClassCastException
  type IndexOutOfBoundsException      = java.lang.IndexOutOfBoundsException
  type ArrayIndexOutOfBoundsException = java.lang.ArrayIndexOutOfBoundsException
  type UnsupportedOperationException  = java.lang.UnsupportedOperationException
  type IllegalArgumentException       = java.lang.IllegalArgumentException
  type NoSuchElementException         = java.util.NoSuchElementException
  type NumberFormatException          = java.lang.NumberFormatException

  // miscelleaneous -----------------------------------------------------

  val $scope = scala.xml.TopScope

  type Function[-A, +B] = Function1[A, B]

  type Map[A, B] = collection.immutable.Map[A, B]
  type Set[A] = collection.immutable.Set[A]

  val Map = collection.immutable.Map
  val Set = collection.immutable.Set

  // errors and asserts -------------------------------------------------

  def error(message: String): Nothing = throw new Error(message)

  def exit: Nothing = exit(0)

  def exit(status: Int): Nothing = {
    java.lang.System.exit(status)
    throw new Throwable()
  }

  def assert(assertion: Boolean) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }

  def assert(assertion: Boolean, message: Any) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: " + message)
  }

  def assume(assumption: Boolean) {
    if (!assumption)
      throw new IllegalArgumentException("assumption failed")
  }

  def assume(assumption: Boolean, message: Any) {
    if (!assumption)
      throw new IllegalArgumentException("assumption failed: " + message)
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

  class ArrowAssoc[A](x: A) {
    def -> [B](y: B): Tuple2[A, B] = Tuple2(x, y)
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
  def format(text: String, xs: Any*) = Console.format(text, xs)

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

  implicit def stringWrapper(x: String) = new runtime.RichString(x)
  implicit def stringBuilderWrapper(x : StringBuilder) = new runtime.RichStringBuilder(x)

  implicit def any2stringadd(x: Any) = new runtime.StringAdd(x)

  implicit def exceptionWrapper(exc: Throwable) = new runtime.RichException(exc)

  implicit def unit2ordered(x: Unit): Ordered[Unit] = new Ordered[Unit] with Proxy {
    def self: Any = x
    def compare(y: Unit): Int = 0
  }

  implicit def iterable2ordered[A <% Ordered[A]](xs: Iterable[A]): Ordered[Iterable[A]] =
    new Ordered[Iterable[A]] with Proxy {
      val self = xs
      def compare(that: Iterable[A]): Int = {
        var res = 0
        val these = xs.elements
        val those = that.elements
        while (res == 0 && these.hasNext)
          res = if (those.hasNext) these.next compare those.next else 1
        if (res == 0) {
          if (those.hasNext) -1 else 0
        } else
          res
      }
    }

  implicit def tuple22ordered[A1 <% Ordered[A1], A2 <% Ordered[A2]](x: Tuple2[A1, A2]): Ordered[Tuple2[A1, A2]] =
    new Ordered[Tuple2[A1, A2]] with Proxy {
      val self = x
      def compare(y: Tuple2[A1, A2]): Int = {
        val res = x._1 compare y._1
        if (res == 0) x._2 compare y._2
        else res
      }
    }

  implicit def tuple32ordered[A1 <% Ordered[A1], A2 <% Ordered[A2], A3 <% Ordered[A3]](x: Tuple3[A1, A2, A3]): Ordered[Tuple3[A1, A2, A3]] =
    new Ordered[Tuple3[A1, A2, A3]] with Proxy {
      val self = x
      def compare(y: Tuple3[A1, A2, A3]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple2(x._2, x._3) compare Tuple2(y._2, y._3)
        else res
      }
    }

  implicit def tuple42ordered[A1 <% Ordered[A1], A2 <% Ordered[A2], A3 <% Ordered[A3], A4 <% Ordered[A4]](x: Tuple4[A1, A2, A3, A4]): Ordered[Tuple4[A1, A2, A3, A4]] =
    new Ordered[Tuple4[A1, A2, A3, A4]] with Proxy {
      val self = x
      def compare(y: Tuple4[A1, A2, A3, A4]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple3(x._2, x._3, x._4) compare Tuple3(y._2, y._3, y._4)
        else res
      }
    }

  implicit def tuple52ordered[A1 <% Ordered[A1], A2 <% Ordered[A2], A3 <% Ordered[A3], A4 <% Ordered[A4], A5 <% Ordered[A5]](x: Tuple5[A1, A2, A3, A4, A5]): Ordered[Tuple5[A1, A2, A3, A4, A5]] =
    new Ordered[Tuple5[A1, A2, A3, A4, A5]] with Proxy {
      val self = x
      def compare(y: Tuple5[A1, A2, A3, A4, A5]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple4(x._2, x._3, x._4, x._5) compare Tuple4(y._2, y._3, y._4, y._5)
        else res
      }
    }

  implicit def tuple62ordered[A1 <% Ordered[A1], A2 <% Ordered[A2], A3 <% Ordered[A3], A4 <% Ordered[A4], A5 <% Ordered[A5], A6 <% Ordered[A6]](x: Tuple6[A1, A2, A3, A4, A5, A6]): Ordered[Tuple6[A1, A2, A3, A4, A5, A6]] =
    new Ordered[Tuple6[A1, A2, A3, A4, A5, A6]] with Proxy {
      val self = x
      def compare(y: Tuple6[A1, A2, A3, A4, A5, A6]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple5(x._2, x._3, x._4, x._5, x._6) compare Tuple5(y._2, y._3, y._4, y._5, y._6)
        else res
      }
    }

  implicit def tuple72ordered[A1 <% Ordered[A1], A2 <% Ordered[A2], A3 <% Ordered[A3], A4 <% Ordered[A4], A5 <% Ordered[A5], A6 <% Ordered[A6], A7 <% Ordered[A7]](x: Tuple7[A1, A2, A3, A4, A5, A6, A7]): Ordered[Tuple7[A1, A2, A3, A4, A5, A6, A7]] =
    new Ordered[Tuple7[A1, A2, A3, A4, A5, A6, A7]] with Proxy {
      val self = x
      def compare(y: Tuple7[A1, A2, A3, A4, A5, A6, A7]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple6(x._2, x._3, x._4, x._5, x._6, x._7) compare Tuple6(y._2, y._3, y._4, y._5, y._6, y._7)
        else res
      }
    }

  implicit def tuple82ordered[A1 <% Ordered[A1], A2 <% Ordered[A2], A3 <% Ordered[A3], A4 <% Ordered[A4], A5 <% Ordered[A5], A6 <% Ordered[A6], A7 <% Ordered[A7], A8 <% Ordered[A8]](x: Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]): Ordered[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] =
    new Ordered[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] with Proxy {
      val self = x
      def compare(y: Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple7(x._2, x._3, x._4, x._5, x._6, x._7, x._8) compare Tuple7(y._2, y._3, y._4, y._5, y._6, y._7, y._8)
        else res
      }
    }

  implicit def tuple92ordered[A1 <% Ordered[A1], A2 <% Ordered[A2], A3 <% Ordered[A3], A4 <% Ordered[A4], A5 <% Ordered[A5], A6 <% Ordered[A6], A7 <% Ordered[A7], A8 <% Ordered[A8], A9 <% Ordered[A9]](x: Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]): Ordered[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    new Ordered[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] with Proxy {
      val self = x
      def compare(y: Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple8(x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9) compare Tuple8(y._2, y._3, y._4, y._5, y._6, y._7, y._8, y._9)
        else res
      }
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

  implicit def byte2Byte(x: Byte) = new java.lang.Byte(x)
  implicit def short2Short(x: Short) = new java.lang.Short(x)
  implicit def char2Character(x: Char) = new java.lang.Character(x)
  implicit def int2Integer(x: Int) = new java.lang.Integer(x)
  implicit def long2Long(x: Long) = new java.lang.Long(x)
  implicit def float2Float(x: Float) = new java.lang.Float(x)
  implicit def double2Double(x: Double) = new java.lang.Double(x)
  implicit def boolean2Boolean(x: Boolean) = java.lang.Boolean.valueOf(x)

  /** any array projection can be automatically converted into an array */
  implicit def forceArrayProjection[A](x : Array.Projection[A]) : Array[A] = x.force
  /** any random access character seq (including rich string can be converted into a string */
  implicit def forceRandomAccessCharSeq(x : runtime.RichString) : String = x.mkString

  def currentThread = java.lang.Thread.currentThread()

  // runtime types -------------------------------------------------------

  def getType[A](x: A)(implicit rep: runtime.TypeRep[A]): runtime.TypeRep[A] = rep

  implicit def boolRep: runtime.TypeRep[Boolean] = runtime.TypeRep.BooleanRep
  implicit def byteRep: runtime.TypeRep[Byte] = runtime.TypeRep.ByteRep
  implicit def charRep: runtime.TypeRep[Char] = runtime.TypeRep.CharRep
  implicit def shortRep: runtime.TypeRep[Short] = runtime.TypeRep.ShortRep
  implicit def intRep: runtime.TypeRep[Int] = runtime.TypeRep.IntRep
  implicit def longRep: runtime.TypeRep[Long] = runtime.TypeRep.LongRep
  implicit def floatRep: runtime.TypeRep[Float] = runtime.TypeRep.FloatRep
  implicit def doubleRep: runtime.TypeRep[Double] = runtime.TypeRep.DoubleRep

  implicit def unitRep: runtime.TypeRep[Unit] = runtime.TypeRep.UnitRep
  implicit def classRep: runtime.TypeRep[Class] = runtime.TypeRep.ClassRep
  implicit def stringRep: runtime.TypeRep[String] = runtime.TypeRep.StringRep
  //implicit def noneRep: runtime.TypeRep[None.type] = runtime.TypeRep.NoneRep
  //implicit def nilRep: runtime.TypeRep[Nil.type] = runtime.TypeRep.NilRep
  implicit def anyRep: runtime.TypeRep[Any] = runtime.TypeRep.AnyRep

  implicit def listRep[A](implicit elemrep: runtime.TypeRep[A]): runtime.TypeRep[List[A]] =
    runtime.TypeRep.ListRep(elemrep)

  implicit def arrayRep[A](implicit elemrep: runtime.TypeRep[A]): runtime.TypeRep[Array[A]] =
    runtime.TypeRep.ArrayRep(elemrep)

  implicit def tuple2Rep[A1, A2](implicit _1: runtime.TypeRep[A1], _2: runtime.TypeRep[A2]): runtime.TypeRep[Tuple2[A1, A2]] =
    runtime.TypeRep.Tuple2Rep(_1, _2)
  implicit def tuple3Rep[A1, A2, A3](implicit _1: runtime.TypeRep[A1], _2: runtime.TypeRep[A2], _3: runtime.TypeRep[A3]): runtime.TypeRep[Tuple3[A1, A2, A3]] =
    runtime.TypeRep.Tuple3Rep(_1, _2, _3)
  implicit def tuple4Rep[A1, A2, A3, A4](implicit _1: runtime.TypeRep[A1], _2: runtime.TypeRep[A2], _3: runtime.TypeRep[A3], _4: runtime.TypeRep[A4]): runtime.TypeRep[Tuple4[A1, A2, A3, A4]] =
    runtime.TypeRep.Tuple4Rep(_1, _2, _3, _4)
  implicit def tuple5Rep[A1, A2, A3, A4, A5](implicit _1: runtime.TypeRep[A1], _2: runtime.TypeRep[A2], _3: runtime.TypeRep[A3], _4: runtime.TypeRep[A4], _5: runtime.TypeRep[A5]): runtime.TypeRep[Tuple5[A1, A2, A3, A4, A5]] =
    runtime.TypeRep.Tuple5Rep(_1, _2, _3, _4, _5)
  implicit def tuple6Rep[A1, A2, A3, A4, A5, A6](implicit _1: runtime.TypeRep[A1], _2: runtime.TypeRep[A2], _3: runtime.TypeRep[A3], _4: runtime.TypeRep[A4], _5: runtime.TypeRep[A5], _6: runtime.TypeRep[A6]): runtime.TypeRep[Tuple6[A1, A2, A3, A4, A5, A6]] =
    runtime.TypeRep.Tuple6Rep(_1, _2, _3, _4, _5, _6)
  implicit def tuple7Rep[A1, A2, A3, A4, A5, A6, A7](implicit _1: runtime.TypeRep[A1], _2: runtime.TypeRep[A2], _3: runtime.TypeRep[A3], _4: runtime.TypeRep[A4], _5: runtime.TypeRep[A5], _6: runtime.TypeRep[A6], _7: runtime.TypeRep[A7]): runtime.TypeRep[Tuple7[A1, A2, A3, A4, A5, A6, A7]] =
    runtime.TypeRep.Tuple7Rep(_1, _2, _3, _4, _5, _6, _7)
  implicit def tuple8Rep[A1, A2, A3, A4, A5, A6, A7, A8](implicit _1: runtime.TypeRep[A1], _2: runtime.TypeRep[A2], _3: runtime.TypeRep[A3], _4: runtime.TypeRep[A4], _5: runtime.TypeRep[A5], _6: runtime.TypeRep[A6], _7: runtime.TypeRep[A7], _8: runtime.TypeRep[A8]): runtime.TypeRep[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] =
    runtime.TypeRep.Tuple8Rep(_1, _2, _3, _4, _5, _6, _7, _8)
  implicit def tuple9Rep[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit _1: runtime.TypeRep[A1], _2: runtime.TypeRep[A2], _3: runtime.TypeRep[A3], _4: runtime.TypeRep[A4], _5: runtime.TypeRep[A5], _6: runtime.TypeRep[A6], _7: runtime.TypeRep[A7], _8: runtime.TypeRep[A8], _9: runtime.TypeRep[A9]): runtime.TypeRep[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    runtime.TypeRep.Tuple9Rep(_1, _2, _3, _4, _5, _6, _7, _8, _9)

  implicit def func1Rep[A1, B](implicit a1: runtime.TypeRep[A1], b: runtime.TypeRep[B]): runtime.TypeRep[Function1[A1, B]] =
    runtime.TypeRep.Function1Rep(a1, b)
  implicit def func2Rep[A1, A2, B](implicit a1: runtime.TypeRep[A1], a2: runtime.TypeRep[A2], b: runtime.TypeRep[B]): runtime.TypeRep[Function2[A1, A2, B]] =
    runtime.TypeRep.Function2Rep(a1, a2, b)
  implicit def func3Rep[A1, A2, A3, B](implicit a1: runtime.TypeRep[A1], a2: runtime.TypeRep[A2], a3: runtime.TypeRep[A3], b: runtime.TypeRep[B]): runtime.TypeRep[Function3[A1, A2, A3, B]] =
    runtime.TypeRep.Function3Rep(a1, a2, a3, b)
  implicit def func4Rep[A1, A2, A3, A4, B](implicit a1: runtime.TypeRep[A1], a2: runtime.TypeRep[A2], a3: runtime.TypeRep[A3], a4: runtime.TypeRep[A4], b: runtime.TypeRep[B]): runtime.TypeRep[Function4[A1, A2, A3, A4, B]] =
    runtime.TypeRep.Function4Rep(a1, a2, a3, a4, b)
  implicit def func5Rep[A1, A2, A3, A4, A5, B](implicit a1: runtime.TypeRep[A1], a2: runtime.TypeRep[A2], a3: runtime.TypeRep[A3], a4: runtime.TypeRep[A4], a5: runtime.TypeRep[A5], b: runtime.TypeRep[B]): runtime.TypeRep[Function5[A1, A2, A3, A4, A5, B]] =
    runtime.TypeRep.Function5Rep(a1, a2, a3, a4, a5, b)
  implicit def func6Rep[A1, A2, A3, A4, A5, A6, B](implicit a1: runtime.TypeRep[A1], a2: runtime.TypeRep[A2], a3: runtime.TypeRep[A3], a4: runtime.TypeRep[A4], a5: runtime.TypeRep[A5], a6: runtime.TypeRep[A6], b: runtime.TypeRep[B]): runtime.TypeRep[Function6[A1, A2, A3, A4, A5, A6, B]] =
    runtime.TypeRep.Function6Rep(a1, a2, a3, a4, a5, a6, b)
  implicit def func7Rep[A1, A2, A3, A4, A5, A6, A7, B](implicit a1: runtime.TypeRep[A1], a2: runtime.TypeRep[A2], a3: runtime.TypeRep[A3], a4: runtime.TypeRep[A4], a5: runtime.TypeRep[A5], a6: runtime.TypeRep[A6], a7: runtime.TypeRep[A7], b: runtime.TypeRep[B]): runtime.TypeRep[Function7[A1, A2, A3, A4, A5, A6, A7, B]] =
    runtime.TypeRep.Function7Rep(a1, a2, a3, a4, a5, a6, a7, b)
  implicit def func8Rep[A1, A2, A3, A4, A5, A6, A7, A8, B](implicit a1: runtime.TypeRep[A1], a2: runtime.TypeRep[A2], a3: runtime.TypeRep[A3], a4: runtime.TypeRep[A4], a5: runtime.TypeRep[A5], a6: runtime.TypeRep[A6], a7: runtime.TypeRep[A7], a8: runtime.TypeRep[A8], b: runtime.TypeRep[B]): runtime.TypeRep[Function8[A1, A2, A3, A4, A5, A6, A7, A8, B]] =
    runtime.TypeRep.Function8Rep(a1, a2, a3, a4, a5, a6, a7, a8, b)
  implicit def func9Rep[A1, A2, A3, A4, A5, A6, A7, A8, A9, B](implicit a1: runtime.TypeRep[A1], a2: runtime.TypeRep[A2], a3: runtime.TypeRep[A3], a4: runtime.TypeRep[A4], a5: runtime.TypeRep[A5], a6: runtime.TypeRep[A6], a7: runtime.TypeRep[A7], a8: runtime.TypeRep[A8], a9: runtime.TypeRep[A9], b: runtime.TypeRep[B]): runtime.TypeRep[Function9[A1, A2, A3, A4, A5, A6, A7, A8, A9, B]] =
    runtime.TypeRep.Function9Rep(a1, a2, a3, a4, a5, a6, a7, a8, a9, b)
/*
  implicit def objectRep[A <: AnyRef](obj: A)(implicit rep: runtime.TypeRep[A]): runtime.TypeRep[Class] =
    runtime.TypeRep.ObjectRep(obj.getClass)
*/
}
