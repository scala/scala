/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
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

  // classOf dummy -------------------------------------------------

  /** Return the runtime representation of a class type. */
  def classOf[T]: Class = null

  // aliases -------------------------------------------------------

  type byte = scala.Byte
  type short = scala.Short
  type char = scala.Char
  type int = scala.Int
  type long = scala.Long
  type float = scala.Float
  type double = scala.Double
  type boolean = scala.Boolean
  type unit = scala.Unit

  type All = Nothing
  type AllRef = Null

  type String = System.String
  type StringBuilder = compat.StringBuilder
  type Class = System.Type

  type Throwable = System.Exception
  type Exception = System.Exception
  type Error = System.Exception
  type RuntimeException = System.Exception
  type NullPointerException = System.NullReferenceException
  type ClassCastException = System.InvalidCastException
  type IndexOutOfBoundsException = System.IndexOutOfRangeException
  type ArrayIndexOutOfBoundsException = System.IndexOutOfRangeException
  type UnsupportedOperationException = System.InvalidOperationException
  type IllegalArgumentException = System.ArgumentException
  type NoSuchElementException = System.InvalidOperationException
  //type NumberFormatException = java.lang.NumberFormatException

  // --- Miscelleaneous -----------------------------------------------

  //val $scope = scala.xml.TopScope

  type Function[-a,+b] = Function1[a,b]

  type Map[a, b] = collection.immutable.Map[a, b]
  type Set[a] = collection.immutable.Set[a]

  val Map = collection.immutable.Map
  val Set = collection.immutable.Set


  // errors and asserts -------------------------------------------------

  def error(message: String): Nothing = throw new Error(message)

  def exit: Nothing = exit(0)

  def exit(status: Int): Nothing = {
    System.Environment.Exit(status)
    throw new Throwable()
  }

  def assert(assertion: Boolean): Unit = {
    if (!assertion)
      throw new Error("assertion failed")
  }

  def assert(assertion: Boolean, message: Any): Unit = {
    if (!assertion)
      throw new Error("assertion failed: " + message)
  }

  def assume(assumption: Boolean): Unit = {
    if (!assumption)
      throw new Error("assumption failed")
  }

  def assume(assumption: Boolean, message: Any): Unit = {
    if (!assumption)
      throw new Error("assumption failed: " + message)
  }

  // --- Tupling ----------------------------------------------

  type Pair[+a, +b] = Tuple2[a, b]
  object Pair {
    def apply[a, b](x: a, y: b) = Tuple2(x, y)
    def unapply[a, b](x: Tuple2[a, b]): Option[Tuple2[a, b]] = Some(x)
  }

  type Triple[+a, +b, +c] = Tuple3[a, b, c]
  object Triple {
    def apply[a, b, c](x: a, y: b, z: c) = Tuple3(x, y, z)
    def unapply[a, b, c](x: Tuple3[a, b, c]): Option[Tuple3[a, b, c]] = Some(x)
  }

  class ArrowAssoc[a](x: a) {
    def -> [b](y: b): Tuple2[a, b] = Tuple2(x, y)
  }
  implicit def any2ArrowAssoc[a](x: a): ArrowAssoc[a] = new ArrowAssoc(x)

  def Tuple[a1](x1: a1) = Tuple1(x1)
  def Tuple[a1, a2](x1: a1, x2: a2) = Tuple2(x1, x2)
  def Tuple[a1, a2, a3](x1: a1, x2: a2, x3: a3) = Tuple3(x1, x2, x3)
  def Tuple[a1, a2, a3, a4](x1: a1, x2: a2, x3: a3, x4: a4) = Tuple4(x1, x2, x3, x4)
  def Tuple[a1, a2, a3, a4, a5](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5) = Tuple5(x1, x2, x3, x4, x5)
  def Tuple[a1, a2, a3, a4, a5, a6](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5, x6: a6) = Tuple6(x1, x2, x3, x4, x5, x6)
  def Tuple[a1, a2, a3, a4, a5, a6, a7](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5, x6: a6, x7: a7) = Tuple7(x1, x2, x3, x4, x5, x6, x7)
  def Tuple[a1, a2, a3, a4, a5, a6, a7, a8](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5, x6: a6, x7: a7, x8: a8) = Tuple8(x1, x2, x3, x4, x5, x6, x7, x8)
  def Tuple[a1, a2, a3, a4, a5, a6, a7, a8, a9](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5, x6: a6, x7: a7, x8: a8, x9: a9) = Tuple9(x1, x2, x3, x4, x5, x6, x7, x8, x9)

  // views -------------------------------------------------------------

  implicit def identity[a](x: a): a = x

  implicit def byteWrapper(x: byte)     = new runtime.RichByte(x)
  implicit def shortWrapper(x: short)   = new runtime.RichShort(x)
  implicit def intWrapper(x: int)       = new runtime.RichInt(x)
  implicit def charWrapper(c: char)     = new runtime.RichChar(c)
  implicit def longWrapper(x: long)     = new runtime.RichLong(x)
  implicit def floatWrapper(x: float)   = new runtime.RichFloat(x)
  implicit def doubleWrapper(x: double) = new runtime.RichDouble(x)

  implicit def booleanWrapper(x: boolean)  = new runtime.RichBoolean(x)

  implicit def stringWrapper(x: String) = new runtime.RichString(x)

  implicit def any2stringadd(x: Any) = new runtime.StringAdd(x)

  implicit def exceptionWrapper(exc: Throwable) = new runtime.RichException(exc)

  final class GetClassWrapper(obj: AnyRef) {
    def getClass(): runtime.RichClass = classWrapper(obj.GetType())
  }
  implicit def getClassWrapper(obj: AnyRef) = new GetClassWrapper(obj)
  implicit def classWrapper(clazz: Class): runtime.RichClass = new runtime.RichClass(clazz)

  implicit def unit2ordered(x: unit): Ordered[unit] = new Ordered[unit] with Proxy {
    def self: Any = x
    def compare (y: unit): int = 0
  }

  implicit def iterable2ordered[a <% Ordered[a]](xs: Iterable[a]): Ordered[Iterable[a]] =
    new Ordered[Iterable[a]] with Proxy {
      val self = xs
      def compare (that: Iterable[a]) = {
        var res = 0
        val these = xs.elements
        val those = that.elements
        while (res == 0 && these.hasNext)
          res = if (those.hasNext) these.next compare those.next else 1
        res
      }
    }

  implicit def tuple22ordered[a1 <% Ordered[a1], a2 <% Ordered[a2]](x: Tuple2[a1, a2]): Ordered[Tuple2[a1, a2]] =
    new Ordered[Tuple2[a1, a2]] with Proxy {
      val self = x
      def compare (y: Tuple2[a1, a2]): Int = {
        val res = x._1 compare y._1
        if (res == 0) x._2 compare y._2
        else res
      }
    }

  implicit def tuple32ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3]](x: Tuple3[a1, a2, a3]): Ordered[Tuple3[a1, a2, a3]] =
    new Ordered[Tuple3[a1, a2, a3]] with Proxy {
      val self = x
      def compare (y: Tuple3[a1, a2, a3]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple2(x._2, x._3) compare Tuple2(y._2, y._3)
        else res
      }
    }

  implicit def tuple42ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4]](x: Tuple4[a1, a2, a3, a4]): Ordered[Tuple4[a1, a2, a3, a4]] =
    new Ordered[Tuple4[a1, a2, a3, a4]] with Proxy {
      val self = x
      def compare (y: Tuple4[a1, a2, a3, a4]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple3(x._2, x._3, x._4) compare Tuple3(y._2, y._3, y._4)
        else res
      }
    }

  implicit def tuple52ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5]](x: Tuple5[a1, a2, a3, a4, a5]): Ordered[Tuple5[a1, a2, a3, a4, a5]] =
    new Ordered[Tuple5[a1, a2, a3, a4, a5]] with Proxy {
      val self = x
      def compare (y: Tuple5[a1, a2, a3, a4, a5]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple4(x._2, x._3, x._4, x._5) compare Tuple4(y._2, y._3, y._4, y._5)
        else res
      }
    }

  implicit def tuple62ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5], a6 <% Ordered[a6]](x: Tuple6[a1, a2, a3, a4, a5, a6]): Ordered[Tuple6[a1, a2, a3, a4, a5, a6]] =
    new Ordered[Tuple6[a1, a2, a3, a4, a5, a6]] with Proxy {
      val self = x
      def compare (y: Tuple6[a1, a2, a3, a4, a5, a6]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple5(x._2, x._3, x._4, x._5, x._6) compare Tuple5(y._2, y._3, y._4, y._5, y._6)
        else res
      }
    }

  implicit def tuple72ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5], a6 <% Ordered[a6], a7 <% Ordered[a7]](x: Tuple7[a1, a2, a3, a4, a5, a6, a7]): Ordered[Tuple7[a1, a2, a3, a4, a5, a6, a7]] =
    new Ordered[Tuple7[a1, a2, a3, a4, a5, a6, a7]] with Proxy {
      val self = x
      def compare (y: Tuple7[a1, a2, a3, a4, a5, a6, a7]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple6(x._2, x._3, x._4, x._5, x._6, x._7) compare Tuple6(y._2, y._3, y._4, y._5, y._6, y._7)
        else res
      }
    }

  implicit def tuple82ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5], a6 <% Ordered[a6], a7 <% Ordered[a7], a8 <% Ordered[a8]](x: Tuple8[a1, a2, a3, a4, a5, a6, a7, a8]): Ordered[Tuple8[a1, a2, a3, a4, a5, a6, a7, a8]] =
    new Ordered[Tuple8[a1, a2, a3, a4, a5, a6, a7, a8]] with Proxy {
      val self = x
      def compare (y: Tuple8[a1, a2, a3, a4, a5, a6, a7, a8]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple7(x._2, x._3, x._4, x._5, x._6, x._7, x._8) compare Tuple7(y._2, y._3, y._4, y._5, y._6, y._7, y._8)
        else res
      }
    }

  implicit def tuple92ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5], a6 <% Ordered[a6], a7 <% Ordered[a7], a8 <% Ordered[a8], a9 <% Ordered[a9]](x: Tuple9[a1, a2, a3, a4, a5, a6, a7, a8, a9]): Ordered[Tuple9[a1, a2, a3, a4, a5, a6, a7, a8, a9]] =
    new Ordered[Tuple9[a1, a2, a3, a4, a5, a6, a7, a8, a9]] with Proxy {
      val self = x
      def compare (y: Tuple9[a1, a2, a3, a4, a5, a6, a7, a8, a9]): Int = {
        val res = x._1 compare y._1
        if (res == 0) Tuple8(x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9) compare Tuple8(y._2, y._3, y._4, y._5, y._6, y._7, y._8, y._9)
        else res
      }
    }

  implicit def byte2short(x: byte): short = x.toShort
  implicit def byte2int(x: byte): int = x.toInt
  implicit def byte2long(x: byte): long = x.toLong
  implicit def byte2float(x: byte): float = x.toFloat
  implicit def byte2double(x: byte): double = x.toDouble

  implicit def short2int(x: short): int = x.toInt
  implicit def short2long(x: short): long = x.toLong
  implicit def short2float(x: short): float = x.toFloat
  implicit def short2double(x: short): double = x.toDouble

  implicit def char2int(x: char): int = x.toInt
  implicit def char2long(x: char): long = x.toLong
  implicit def char2float(x: char): float = x.toFloat
  implicit def char2double(x: char): double = x.toDouble

  implicit def int2long(x: int): long = x.toLong
  implicit def int2float(x: int): float = x.toFloat
  implicit def int2double(x: int): double = x.toDouble

  implicit def long2float(x: long): float = x.toFloat
  implicit def long2double(x: long): double = x.toDouble

  implicit def float2double(x: float): double = x.toDouble

  def currentThread = System.Threading.Thread.CurrentThread

}
