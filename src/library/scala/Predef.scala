/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


/** The <code>Predef</code> object provides definitions that are
 *  accessible in all Scala compilation units without explicit
 *  qualification.
 */
object Predef {

  // aliases -------------------------------------------------------

  type Nothing = All
  type Null = AllRef

  type byte = scala.Byte;
  type short = scala.Short;
  type char = scala.Char;
  type int = scala.Int;
  type long = scala.Long;
  type float = scala.Float;
  type double = scala.Double;
  type boolean = scala.Boolean;
  type unit = scala.Unit;

  type String = java.lang.String;
  type NullPointerException = java.lang.NullPointerException;
  type Throwable = java.lang.Throwable;

  type Pair[+a, +b] = Tuple2[a, b];
  def Pair[a, b](x: a, y: b) = Tuple2(x, y);

  type Triple[+a, +b, +c] = Tuple3[a, b, c];
  def Triple[a, b, c](x: a, y: b, z: c) = Tuple3(x, y, z);

  def Tuple[a1, a2](x1: a1, x2: a2) = Tuple2(x1, x2);
  def Tuple[a1, a2, a3](x1: a1, x2: a2, x3: a3) = Tuple3(x1, x2, x3);
  def Tuple[a1, a2, a3, a4](x1: a1, x2: a2, x3: a3, x4: a4) = Tuple4(x1, x2, x3, x4);
  def Tuple[a1, a2, a3, a4, a5](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5) = Tuple5(x1, x2, x3, x4, x5);
  def Tuple[a1, a2, a3, a4, a5, a6](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5, x6: a6) = Tuple6(x1, x2, x3, x4, x5, x6);
  def Tuple[a1, a2, a3, a4, a5, a6, a7](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5, x6: a6, x7: a7) = Tuple7(x1, x2, x3, x4, x5, x6, x7);
  def Tuple[a1, a2, a3, a4, a5, a6, a7, a8](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5, x6: a6, x7: a7, x8: a8) = Tuple8(x1, x2, x3, x4, x5, x6, x7, x8);
  def Tuple[a1, a2, a3, a4, a5, a6, a7, a8, a9](x1: a1, x2: a2, x3: a3, x4: a4, x5: a5, x6: a6, x7: a7, x8: a8, x9: a9) = Tuple9(x1, x2, x3, x4, x5, x6, x7, x8, x9);

/*
  def id[a](x: a): a = x;
  def fst[a](x: a, y: Any): a = x;
  def scd[a](x: Any, y: a): a = y;
*/
//todo: remove from here!
  val namespace$default = "";
  val $scope = scala.xml.TopScope;

  type Function[-a,+b] = Function1[a,b];

  // arrays -----------------------------------------------------------

  /** Create an array with given elements.
   *
   *  @param xs the elements to put in the array
   *  @return the array containing elements xs.
   */
  def Array[A <: AnyRef](xs: A*): Array[A] = {
    val array = new Array[A](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }

  val Array = scala.Array;

/* The following metod clashes with the previous one, and has therefore been
 * removed. Note that this is a choice between efficiency and generality.
 * The previous factory method is more efficient than the one that has been
 * commented out. Since it is anyway possible to create a polymorphic array
 * using
 *        new Array[T]
 * it was preferred to restrict the definition of the factory method.

   def Array[A](xs: A*): Array[A] = {
    val array = new Array[A](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
*/

  def Array(xs: boolean*): Array[boolean] = {
    val array = new Array[boolean](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(xs: byte*): Array[byte] = {
    val array = new Array[byte](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(xs: short*): Array[short] = {
    val array = new Array[short](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(xs: char*): Array[char] = {
    val array = new Array[char](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(xs: int*): Array[int] = {
    val array = new Array[int](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(xs: long*): Array[long] = {
    val array = new Array[long](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(xs: float*): Array[float] = {
    val array = new Array[float](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(xs: double*): Array[double] = {
    val array = new Array[double](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(xs: unit*): Array[unit] = {
    val array = new Array[unit](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }

  // errors and asserts -------------------------------------------------

  def error(message: String): Nothing = throw new Error(message);

  def exit: Nothing = exit(0);

  def exit(status: Int): Nothing = {
    java.lang.System.exit(status);
    throw new Throwable()
  }

  def assert(assertion: Boolean): Unit = {
    if (!assertion)
      throw new Error("assertion failed");
  }

  def assert(assertion: Boolean, message: Any): Unit = {
    if (!assertion)
      throw new Error("assertion failed: " + message);
  }

  def assume(assumption: Boolean): Unit = {
    if (!assumption)
      throw new Error("assumption failed");
  }

  def assume(assumption: Boolean, message: Any): Unit = {
    if (!assumption)
      throw new Error("assumption failed: " + message);
  }

  // views -------------------------------------------------------------

  implicit def identity[a](x: a): a = x;

  implicit def int2ordered(x: int): Ordered[int] = new Ordered[int] with Proxy {
    def self: Any = x;
    def compareTo [b >: int <% Ordered[b]](y: b): int = y match {
      case y1: int =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  implicit def char2ordered(x: char): Ordered[char] = new Ordered[char] with Proxy {
    def self: Any = x;
    def compareTo [b >: char <% Ordered[b]](y: b): int = y match {
      case y1: char =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  implicit def long2ordered(x: long): Ordered[long] = new Ordered[long] with Proxy {
    def self: Any = x;
    def compareTo [b >: long <% Ordered[b]](y: b): int = y match {
      case y1: long =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  implicit def float2ordered(x: float): Ordered[float] = new Ordered[float] with Proxy {
    def self: Any = x;
    def compareTo [b >: float <% Ordered[b]](y: b): int = y match {
      case y1: float =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  implicit def double2ordered(x: double): Ordered[double] = new Ordered[double] with Proxy {
    def self: Any = x;
    def compareTo [b >: double <% Ordered[b]](y: b): int = y match {
      case y1: double =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  implicit def boolean2ordered(x: boolean): Ordered[boolean] = new Ordered[boolean] with Proxy {
    def self: Any = x;
    def compareTo [b >: boolean <% Ordered[b]](y: b): int = y match {
      case y1: boolean =>
        if (x == y1) 0
        else if (x) 1
        else -1
      case _ => -(y compareTo x)
    }
  }

  implicit def seq2ordered[a <% Ordered[a]](xs: Array[a]): Ordered[Seq[a]] =
    new Ordered[Seq[a]] with Proxy {
      val self = xs
      def compareTo[b >: Seq[a] <% Ordered[b]](that: b): Int = that match {
        case that: Seq[a] =>
          var res = 0
          val these = xs.elements
          val those = that.elements
          while (res == 0 && these.hasNext)
            res = if (those.hasNext) these.next compareTo those.next else 1
          res
        case _ =>
          -(that compareTo xs)
      }
    }

  implicit def tuple22ordered[a1 <% Ordered[a1], a2 <% Ordered[a2]](x: Tuple2[a1, a2]): Ordered[Tuple2[a1, a2]] =
    new Ordered[Tuple2[a1, a2]] with Proxy {
      val self = x
      def compareTo[T >: Tuple2[a1, a2] <% Ordered[T]](y: T): Int = y match {
        case y: Tuple2[a1, a2] =>
          val res = x._1 compareTo y._1
          if (res == 0) x._2 compareTo y._2
          else res
        case _ => -(y compareTo x)
      }
    }

  implicit def tuple32ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3]](x: Tuple3[a1, a2, a3]): Ordered[Tuple3[a1, a2, a3]] =
    new Ordered[Tuple3[a1, a2, a3]] with Proxy {
      val self = x
      def compareTo[T >: Tuple3[a1, a2, a3] <% Ordered[T]](y: T): Int = y match {
        case y: Tuple3[a1, a2, a3] =>
          val res = x._1 compareTo y._1;
          if (res == 0) Tuple2(x._2, x._3) compareTo Tuple2(y._2, y._3)
          else res
        case _ => -(y compareTo x)
      }
    }

  implicit def tuple42ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4]](x: Tuple4[a1, a2, a3, a4]): Ordered[Tuple4[a1, a2, a3, a4]] =
    new Ordered[Tuple4[a1, a2, a3, a4]] with Proxy {
      val self = x
      def compareTo[T >: Tuple4[a1, a2, a3, a4] <% Ordered[T]](y: T): Int = y match {
        case y: Tuple4[a1, a2, a3, a4] =>
          val res = x._1 compareTo y._1;
          if (res == 0) Tuple3(x._2, x._3, x._4) compareTo Tuple3(y._2, y._3, y._4)
          else res
        case _ => -(y compareTo x)
      }
    }

  implicit def tuple52ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5]](x: Tuple5[a1, a2, a3, a4, a5]): Ordered[Tuple5[a1, a2, a3, a4, a5]] =
    new Ordered[Tuple5[a1, a2, a3, a4, a5]] with Proxy {
      val self = x
      def compareTo[T >: Tuple5[a1, a2, a3, a4, a5] <% Ordered[T]](y: T): Int = y match {
        case y: Tuple5[a1, a2, a3, a4, a5] =>
          val res = x._1 compareTo y._1;
          if (res == 0) Tuple4(x._2, x._3, x._4, x._5) compareTo Tuple4(y._2, y._3, y._4, y._5)
          else res
        case _ => -(y compareTo x)
      }
    }

  implicit def tuple62ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5], a6 <% Ordered[a6]](x: Tuple6[a1, a2, a3, a4, a5, a6]): Ordered[Tuple6[a1, a2, a3, a4, a5, a6]] =
    new Ordered[Tuple6[a1, a2, a3, a4, a5, a6]] with Proxy {
      val self = x
      def compareTo[T >: Tuple6[a1, a2, a3, a4, a5, a6] <% Ordered[T]](y: T): Int = y match {
        case y: Tuple6[a1, a2, a3, a4, a5, a6] =>
          val res = x._1 compareTo y._1;
          if (res == 0) Tuple5(x._2, x._3, x._4, x._5, x._6) compareTo Tuple5(y._2, y._3, y._4, y._5, y._6)
          else res
        case _ => -(y compareTo x)
      }
    }

  implicit def tuple72ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5], a6 <% Ordered[a6], a7 <% Ordered[a7]](x: Tuple7[a1, a2, a3, a4, a5, a6, a7]): Ordered[Tuple7[a1, a2, a3, a4, a5, a6, a7]] =
    new Ordered[Tuple7[a1, a2, a3, a4, a5, a6, a7]] with Proxy {
      val self = x
      def compareTo[T >: Tuple7[a1, a2, a3, a4, a5, a6, a7] <% Ordered[T]](y: T): Int = y match {
        case y: Tuple7[a1, a2, a3, a4, a5, a6, a7] =>
          val res = x._1 compareTo y._1;
          if (res == 0) Tuple6(x._2, x._3, x._4, x._5, x._6, x._7) compareTo Tuple6(y._2, y._3, y._4, y._5, y._6, y._7)
          else res
        case _ => -(y compareTo x)
      }
    }

  implicit def tuple82ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5], a6 <% Ordered[a6], a7 <% Ordered[a7], a8 <% Ordered[a8]](x: Tuple8[a1, a2, a3, a4, a5, a6, a7, a8]): Ordered[Tuple8[a1, a2, a3, a4, a5, a6, a7, a8]] =
    new Ordered[Tuple8[a1, a2, a3, a4, a5, a6, a7, a8]] with Proxy {
      val self = x
      def compareTo[T >: Tuple8[a1, a2, a3, a4, a5, a6, a7, a8] <% Ordered[T]](y: T): Int = y match {
        case y: Tuple8[a1, a2, a3, a4, a5, a6, a7, a8] =>
          val res = x._1 compareTo y._1;
          if (res == 0) Tuple7(x._2, x._3, x._4, x._5, x._6, x._7, x._8) compareTo Tuple7(y._2, y._3, y._4, y._5, y._6, y._7, y._8)
          else res
        case _ => -(y compareTo x)
      }
    }

  implicit def tuple92ordered[a1 <% Ordered[a1], a2 <% Ordered[a2], a3 <% Ordered[a3], a4 <% Ordered[a4], a5 <% Ordered[a5], a6 <% Ordered[a6], a7 <% Ordered[a7], a8 <% Ordered[a8], a9 <% Ordered[a9]](x: Tuple9[a1, a2, a3, a4, a5, a6, a7, a8, a9]): Ordered[Tuple9[a1, a2, a3, a4, a5, a6, a7, a8, a9]] =
    new Ordered[Tuple9[a1, a2, a3, a4, a5, a6, a7, a8, a9]] with Proxy {
      val self = x
      def compareTo[T >: Tuple9[a1, a2, a3, a4, a5, a6, a7, a8, a9] <% Ordered[T]](y: T): Int = y match {
        case y: Tuple9[a1, a2, a3, a4, a5, a6, a7, a8, a9] =>
          val res = x._1 compareTo y._1;
          if (res == 0) Tuple8(x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9) compareTo Tuple8(y._2, y._3, y._4, y._5, y._6, y._7, y._8, y._9)
          else res
        case _ => -(y compareTo x)
      }
    }

  implicit def string2ordered(x: String): Ordered[String] = new Ordered[String] with Proxy {
    def self: Any = x;
    def compareTo [b >: String <% Ordered[b]](y: b): int = y match {
      case y1: String => x compareTo y1;
      case _ => -(y compareTo x)
    }
  }

  implicit def string2seq(str: String): Seq[Char] = new Seq[Char] {
    def length = str.length();
    def elements = Iterator.fromString(str);
    def apply(n: Int) = str.charAt(n);
    override def hashCode(): Int = str.hashCode();
    override def equals(y: Any): Boolean = (str == y);
    override protected def stringPrefix: String = "String";
  }

  implicit def byte2short(x: byte): short = x.toShort;
  implicit def byte2int(x: byte): int = x.toInt;
  implicit def byte2long(x: byte): long = x.toLong;
  implicit def byte2float(x: byte): float = x.toFloat;
  implicit def byte2double(x: byte): double = x.toDouble;

  implicit def short2int(x: short): int = x.toInt;
  implicit def short2long(x: short): long = x.toLong;
  implicit def short2float(x: short): float = x.toFloat;
  implicit def short2double(x: short): double = x.toDouble;

  implicit def char2int(x: char): int = x.toInt;
  implicit def char2long(x: char): long = x.toLong;
  implicit def char2float(x: char): float = x.toFloat;
  implicit def char2double(x: char): double = x.toDouble;

  implicit def int2long(x: int): long = x.toLong;
  implicit def int2float(x: int): float = x.toFloat;
  implicit def int2double(x: int): double = x.toDouble;

  implicit def long2float(x: long): float = x.toFloat;
  implicit def long2double(x: long): double = x.toDouble;

  implicit def float2double(x: float): double = x.toDouble;
}
