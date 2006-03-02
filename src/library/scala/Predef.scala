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

  type Pair[+p, +q] = Tuple2[p, q];
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

  def id[a](x: a): a = x;
  def fst[a](x: a, y: Any): a = x;
  def scd[a](x: Any, y: a): a = y;

  val namespace$default = "";
  val $scope = scala.xml.TopScope;

  type Function[-a,+b] = Function1[a,b];

  // arrays -----------------------------------------------------------

  /** Create an array with given elements.
   *
   *  @param xs the elements to put in the array
   *  @return the array containing elements xs.
   */
/*
   def Array[A](xs: A*): Array[A] = {
    val array = new Array[A](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
*/
  def Array[A <: AnyRef](xs: A*): Array[A] = {
    val array = new Array[A](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
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

  def error(message: String): All = throw new Error(message);

  def exit: All = exit(0);

  def exit(status: Int): All = {
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
  def view(x: int): Ordered[int] = int2ordered(x);

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
  def view(x: char): Ordered[char] = char2ordered(x);

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
  def view(x: long): Ordered[long] = long2ordered(x);

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
  def view(x: float): Ordered[float] = float2ordered(x);

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
  def view(x: double): Ordered[double] = double2ordered(x);

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
  def view(x: boolean): Ordered[boolean] = boolean2ordered(x);

  implicit def array2ordered[A <% Ordered[A]](xs: Array[A]): Ordered[Array[A]] = new Ordered[Array[A]] with Proxy {
    def self: Any = xs;
    def compareTo[B >: Array[A] <% Ordered[B]](that: B): Int = that match {
      case ys: Array[A] =>
        var i, res = 0;
        while ((i < xs.length) && (i < ys.length) && (res == 0)) {
            res = xs(i) compareTo ys(i);
            i = i + 1;
        }
        if (res != 0) res
        else if (i < xs.length) 1
        else if (i < ys.length) -1
        else 0
      case _ =>
        -(that compareTo xs)
    }
  }
  def view[A <% Ordered[A]](xs: Array[A]): Ordered[Array[A]] = array2ordered(xs);

  private def first(xs: Int*): Int = xs.elements.find(x => x != 0) match {
    case Some(r) => r
    case _ => 0
  }

  /* We can't bootstrap currently with the following views included. We have to
   * wait for the next release...
   *
  implicit def view[A <% Ordered[A], B <% Ordered[B]](x: Tuple2[A, B]): Ordered[Tuple2[A, B]] =
        new Ordered[Tuple2[A, B]] with Proxy(x) {
          def compareTo[T >: Tuple2[A, B] <% Ordered[T]](y: T): Int = y match {
            case y1: Tuple2[A, B] => first(x._1.compareTo(y1._1),
                                           x._2.compareTo(y1._2));
            case _ => -(y compareTo x)
          }
        }

  implicit def view[A <% Ordered[A], B <% Ordered[B], C <% Ordered[C]]
        (x: Tuple3[A, B, C]): Ordered[Tuple3[A, B, C]] =
        new Ordered[Tuple3[A, B, C]] with Proxy(x) {
          def compareTo[T >: Tuple3[A, B, C] <% Ordered[T]](y: T): Int = y match {
            case y1: Tuple3[A, B, C] => first(x._1.compareTo(y1._1),
                                              x._2.compareTo(y1._2),
                                              x._3.compareTo(y1._3));
            case _ => -(y compareTo x)
          }
        }

  implicit def view[A <% Ordered[A], B <% Ordered[B], C <% Ordered[C], D <% Ordered[D]]
        (x: Tuple4[A, B, C, D]): Ordered[Tuple4[A, B, C, D]] =
        new Ordered[Tuple4[A, B, C, D]] with Proxy(x) {
          def compareTo[T >: Tuple4[A, B, C, D] <% Ordered[T]](y: T): Int = y match {
            case y1: Tuple4[A, B, C, D] => first(x._1.compareTo(y1._1),
                                                 x._2.compareTo(y1._2),
                                                 x._3.compareTo(y1._3),
                                                 x._4.compareTo(y1._4));
            case _ => -(y compareTo x)
          }
        }

  implicit def view[A <% Ordered[A], B <% Ordered[B], C <% Ordered[C], D <% Ordered[D], E <% Ordered[E]]
        (x: Tuple5[A, B, C, D, E]): Ordered[Tuple5[A, B, C, D, E]] =
        new Ordered[Tuple5[A, B, C, D, E]] with Proxy(x) {
          def compareTo[T >: Tuple5[A, B, C, D, E] <% Ordered[T]](y: T): Int = y match {
            case y1: Tuple5[A, B, C, D, E] => first(x._1.compareTo(y1._1),
                                                    x._2.compareTo(y1._2),
                                                    x._3.compareTo(y1._3),
                                                    x._4.compareTo(y1._4),
                                                    x._5.compareTo(y1._5));
            case _ => -(y compareTo x)
          }
        }
  */

  implicit def string2ordered(x: String): Ordered[String] = new Ordered[String] with Proxy {
    def self: Any = x;
    def compareTo [b >: String <% Ordered[b]](y: b): int = y match {
      case y1: String => x compareTo y1;
      case _ => -(y compareTo x)
    }
  }
  def view(x: String): Ordered[String] = string2ordered(x);

  implicit def array2seq[A](xs: Array[A]): Seq[A] = new Seq[A] {
    def length = xs.length;
    def elements = Iterator.fromArray(xs);
    def apply(n: Int) = xs(n);
    override def hashCode(): Int = xs.hashCode();
    override def equals(y: Any): Boolean = (xs == y);
    override protected def stringPrefix: String = "Array";
  }
  def view[A](xs: Array[A]): Seq[A] = array2seq(xs);

  implicit def string2seq(str: String): Seq[Char] = new Seq[Char] {
    def length = str.length();
    def elements = Iterator.fromString(str);
    def apply(n: Int) = str.charAt(n);
    override def hashCode(): Int = str.hashCode();
    override def equals(y: Any): Boolean = (str == y);
    override protected def stringPrefix: String = "String";
  }
  def view(x: String): Seq[Char] = string2seq(x);

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
