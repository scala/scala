/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2003, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

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

  def id[a](x: a): a = x;
  def fst[a](x: a, y: Any): a = x;
  def scd[a](x: Any, y: a): a = y;

  val namespace$default = "";

  type Function[-a,+b] = Function1[a,b];

  // arrays -----------------------------------------------------------

  /** Create an array with given elements.
   *
   *  @param xs the elements to put in the array
   *  @return the array containing elements xs.
   */
  def Array[A](xs: A*): Array[A] = {
    val array = new Array[A](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
/*
  def Array[A <: AnyRef](xs: A*): Array[A] = {
    val array = new Array[A](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }

  def Array(x: boolean, xs: boolean*): Array[boolean] = {
    val array = new Array[boolean](xs.length + 1);
    array(0) = x;
    var i = 1;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(x: byte, xs: byte*): Array[byte] = {
    val array = new Array[byte](xs.length + 1);
    array(0) = x;
    var i = 1;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(x: short, xs: short*): Array[short] = {
    val array = new Array[short](xs.length + 1);
    array(0) = x;
    var i = 1;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(x: char, xs: char*): Array[char] = {
    val array = new Array[char](xs.length + 1);
    array(0) = x;
    var i = 1;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(x: int, xs: int*): Array[int] = {
    val array = new Array[int](xs.length + 1);
    array(0) = x;
    var i = 1;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(x: long, xs: long*): Array[long] = {
    val array = new Array[long](xs.length + 1);
    array(0) = x;
    var i = 1;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(x: float, xs: float*): Array[float] = {
    val array = new Array[float](xs.length + 1);
    array(0) = x;
    var i = 1;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
  def Array(x: double, xs: double*): Array[double] = {
    val array = new Array[double](xs.length + 1);
    array(0) = x;
    var i = 1;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }
*/
  // errors and asserts -------------------------------------------------

  def error(message: String): All = throw new Error(message);

  def exit: Unit = java.lang.System.exit(0);

  def assert(assertion: Boolean): Unit = {
    if (!assertion)
      throw new Error("assertion failed");
  }

  def assert(assertion: Boolean, message: Any): Unit = {
    if (!assertion)
      throw new Error("assertion failed: " + message);
  }

  // views -------------------------------------------------------------

  def view(x: int): Ordered[int] = new Ordered[int] with Proxy(x) {
    def compareTo [b >: int <% Ordered[b]](y: b): int = y match {
      case y1: int =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  def view(x: char): Ordered[char] = new Ordered[char] with Proxy(x) {
    def compareTo [b >: char <% Ordered[b]](y: b): int = y match {
      case y1: char =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  def view(x: long): Ordered[long] = new Ordered[long] with Proxy(x) {
    def compareTo [b >: long <% Ordered[b]](y: b): int = y match {
      case y1: long =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  def view(x: float): Ordered[float] = new Ordered[float] with Proxy(x) {
    def compareTo [b >: float <% Ordered[b]](y: b): int = y match {
      case y1: float =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  def view(x: double): Ordered[double] = new Ordered[double] with Proxy(x) {
    def compareTo [b >: double <% Ordered[b]](y: b): int = y match {
      case y1: double =>
        if (x < y1) -1
        else if (x > y1) 1
        else 0
      case _ => -(y compareTo x)
    }
  }

  def view(x: boolean): Ordered[boolean] = new Ordered[boolean] with Proxy(x) {
    def compareTo [b >: boolean <% Ordered[b]](y: b): int = y match {
      case y1: boolean =>
        if (x == y1) 0
        else if (x) 1
        else -1
      case _ => -(y compareTo x)
    }
  }

  def view[A <% Ordered[A]](xs: Array[A]): Ordered[Array[A]] = new Ordered[Array[A]] with Proxy(xs) {
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

  private def first(xs: Int*): Int = xs.elements.find(x => x != 0) match {
    case Some(r) => r
    case _ => 0
  }

  /* We can't bootstrap currently with the following views included. We have to
   * wait for the next release...
   *
  def view[A <% Ordered[A], B <% Ordered[B]](x: Tuple2[A, B]): Ordered[Tuple2[A, B]] =
        new Ordered[Tuple2[A, B]] with Proxy(x) {
          def compareTo[T >: Tuple2[A, B] <% Ordered[T]](y: T): Int = y match {
            case y1: Tuple2[A, B] => first(x._1.compareTo(y1._1),
                                           x._2.compareTo(y1._2));
            case _ => -(y compareTo x)
          }
        }

  def view[A <% Ordered[A], B <% Ordered[B], C <% Ordered[C]]
        (x: Tuple3[A, B, C]): Ordered[Tuple3[A, B, C]] =
        new Ordered[Tuple3[A, B, C]] with Proxy(x) {
          def compareTo[T >: Tuple3[A, B, C] <% Ordered[T]](y: T): Int = y match {
            case y1: Tuple3[A, B, C] => first(x._1.compareTo(y1._1),
                                              x._2.compareTo(y1._2),
                                              x._3.compareTo(y1._3));
            case _ => -(y compareTo x)
          }
        }

  def view[A <% Ordered[A], B <% Ordered[B], C <% Ordered[C], D <% Ordered[D]]
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

  def view[A <% Ordered[A], B <% Ordered[B], C <% Ordered[C], D <% Ordered[D], E <% Ordered[E]]
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

  def view(x: String): Ordered[String] = new Ordered[String] with Proxy(x) {
    def compareTo [b >: String <% Ordered[b]](y: b): int = y match {
      case y1: String => x compareTo y1;
      case _ => -(y compareTo x)
    }
  }

  def view[A](xs: Array[A]): Seq[A] = new Seq[A] {
    def length = xs.length;
    def elements = Iterator.fromArray(xs);
    def apply(n: Int) = xs(n);
    override def hashCode(): Int = xs.hashCode();
    override def equals(y: Any): Boolean = xs.equals(y);
    override protected def stringPrefix: String = "Array";
  }

  def view(str: String): Seq[Char] = new Seq[Char] {
    def length = str.length();
    def elements = Iterator.fromString(str);
    def apply(n: Int) = str.charAt(n);
    override def hashCode(): Int = str.hashCode();
    override def equals(y: Any): Boolean = str.equals(y);
    override protected def stringPrefix: String = "String";
  }
}
