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

  type Function[-a,+b] = Function1[a,b];

  // arrays -----------------------------------------------------------

  /** Create an array with given elements.
   *
   *  @param xs the elements to put in the array
   *  @return the array containing elements xs.
   */
  def Array[A](xs: A*): Array[A] = {
    val array: Array[A] = new Array[A](xs.length);
    var i = 0;
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array;
  }

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

  def view[A](xs: Array[A]): Seq[A] = new Seq[A] {
    def length = xs.length;
    def elements = Iterator.fromArray(xs);
    def apply(n: Int) = xs(n);
    override def hashCode(): Int = xs.hashCode();
    override def equals(y: Any): Boolean = xs.equals(y);
    override protected def stringPrefix: String = "Array";
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
		else -1
	  case _ =>
		-(that compareTo xs)
	}
  }

  def view(x: String): Ordered[String] = new Ordered[String] with Proxy(x) {
    def compareTo [b >: String <% Ordered[b]](y: b): int = y match {
      case y1: String => x compareTo y1;
      case _ => -(y compareTo x)
    }
  }
}
