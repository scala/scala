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

    def error(message: String): All = throw new Error(message);

    def exit: scala.Unit = java.lang.System.exit(0);

    def assert(assertion: Boolean): Unit = {
    	if (!assertion)
    		throw new Error("assertion failed");
    }
    def assert(assertion: Boolean, message: Any): Unit = {
    	if (!assertion)
    		throw new Error("assertion failed: " + message);
    }

    type Pair[+p, +q] = Tuple2[p, q];
    def Pair[a, b](x: a, y: b) = Tuple2(x, y);

    type Triple[+a, +b, +c] = Tuple3[a, b, c];
    def Triple[a, b, c](x: a, y: b, z: c) = Tuple3(x, y, z);

    def id[a](x: a): a = x;
    def fst[a](x: a, y: Any): a = x;
    def scd[a](x: Any, y: a): a = y;

  val Text = scala.xml.Text;

  type Function[-a,+b] = Function1[a,b];

}
*
