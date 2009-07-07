/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.testing

/** <p>
 *    Classes inheriting trait <code>Show</code> can test their member methods
 *    using the notattion <code>meth(arg<sub>1</sub>, ..., arg<sub>n</sub>)</code>,
 *    where <code>meth</code> is the name of the method and
 *    <code>arg<sub>1</sub>,...,arg<sub>n</sub></code> are the arguments.
 *    The only difference to a normal method call is the leading quote
 *    character ('). A quoted method call like the one above will produces a
 *    legible diagnostic to be printed on <a href="../Console.html"
 *    target="ContentFrame"><code>Console</code></a>. It is of the form
 *  </p><pre>
 *    meth(arg<sub>1</sub>, ..., arg<sub>n</sub>)  gives  &lt;result&gt;</pre>
 *  <p>
 *    where <code>&lt;result&gt;</code> is the result of evaluating the call.
 *  </p>
 */
trait Show {

  /** The result class of wrapper <code>symApply</code>.
   *  Prints out diagnostics of method applications.
   */
  class SymApply(f: Symbol) {
    def apply[A](args: A*) {
      println(test(f, args: _*))
    }
  }

  /** An implicit definition that adds an apply method to Symbol which forwards to `test`.
   */
  implicit def symApply(sym: Symbol) = new SymApply(sym)

  /** Apply method with name of given symbol `f` to given arguments and return
   *  a result diagnostics.
   */
  def test[A](f: Symbol, args: A*): String = {
    val args1 = args map (_.asInstanceOf[AnyRef])
    def testMethod(meth: java.lang.reflect.Method): String =
      f.name+"("+(args mkString ",")+")  gives  "+
      {
        try {
          meth.invoke(this, args1: _*)
        } catch {
          case ex: IllegalAccessException => ex
          case ex: IllegalArgumentException => ex
          case ex: java.lang.reflect.InvocationTargetException => ex
        }
      }
    getClass.getMethods.toList filter (_.getName == f.name) match {
      case List() =>
        f.name+" is not defined"
      case List(m) =>
        testMethod(m)
      case ms => // multiple methods, disambiguate by number of arguments
        ms filter (_.getParameterTypes.length == args.length) match {
          case List() =>
            testMethod(ms.head) // go ahead anyway, to get an exception
          case List(m) =>
            testMethod(m)
          case ms =>
            "cannot disambiguate between multiple implementations of "+f.name
        }
    }
  }
}
