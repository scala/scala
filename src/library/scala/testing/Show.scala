/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.testing

/** Classes inheriting trait `Show` can test their member methods using the
 *  notation `meth(arg,,1,,, ..., arg,,n,,)`, where `meth` is the name of
 *  the method and `arg,,1,,,...,arg,,n,,` are the arguments.
 *
 *  The only difference to a normal method call is the leading quote
 *  character (`'`). A quoted method call like the one above will produces
 *  a legible diagnostic to be printed on [[scala.Console]].
 *
 *  It is of the form
 *
 *    `meth(arg,,1,,, ..., arg,,n,,)`  gives  `&lt;result&gt;`
 *
 *  where `&lt;result&gt;` is the result of evaluating the call.
 *
 */
@deprecated("This class will be removed.", "2.10.0")
trait Show {

  /** An implicit definition that adds an apply method to Symbol which forwards to `test`. 
   *  Prints out diagnostics of method applications.
   */
  implicit class SymApply(f: Symbol) {
    def apply[A](args: A*) {
      println(test(f, args: _*))
    }
  }

  @deprecated("use SymApply instead", "2.10")
  def symApply(sym: Symbol): SymApply = new SymApply(sym)

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
