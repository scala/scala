/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT.
// genprod generated these sources at: Mon Jun 08 18:05:40 CEST 2015

package scala


/** A function of 0 parameters.
 *  
 *  In the following example, the definition of javaVersion is a
 *  shorthand for the anonymous class definition anonfun0:
 *
 *  {{{
 *  object Main extends App {
 *    val javaVersion = () => sys.props("java.version")
 *
 *    val anonfun0 = new Function0[String] {
 *      def apply(): String = sys.props("java.version")
 *    }
 *    assert(javaVersion() == anonfun0())
 * }
 *  }}}
 */
trait Function0[@specialized(Specializable.Primitives) +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(): R

  override def toString() = "<function0>"
}
