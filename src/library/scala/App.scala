/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.compat.Platform.currentTime
import scala.collection.mutable.ListBuffer

/** The `App` trait can be used to quickly turn objects
 *  into executable programs. Here is an example:
 *  {{{
 *  object Main extends App {
 *    Console.println("Hello World: " + (args mkString ", "))
 *  }
 *  }}}
 *  Here, object `Main` inherits the `main` method of `App`.
 *
 *  `args` returns the current command line arguments as an array.
 *
 *  ==Caveats==
 *
 *  '''''It should be noted that this trait is implemented using the [[DelayedInit]]
 *  functionality, which means that fields of the object will not have been initialized
 *  before the main method has been executed.'''''
 *
 *  It should also be noted that the `main` method should not be overridden:
 *  the whole class body becomes the “main method”.
 *
 *  Future versions of this trait will no longer extend `DelayedInit`.
 *
 *  @author  Martin Odersky
 *  @version 2.1, 15/02/2011
 */
trait App extends DelayedInit {

  /** The time when the execution of this program started, in milliseconds since 1
    * January 1970 UTC. */
  @deprecatedOverriding("executionStart should not be overridden", "2.11.0")
  val executionStart: Long = currentTime

  /** The command line arguments passed to the application's `main` method.
   */
  @deprecatedOverriding("args should not be overridden", "2.11.0")
  protected def args: Array[String] = _args

  private var _args: Array[String] = _

  private val initCode = new ListBuffer[() => Unit]

  /** The init hook. This saves all initialization code for execution within `main`.
   *  This method is normally never called directly from user code.
   *  Instead it is called as compiler-generated code for those classes and objects
   *  (but not traits) that inherit from the `DelayedInit` trait and that do not
   *  themselves define a `delayedInit` method.
   *  @param body the initialization code to be stored for later execution
   */
  @deprecated("the delayedInit mechanism will disappear", "2.11.0")
  override def delayedInit(body: => Unit) {
    initCode += (() => body)
  }

  /** The main method.
   *  This stores all arguments so that they can be retrieved with `args`
   *  and then executes all initialization code segments in the order in which
   *  they were passed to `delayedInit`.
   *  @param args the arguments passed to the main method
   */
  @deprecatedOverriding("main should not be overridden", "2.11.0")
  def main(args: Array[String]) = {
    this._args = args
    for (proc <- initCode) proc()
    if (util.Properties.propIsSet("scala.time")) {
      val total = currentTime - executionStart
      Console.println("[total " + total + "ms]")
    }
  }
}
