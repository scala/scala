/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

import scala.compat.Platform.currentTime
import scala.collection.mutable.ListBuffer

/** The `Application` trait can be used to quickly turn objects
 *  into executable programs. Here is an example:
 *  {{{
 *    object Main extends Application {
 *      Console.println("Hello World: " + (arguments mkString ", "))
 *    }
 *  }}}
 *  Here, object `Main` inherits the `main` method of `Application`.
 *
 *  `arguments` returns the current command line arguments as an array.
 *
 *  Note: The use of Application was discouraged prior to 2.9 because
 *  application code would be run in the object constructor. This is no longer true.
 *  Application code is now stored and run in the main method. As a consequence,
 *  extending `Application` is now recommended over implementing `main` explicitly.
 *
 *  @author  Martin Odersky
 *  @version 2.0, 14/12/2010
 */
trait Application extends DelayedInit {

  /** The time when the execution of this program started, in milliseconds since 1
    * January 1970 UTC. */
  val executionStart: Long = currentTime

  /** The command line arguments passed to the application's `main` method.
   */
  protected def arguments: Array[String] = args

  private var args: Array[String] = _

  private val initCode = new ListBuffer[() => Unit]

  /** The init hook. This saves all initialization code for execution within `main`.
   *  This methos is normally never called directly from user code.
   *  Instead it is called as compiler-generated code for those classes, objects, and traits
   *  that inherit from the `DelayedInit` trait and that do not themselves define
   *  a `delayedInit` method.
   *  @param body the initialization code to be stored for later execution
   */
  override def delayedInit(body: => Unit) {
    initCode += (() => body)
  }

  /** The main method.
   *  This stores all argument so that they can be retrieved with `arguments`
   *  and the executes all initialization code segements in the order they were
   *  passed to `delayedInit`
   *  @param args the arguments passed to the main method
   */
  def main(args: Array[String]) = {
    this.args = args
    for (proc <- initCode) proc()
    if (util.Properties.propIsSet("scala.time")) {
      val total = currentTime - executionStart
      Console.println("[total " + total + "ms]")
    }
  }
}
