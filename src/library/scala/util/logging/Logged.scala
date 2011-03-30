/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.logging

/** Mixing in Logged indicates that a class provides support for logging.
 *  For instance:
{{{
  // The developer of the library writes:
  class MyClass extends Logged {
    // do stuff, call log
  }
  // The user of the library instantiates:
  val x = new MyClass() with ConsoleLogger
}}}
 * and the logging is sent to the [[scala.util.logging.ConsoleLogger]] object.
 */
trait Logged {
  /** This method should log the message given as argument somewhere
   *  as a side-effect.
   *
   *  @param msg  message to be logged
   */
  def log(msg: String): Unit = {}
}
