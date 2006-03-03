// $Id$

package scala.util.logging;
/**
 *  A ConsoleLogger is mixed into a concrete class who has class Logged
 *  among its base classes.
 */
mixin class ConsoleLogger {
  /** logs argument to Console using Console.println
   */
  def log(msg:String): Unit = Console.println(msg);
}
