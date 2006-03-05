/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
