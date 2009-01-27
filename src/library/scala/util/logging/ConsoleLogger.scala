/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.logging

/**
 *  The trait <code>ConsoleLogger</code> is mixed into a concrete class who
 *  has class <code>Logged</code> among its base classes.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
trait ConsoleLogger extends Logged {

  /** logs argument to Console using <code>Console.println</code>
   *
   *  @param msg ...
   */
  override def log(msg: String): Unit = Console.println(msg)
}
