/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.logging

/**
 *  The trait `ConsoleLogger` is mixed into a concrete class who
 *  has class `Logged` among its base classes.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
trait ConsoleLogger extends Logged {

  /** logs argument to Console using [[scala.Console.println]]
   */
  override def log(msg: String): Unit = Console.println(msg)
}
