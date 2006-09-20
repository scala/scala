/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.logging

/**
 *  Mixing in the class Logged indicates that a class provides support
 *  for logging. For instance, a developer of a library writes
 *  <code>
   class MyClass with Logged { ... do stuff, call log }
   </code>
   *
   * The user of the library instantiates:
   <code>
   val x = new MyClass() with ConsoleLogger;
   </code>
   * and the logging will be sent to the Console.
   */
trait Logged {

  /** this method should log the message given as argument somewhere
   *  as a side-effect.
   *
   *  @param msg ...
   */
  def log(msg: String): Unit = {}
}
