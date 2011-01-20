/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.logging

/** <p>
 *    Mixing in the class <code>Logged</code> indicates that a class provides
 *    support for logging. For instance, the developer of a library writes
 *  </p>
 *  <pre>
 *    <b>class</b> MyClass <b>extends</b> Logged { /* do stuff, call log */ }
 *  </pre>
 *  <p>
 *    The user of the library instantiates:
 *  </p>
 *  <pre>
 *    <b>val</b> x = <b>new</b> MyClass() <b>with</b> ConsoleLogger
 *  </pre>
 *  <p>
 *    and the logging will be sent to the <a href="../../Console$object.html"
 *    target="contentFrame"><code>Console</code></a> object.
 *  </p>
 */
trait Logged {

  /** This method should log the message given as argument somewhere
   *  as a side-effect.
   *
   *  @param msg ...
   */
  def log(msg: String): Unit = {}
}
