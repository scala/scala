/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import java.lang.System.getProperty
import scala.compat.Platform.currentTime

/** <p>
 *    The <code>Application</code> trait can be used to quickly turn objects
 *    into executable programs, but is <em>not recommended</em>.
 *    Here is an example:
 *  </p><pre>
 *  <b>object</b> Main <b>extends</b> Application {
 *    Console.println("Hello World!")
 *  }
 *  </pre>
 *  <p>
 *    Here, object <code>Main</code> inherits the <code>main</code> method
 *    of <code>Application</code>. The body of the <code>Main</code> object
 *    defines the main program. This technique does not work if the main
 *    program depends on command-line arguments (which are not accessible
 *    with the technique presented here).
 *  </p>
 *  <p>
 *    It is possible to time the execution of objects that inherit from class
 *    <code>Application</code> by setting the global <code>scala.time</code>
 *    property. Here is an example for benchmarking object <code>Main</code>:
 *  </p><pre>
 *  java -Dscala.time Main
 *  </pre>
 *  <p>
 *    In practice the <code>Application</code> trait has a number of serious
 *    pitfalls:
 *  </p>
 *  <ul>
 *    <li> Threaded code that references the object will block until static
 *    initialization is complete.  However, because the entire execution of an
 *    <code>object</code> extending <code>Application</code> takes place during
 *    static initialization, concurrent code will <em>always</em> deadlock if
 *    it must synchronize with the enclosing object.</li>
 *    <li>As described above, there is no way to obtain the
 *    command-line arguments because all code in body of an <code>object</code>
 *    extending <code>Application</code> is run as part of the static initialization
 *    which occurs before <code>Application</code>'s <code>main</code> method
 *    even begins execution.</li>
 *    <li>Static initializers are run only once during program execution, and
 *    JVM authors usually assume their execution to be relatively short.
 *    Therefore, certain JVM configurations may become confused, or simply fail to
 *    optimize or JIT the code in the body of an <code>object</code> extending
 *    <code>Application</code>.  This can lead to a significant
 *    performance degradation.</li>
 *  </ul>
 *
 *  Instead, it is recommended to define a <code>main</code> method explicitly:
 *  <pre>
 *  <b>object</b> Main {
 *    <b>def</b> main(args: Array[String]) {
 *      //..
 *    }
 *  }
 *  </pre>
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/09/2003
 */

trait Application {

  /** The time when execution of this program started.
   */
  val executionStart: Long = currentTime

  /** The default main method.
   *
   *  @param args the arguments passed to the main method
   */
  def main(args: Array[String]) {
    if (getProperty("scala.time") ne null) {
      val total = currentTime - executionStart
      Console.println("[total " + total + "ms]")
    }
  }
}
