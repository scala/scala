/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

//import java.lang.System.getProperty
//import scala.compat.Platform.currentTime

/** <p>
 *    The <code>Application</code> class can be used to quickly turn objects
 *    into executable programs. Here is an example:
 *  </p><pre>
 *  object Main with Application {
 *    Console.println("Hello World!");
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
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/09/2003
 */

trait Application {

    /** The time when execution of this program started.
     */
//    val executionStart: Long = currentTime

  /** The default main method.
   *
   *  @param args the arguments passed to the main method
   */
  def main(args: Array[String]) = {
//     if (getProperty("scala.time") ne null) {
//       val total = currentTime - executionStart
//       Console.println("[total " + total + "ms]")
//     }
  }

}
