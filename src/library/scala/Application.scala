/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.compat.Platform.currentTime

/** The `Application` trait can be used to quickly turn objects
 *  into executable programs, but is ''not recommended''.
 *  Here is an example:
 *  {{{
 *  object Main extends Application {
 *    Console.println("Hello World!")
 *  }
 *  }}}
 *  Here, object `Main` inherits the `main` method of `Application`.
 *  The body of the `Main` object defines the main program. This technique
 *  does not work if the main program depends on command-line arguments
 *  (which are not accessible with the technique presented here).
 *
 *  It is possible to time the execution of objects that inherit from class
 *  `Application` by setting the global `scala.time`
 *  property. Here is an example for benchmarking object `Main`:
 *  {{{
 *  java -Dscala.time Main
 *  }}}
 *  In practice the `Application` trait has a number of serious pitfalls:
 *
 *  - Threaded code that references the object will block until static
 *    initialization is complete.  However, because the entire execution
 *    of an `object` extending `Application` takes place during
 *    static initialization, concurrent code will ''always'' deadlock if
 *    it must synchronize with the enclosing object.
 *  - As described above, there is no way to obtain the
 *    command-line arguments because all code in body of an `object`
 *    extending `Application` is run as part of the static initialization
 *    which occurs before `Application`'s `main` method
 *    even begins execution.
 *  - Static initializers are run only once during program execution, and
 *    JVM authors usually assume their execution to be relatively short.
 *    Therefore, certain JVM configurations may become confused, or simply
 *    fail to optimize or JIT the code in the body of an `object` extending
 *    `Application`.  This can lead to a significant performance degradation.
 *
 *  It is recommended to use the [[scala.App]] trait instead.
 *  {{{
 *  object Main {
 *    def main(args: Array[String]) {
 *      //..
 *    }
 *  }
 *  }}}
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/09/2003
 */
@deprecated("use App instead", "2.9.0")
trait Application {

  /** The time when the execution of this program started,
    * in milliseconds since 1 January 1970 UTC. */
  val executionStart: Long = currentTime

  /** The default main method.
   *
   *  @param args the arguments passed to the main method
   */
  def main(args: Array[String]) {
    if (util.Properties propIsSet "scala.time") {
      val total = currentTime - executionStart
      Console.println("[total " + total + "ms]")
    }
  }
}
