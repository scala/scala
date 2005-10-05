/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** The <code>Application</code> class can be used to quickly turn objects
 *  into executable programs. Here is an example:
 *  <pre>
 *  object Main with Application {
 *    Console.println("Hello World!");
 *  }
 *  </pre>
 *  Here, object <code>Main</code> inherits the <code>main</code> method
 *  of <code>Application</code>. The body of the <code>Main</code> object
 *  defines the main program. This technique does not work if the main
 *  program depends on command-line arguments (which are not accessible
 *  with the technique presented here).
 *
 *  It is possible to time the execution of objects that inherit from
 *  class <code>Application</code> by setting the global scala.time property.
 *  Here is an example for benchmarking object <code>Main</code>:
 *  <pre>
 *  java -Dscala.time Main
 *  </pre>
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/09/03
 */

[_trait_] class Application {

    /** The time when execution of this program started.
     */
    val executionStart: Long = java.lang.System.currentTimeMillis();

    /** The default main method.
     */
    def main(args: Array[String]) = {
        if (java.lang.System.getProperty("scala.time") != null)
          java.lang.System.out.println("[total " +
                       (java.lang.System.currentTimeMillis()
                    - executionStart) + "ms]");
    }
}
