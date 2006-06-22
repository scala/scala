/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

import java.io.File
import java.lang.reflect.{Method,Modifier}
import java.net.URLClassLoader

/** An object that runs another object specified by name. */
object ObjectRunner {
  /** Run a given object, specified by name, using a
    * specified classpath and argument list.
    *
    * Throws: ClassNotFoundException, NoSuchMtehodError,
    *         InvocationTargetException
    */
  def run(
      classpath: List[String],
      objectName: String,
      arguments: Seq[String]): Unit =
  {
      val classpathURLs = classpath.map(s => new File(s).toURL).toArray
      val mainLoader = new URLClassLoader(classpathURLs, null)
      val clsToRun = Class.forName(objectName, true, mainLoader)

      val method = clsToRun.getMethod("main", List(classOf[Array[String]]).toArray)

      method.invoke(null, List(arguments.toArray).toArray)
  }
}
