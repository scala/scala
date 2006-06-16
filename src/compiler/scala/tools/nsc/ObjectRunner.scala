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
  def run(
      classpath: List[String],
      objectName: String,
      arguments: Seq[String]): Unit =
    try {
      val classpathURLs = classpath.map(s => new File(s).toURL).toArray
      val mainLoader = new URLClassLoader(classpathURLs, null)
      val clsToRun = Class.forName(objectName, true, mainLoader)

      val method = clsToRun.getMethod("main", List(classOf[Array[String]]).toArray)

      val res = method.invoke(null, List(arguments.toArray).toArray)
      ()
    } catch {
      case e: Exception =>
        // ClassNotFoundException, InvocationTargetException, NoSuchMethod ..
        Console.println(e)
        exit(1)
    }

}
