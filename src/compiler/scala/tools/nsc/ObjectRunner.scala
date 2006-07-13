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
  /** Look up a class with a given class path */
  def findClass(classpath: List[String], objectName: String)
  : Option[Class] =
  {
    val classpathURLs = classpath.map(s => new File(s).toURL).toArray
    val mainLoader = new URLClassLoader(classpathURLs, null)
    try {
      Some(Class.forName(objectName, true, mainLoader))
    } catch {
      case _:ClassNotFoundException => None
    }
  }

  /** Check whether a class with the specified name
    * exists on the specified class path.
    */
  def classExists(classpath: List[String], objectName: String) =
    !(findClass(classpath, objectName).isEmpty)

  /** Run a given object, specified by name, using a
    * specified classpath and argument list.
    *
    * Throws: ClassNotFoundException, NoSuchMethodError,
    *         InvocationTargetException
    */
  def run(
      classpath: List[String],
      objectName: String,
      arguments: Seq[String]): Unit =
  {
      val clsToRun = findClass(classpath, objectName) match {
        case Some(cls) => cls
        case None => throw new ClassNotFoundException(objectName)
      }

      val method = clsToRun.getMethod("main", List(classOf[Array[String]]).toArray)

      method.invoke(null, List(arguments.toArray).toArray)
  }
}
