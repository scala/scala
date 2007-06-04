/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

import java.lang.{Class, ClassNotFoundException, NoSuchMethodException}
import java.lang.reflect.{Method, Modifier}
import java.net.{URL, URLClassLoader}

/** An object that runs another object specified by name.
 *
 *  @author  Lex Spoon
 *  @version 1.0, 15/06/2006
 */
object ObjectRunner {

  /** Look up a class with a given class path.
   *
   *  @param classpath  ...
   *  @param objectName ...
   *  @return           ...
   */
  private def findClass(classpath: List[URL], objectName: String)
  : Option[Class] =
  {
    try {
      val mainLoader = new URLClassLoader(classpath.toArray, null)
      Some(Class.forName(objectName, true, mainLoader))
    } catch {
      case e: SecurityException =>
        Console.println(e.getMessage)
        None
      case _: ClassNotFoundException =>
        None
    }
  }

  /** Check whether a class with the specified name
   *  exists on the specified class path.
   *
   *  @param classpath  ...
   *  @param objectName ...
   *  @return           <code>true</code> iff ...
   */
  def classExists(classpath: List[URL], objectName: String): Boolean =
    !findClass(classpath, objectName).isEmpty

  /** Run a given object, specified by name, using a
   *  specified classpath and argument list.
   *
   *  @param classpath  ...
   *  @param objectName ...
   *  @param arguments  ...
   *
   *  @throws ClassNotFoundException    ...
   *  @throws NoSuchMethodError         ...
   *  @throws InvocationTargetException ...
   */
  def run(classpath: List[URL], objectName: String, arguments: Seq[String]) {
    val clsToRun = findClass(classpath, objectName) match {
      case Some(cls) => cls
      case None => throw new ClassNotFoundException(objectName)
    }

    val method = clsToRun.getMethod("main", List(classOf[Array[String]]).toArray)
    if ((method.getModifiers & Modifier.STATIC) == 0)
      throw new NoSuchMethodException(objectName + ".main is not static")

    method.invoke(null, List(arguments.toArray).toArray)
  }
}
