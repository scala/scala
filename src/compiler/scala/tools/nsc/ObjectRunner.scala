/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
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
 *  @version 1.1, 2007/7/13
 */
object ObjectRunner
{
  // we cannot use the app classloader here or we get what looks to
  // be classloader deadlock, but if we pass null we bypass the extension
  // classloader and our extensions, so we search the hierarchy to find
  // the classloader whose parent is null.  Resolves bug #857.
  private def findExtClassLoader(): ClassLoader = {
    def search(cl: ClassLoader): ClassLoader = {
      if (cl == null) null
      else if (cl.getParent == null) cl
      else search(cl.getParent)
    }

    search(Thread.currentThread.getContextClassLoader)
  }

  /** Create a class loader for the specified class path */
  private def makeClassLoader(classpath: List[URL]): URLClassLoader =
    makeClassLoader(classpath, findExtClassLoader())
  private def makeClassLoader(classpath: List[URL], parent: ClassLoader): URLClassLoader =
    new URLClassLoader(classpath.toArray, parent)

  /** Look up a class with a given class path. */
  private def findClass(loader: ClassLoader, objectName: String)
  : Option[Class[T] forSome { type T }] =
  {
    try {
      Some(Class.forName(objectName, true, loader))
    } catch {
      case e: SecurityException =>
        Console.println(e.getMessage)
        None
      case _: ClassNotFoundException =>
        None
    }
  }

  /** Check whether a class with the specified name
   *  exists on the specified class path. */
  def classExists(classpath: List[URL], objectName: String): Boolean =
    !findClass(makeClassLoader(classpath), objectName).isEmpty

  /** Set the Java context class loader while executing an action */
  def withContextClassLoader[T](loader: ClassLoader)(action: =>T): T = {
    val oldLoader = Thread.currentThread.getContextClassLoader
    try {
      Thread.currentThread.setContextClassLoader(loader)
      action
    } finally {
      Thread.currentThread.setContextClassLoader(oldLoader)
    }
  }


  /** Run a given object, specified by name, using a
   *  specified classpath and argument list.
   *
   *  @throws ClassNotFoundException
   *  @throws NoSuchMethodError
   *  @throws InvocationTargetException
   */
  def run(classpath: List[URL], objectName: String, arguments: Seq[String]) {
    val loader = makeClassLoader(classpath)
    val clsToRun = findClass(loader, objectName) match {
      case Some(cls) => cls
      case None => throw new ClassNotFoundException(objectName)
    }

    val method = clsToRun.getMethod("main", classOf[Array[String]])
    if ((method.getModifiers & Modifier.STATIC) == 0)
      throw new NoSuchMethodException(objectName + ".main is not static")

    withContextClassLoader(loader) {
      method.invoke(null, List(arguments.toArray).toArray: _*)
    }
  }
}
