/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.util

import java.lang.{ ClassLoader => JavaClassLoader }
import java.lang.reflect.{ Modifier, Method }
import java.net.URL
import ScalaClassLoader._
import scala.util.control.Exception.{ catching }

trait ScalaClassLoader extends JavaClassLoader
{
  /** Executing an action with this classloader as context classloader */
  def asContext[T](action: => T): T = {
    val oldLoader = getContextLoader
    try {
      setContextLoader(this)
      action
    }
    finally setContextLoader(oldLoader)
  }
  def setAsContext() { setContextLoader(this) }

  /** Load and link a class with this classloader */
  def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, false)
  /** Load, link and initialize a class with this classloader */
  def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, true)

  private def tryClass[T <: AnyRef](path: String, initialize: Boolean): Option[Class[T]] =
    catching(classOf[ClassNotFoundException], classOf[SecurityException]) opt
      Class.forName(path, initialize, this).asInstanceOf[Class[T]]

  /** Create an instance of a class with this classloader */
  def create(path: String): AnyRef = {
    tryToInitializeClass(path) match {
      case Some(clazz)    => clazz.newInstance()
      case None           => null
    }
  }

  /** Run the main method of a class to be loaded by this classloader */
  def run(objectName: String, arguments: Seq[String]) {
    val clsToRun = tryToInitializeClass(objectName) getOrElse (
      throw new ClassNotFoundException(objectName)
    )

    val method = clsToRun.getMethod("main", classOf[Array[String]])
    if (!Modifier.isStatic(method.getModifiers))
      throw new NoSuchMethodException(objectName + ".main is not static")

    asContext(method.invoke(null, Array(arguments.toArray): _*))
  }
}

class URLClassLoader(urls: List[URL], parent: JavaClassLoader)
    extends java.net.URLClassLoader(urls.toArray, parent)
    with ScalaClassLoader
{
  /** Override to widen to public */
  override def addURL(url: URL) = super.addURL(url)
}

object ScalaClassLoader {
  def setContextLoader(cl: JavaClassLoader) = Thread.currentThread.setContextClassLoader(cl)
  def getContextLoader() = Thread.currentThread.getContextClassLoader()
  def getSystemLoader() = JavaClassLoader.getSystemClassLoader()
  def defaultParentClassLoader() = findExtClassLoader()

  /** XXX move this to RichClass. */
  def callReflectively[T](clazz: Class[_], obj: String, method: String, args: Any*): Option[T] = {
    val exceptions = List(
      classOf[ClassNotFoundException],
      classOf[NoSuchMethodException],
      classOf[SecurityException],
      classOf[NullPointerException],
      classOf[ClassCastException]
    )

    catching(exceptions: _*) opt {
      val o: Class[_] = clazz.getClassLoader loadClass obj
      val m: Method = o getDeclaredMethod method
      m.invoke(o, args map (_.asInstanceOf[AnyRef]) : _*).asInstanceOf[T]
    }
  }

  def fromURLs(urls: Seq[URL]): URLClassLoader =
    new URLClassLoader(urls.toList, defaultParentClassLoader())

  /** True if supplied class exists in supplied path */
  def classExists(urls: Seq[URL], name: String): Boolean =
    (fromURLs(urls) tryToLoadClass name).isDefined

  // we cannot use the app classloader here or we get what looks to
  // be classloader deadlock, but if we pass null we bypass the extension
  // classloader and our extensions, so we search the hierarchy to find
  // the classloader whose parent is null.  Resolves bug #857.
  def findExtClassLoader(): JavaClassLoader = {
    def search(cl: JavaClassLoader): JavaClassLoader = {
      if (cl == null) null
      else if (cl.getParent == null) cl
      else search(cl.getParent)
    }

    search(getContextLoader())
  }
}