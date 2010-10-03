/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import java.lang.{ ClassLoader => JavaClassLoader }
import java.lang.reflect.{ Constructor, Modifier, Method }
import java.net.URL
import ScalaClassLoader._
import scala.util.control.Exception.{ catching }

trait ScalaClassLoader extends JavaClassLoader {
  /** Override to see classloader activity traced */
  protected def trace: Boolean = false

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

  override def findClass(name: String) = {
    val result = super.findClass(name)
    if (trace) println("findClass(%s) = %s".format(name, result))
    result
  }

  override def loadClass(name: String, resolve: Boolean) = {
    val result = super.loadClass(name, resolve)
    if (trace) println("loadClass(%s, %s) = %s".format(name, resolve, result))
    result
  }

  /** The actual bytes for a class file, or an empty array if it can't be found. */
  def findBytesForClassName(s: String): Array[Byte] = {
    val name = s.replaceAll("""\.""", "/") + ".class"
    val url = this.getResource(name)

    if (url == null) Array()
    else new io.Streamable.Bytes { def inputStream() = url.openStream } . toByteArray()
  }

  /** Run the main method of a class to be loaded by this classloader */
  def run(objectName: String, arguments: Seq[String]) {
    val clsToRun = tryToInitializeClass(objectName) getOrElse (
      throw new ClassNotFoundException(objectName)
    )

    val method = clsToRun.getMethod("main", classOf[Array[String]])
    if (!Modifier.isStatic(method.getModifiers))
      throw new NoSuchMethodException(objectName + ".main is not static")

    asContext(method.invoke(null, Array(arguments.toArray: AnyRef): _*))  // !!! : AnyRef shouldn't be necessary
  }
}

object ScalaClassLoader {
  class URLClassLoader(urls: Seq[URL], parent: JavaClassLoader)
      extends java.net.URLClassLoader(urls.toArray, parent)
      with ScalaClassLoader {

    private var classloaderURLs = urls.toList

    /** Override to widen to public */
    override def addURL(url: URL) = {
      classloaderURLs +:= url
      super.addURL(url)
    }

    override def toString = urls.mkString("URLClassLoader(\n  ", "\n  ", "\n)\n")
  }

  def setContextLoader(cl: JavaClassLoader) = Thread.currentThread.setContextClassLoader(cl)
  def getContextLoader() = Thread.currentThread.getContextClassLoader()
  def getSystemLoader(): ScalaClassLoader = new JavaClassLoader(JavaClassLoader.getSystemClassLoader()) with ScalaClassLoader
  def defaultParentClassLoader() = findExtClassLoader()

  def fromURLs(urls: Seq[URL], parent: ClassLoader = defaultParentClassLoader()): URLClassLoader =
    new URLClassLoader(urls.toList, parent)

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

  /** Finding what jar a clazz or instance came from */
  def origin(x: Any): Option[URL] = originOfClass(x.asInstanceOf[AnyRef].getClass)
  def originOfClass(x: Class[_]): Option[URL] =
    Option(x.getProtectionDomain.getCodeSource) flatMap (x => Option(x.getLocation))
}
