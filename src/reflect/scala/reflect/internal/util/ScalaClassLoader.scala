/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package reflect.internal.util

import scala.language.implicitConversions

import java.lang.{ ClassLoader => JClassLoader }
import java.lang.reflect.Modifier
import java.net.{ URLClassLoader => JURLClassLoader }
import java.net.URL

import scala.reflect.runtime.ReflectionUtils.{ show, unwrapHandler }
import ScalaClassLoader._
import scala.util.control.Exception.{ catching }
import scala.reflect.{ ClassTag, classTag }

trait HasClassPath {
  def classPathURLs: Seq[URL]
}

/** A wrapper around java.lang.ClassLoader to lower the annoyance
 *  of java reflection.
 */
trait ScalaClassLoader extends JClassLoader {
  /** Executing an action with this classloader as context classloader */
  def asContext[T](action: => T): T = {
    val saved = contextLoader
    try { setContext(this) ; action }
    finally setContext(saved)
  }
  def setAsContext() { setContext(this) }

  /** Load and link a class with this classloader */
  def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = false)
  /** Load, link and initialize a class with this classloader */
  def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = true)

  private def tryClass[T <: AnyRef](path: String, initialize: Boolean): Option[Class[T]] =
    catching(classOf[ClassNotFoundException], classOf[SecurityException]) opt
      Class.forName(path, initialize, this).asInstanceOf[Class[T]]

  /** Create an instance of a class with this classloader */
  def create(path: String): AnyRef =
    tryToInitializeClass[AnyRef](path).map(_.newInstance()).orNull

  /** Create an instance with ctor args, or invoke errorFn before throwing. */
  def create[T <: AnyRef : ClassTag](path: String, errorFn: String => Unit)(args: AnyRef*): T = {
    def fail(msg: String) = error(msg, new IllegalArgumentException(msg))
    def error(msg: String, e: Throwable) = { errorFn(msg) ; throw e }
    try {
      val clazz = Class.forName(path, /*initialize =*/ true, /*loader =*/ this)
      if (classTag[T].runtimeClass isAssignableFrom clazz) {
        val ctor = {
          val maybes = clazz.getConstructors filter (c => c.getParameterCount == args.size &&
            (c.getParameterTypes zip args).forall { case (k, a) => k isAssignableFrom a.getClass })
          if (maybes.size == 1) maybes.head
          else fail(s"Constructor must accept arg list (${args map (_.getClass.getName) mkString ", "}): ${path}")
        }
        (ctor.newInstance(args: _*)).asInstanceOf[T]
      } else {
        errorFn(s"""Loader for ${classTag[T]}:   [${show(classTag[T].runtimeClass.getClassLoader)}]
                   |Loader for ${clazz.getName}: [${show(clazz.getClassLoader)}]""".stripMargin)
        fail(s"Not a ${classTag[T]}: ${path}")
      }
    } catch {
      case e: ClassNotFoundException =>
        error(s"Class not found: ${path}", e)
      case e @ (_: LinkageError | _: ReflectiveOperationException) =>
        error(s"Unable to create instance: ${path}: ${e.toString}", e)
    }
  }

  /** The actual bytes for a class file, or an empty array if it can't be found. */
  def classBytes(className: String): Array[Byte] = classAsStream(className) match {
    case null   => Array()
    case stream => scala.reflect.io.Streamable.bytes(stream)
  }

  /** An InputStream representing the given class name, or null if not found. */
  def classAsStream(className: String) = getResourceAsStream {
    if (className endsWith ".class") className
    else s"${className.replace('.', '/')}.class"  // classNameToPath
  }

  /** Run the main method of a class to be loaded by this classloader */
  def run(objectName: String, arguments: Seq[String]) {
    val clsToRun = tryToInitializeClass(objectName) getOrElse (
      throw new ClassNotFoundException(objectName)
    )
    val method = clsToRun.getMethod("main", classOf[Array[String]])
    if (!Modifier.isStatic(method.getModifiers))
      throw new NoSuchMethodException(objectName + ".main is not static")

    try asContext(method.invoke(null, Array(arguments.toArray: AnyRef): _*)) // !!! : AnyRef shouldn't be necessary
    catch unwrapHandler({ case ex => throw ex })
  }
}

/** Methods for obtaining various classloaders.
 *      appLoader: the application classloader.  (Also called the java system classloader.)
 *      extLoader: the extension classloader.
 *     bootLoader: the boot classloader.
 *  contextLoader: the context classloader.
 */
object ScalaClassLoader {
  /** Returns loaders which are already ScalaClassLoaders unaltered,
   *  and translates java.net.URLClassLoaders into scala URLClassLoaders.
   *  Otherwise creates a new wrapper.
   */
  implicit def apply(cl: JClassLoader): ScalaClassLoader = cl match {
    case cl: ScalaClassLoader => cl
    case cl: JURLClassLoader  => new URLClassLoader(cl.getURLs.toSeq, cl.getParent)
    case _                    => new JClassLoader(cl) with ScalaClassLoader
  }
  def contextLoader = apply(Thread.currentThread.getContextClassLoader)
  def appLoader     = apply(JClassLoader.getSystemClassLoader)
  def setContext(cl: JClassLoader) =
    Thread.currentThread.setContextClassLoader(cl)
  def savingContextLoader[T](body: => T): T = {
    val saved = contextLoader
    try body
    finally setContext(saved)
  }

  class URLClassLoader(urls: Seq[URL], parent: JClassLoader)
      extends JURLClassLoader(urls.toArray, parent)
         with ScalaClassLoader
         with HasClassPath {

    private var classloaderURLs: Seq[URL] = urls
    def classPathURLs: Seq[URL] = classloaderURLs

    /** Override to widen to public */
    override def addURL(url: URL) = {
      classloaderURLs :+= url
      super.addURL(url)
    }
    override def close(): Unit = {
      super.close()
      classloaderURLs = null
    }
  }

  def fromURLs(urls: Seq[URL], parent: ClassLoader = null): URLClassLoader =
    new URLClassLoader(urls, parent)

  /** True if supplied class exists in supplied path */
  def classExists(urls: Seq[URL], name: String): Boolean =
    (fromURLs(urls) tryToLoadClass name).isDefined

  /** Finding what jar a clazz or instance came from */
  def originOfClass(x: Class[_]): Option[URL] =
    Option(x.getProtectionDomain.getCodeSource) flatMap (x => Option(x.getLocation))
}
