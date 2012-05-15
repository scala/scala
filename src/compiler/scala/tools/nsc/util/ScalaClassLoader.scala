/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import java.lang.{ ClassLoader => JClassLoader }
import java.lang.reflect.{ Constructor, Modifier, Method }
import java.io.{ File => JFile }
import java.net.{ URLClassLoader => JURLClassLoader }
import java.net.URL
import scala.reflect.ReflectionUtils.unwrapHandler
import ScalaClassLoader._
import scala.util.control.Exception.{ catching }
import language.implicitConversions

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
  def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, false)
  /** Load, link and initialize a class with this classloader */
  def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, true)

  private def tryClass[T <: AnyRef](path: String, initialize: Boolean): Option[Class[T]] =
    catching(classOf[ClassNotFoundException], classOf[SecurityException]) opt
      Class.forName(path, initialize, this).asInstanceOf[Class[T]]

  /** Create an instance of a class with this classloader */
  def create(path: String): AnyRef =
    tryToInitializeClass[AnyRef](path) map (_.newInstance()) orNull

  def constructorsOf[T <: AnyRef : ClassTag]: List[Constructor[T]] =
    classTag[T].erasure.getConstructors.toList map (_.asInstanceOf[Constructor[T]])

  /** The actual bytes for a class file, or an empty array if it can't be found. */
  def classBytes(className: String): Array[Byte] = classAsStream(className) match {
    case null   => Array()
    case stream => io.Streamable.bytes(stream)
  }

  /** An InputStream representing the given class name, or null if not found. */
  def classAsStream(className: String) =
    getResourceAsStream(className.replaceAll("""\.""", "/") + ".class")

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

  /** A list comprised of this classloader followed by all its
   *  (non-null) parent classloaders, if any.
   */
  def loaderChain: List[ScalaClassLoader] = this :: (getParent match {
    case null => Nil
    case p    => p.loaderChain
  })
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
  def extLoader     = apply(appLoader.getParent)
  def bootLoader    = apply(null)
  def contextChain  = loaderChain(contextLoader)

  def pathToErasure[T: ClassTag]   = pathToClass(classTag[T].erasure)
  def pathToClass(clazz: Class[_]) = clazz.getName.replace('.', JFile.separatorChar) + ".class"
  def locate[T: ClassTag]          = contextLoader getResource pathToErasure[T]

  /** Tries to guess the classpath by type matching the context classloader
   *  and its parents, looking for any classloaders which will reveal their
   *  classpath elements as urls.  It it can't find any, creates a classpath
   *  from the supplied string.
   */
  def guessClassPathString(default: String = ""): String = {
    val classpathURLs = contextChain flatMap {
      case x: HasClassPath    => x.classPathURLs
      case x: JURLClassLoader => x.getURLs.toSeq
      case _                  => Nil
    }
    if (classpathURLs.isEmpty) default
    else JavaClassPath.fromURLs(classpathURLs).asClasspathString
  }

  def loaderChain(head: JClassLoader) = {
    def loop(cl: JClassLoader): List[JClassLoader] =
      if (cl == null) Nil else cl :: loop(cl.getParent)

    loop(head)
  }
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
    private def classpathString = ClassPath.fromURLs(urls: _*)
    def classPathURLs: Seq[URL] = classloaderURLs
    def classPath: ClassPath[_] = JavaClassPath fromURLs classPathURLs

    /** Override to widen to public */
    override def addURL(url: URL) = {
      classloaderURLs :+= url
      super.addURL(url)
    }
    def toLongString = urls.mkString("URLClassLoader(\n  ", "\n  ", "\n)\n")
  }

  def fromURLs(urls: Seq[URL], parent: ClassLoader = null): URLClassLoader =
    new URLClassLoader(urls, parent)

  /** True if supplied class exists in supplied path */
  def classExists(urls: Seq[URL], name: String): Boolean =
    fromURLs(urls) tryToLoadClass name isDefined

  /** Finding what jar a clazz or instance came from */
  def origin(x: Any): Option[URL] = originOfClass(x.getClass)
  def originOfClass(x: Class[_]): Option[URL] =
    Option(x.getProtectionDomain.getCodeSource) flatMap (x => Option(x.getLocation))
}
