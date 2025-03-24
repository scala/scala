/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect.internal.util

import java.lang.invoke.{MethodHandles, MethodType}

import scala.language.implicitConversions
import java.lang.{ClassLoader => JClassLoader}
import java.lang.reflect.Modifier
import java.net.{URLClassLoader => JURLClassLoader}
import java.net.URL

import scala.reflect.runtime.ReflectionUtils.{show, unwrapHandler}
import scala.util.control.Exception.catching
import scala.reflect.{ClassTag, classTag}

trait HasClassPath {
  def classPathURLs: Seq[URL]
}

final class RichClassLoader(private val self: JClassLoader) extends AnyVal {
  /** Executing an action with this classloader as context classloader */
  def asContext[T](action: => T): T = {
    val saved = Thread.currentThread.getContextClassLoader
    try { ScalaClassLoader.setContext(self) ; action }
    finally ScalaClassLoader.setContext(saved)
  }

  /** Load and link a class with this classloader */
  def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = false)
  /** Load, link and initialize a class with this classloader */
  def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] = tryClass(path, initialize = true)

  private def tryClass[T <: AnyRef](path: String, initialize: Boolean): Option[Class[T]] =
    catching(classOf[ClassFormatError], classOf[ClassNotFoundException], classOf[SecurityException]) opt
      Class.forName(path, initialize, self).asInstanceOf[Class[T]]

  /** Create an instance of a class with this classloader */
  def create(path: String): AnyRef =
    tryToInitializeClass[AnyRef](path).map(_.getConstructor().newInstance()).orNull

  /** Create an instance with ctor args, or invoke errorFn before throwing. */
  def create[T <: AnyRef : ClassTag](path: String, errorFn: String => Unit)(args: Any*): T = {
    def fail(msg: String) = error(msg, new IllegalArgumentException(msg))
    def error(msg: String, e: Throwable) = { errorFn(msg); throw e }
    try {
      val clazz = Class.forName(path, /*initialize =*/ true, /*loader =*/ self)
      if (classTag[T].runtimeClass.isAssignableFrom(clazz)) {
        val ctor = {
          val bySize = clazz.getConstructors.filter(_.getParameterCount == args.size)
          if (bySize.isEmpty) fail(s"No constructor takes ${args.size} parameters.")
          def isAssignable(k: Class[?], a: Any): Boolean =
            if (k == classOf[Int]) a.isInstanceOf[Integer]
            else if (k == classOf[Boolean]) a.isInstanceOf[java.lang.Boolean]
            else if (k == classOf[Long]) a.isInstanceOf[java.lang.Long]
            else k.isAssignableFrom(a.getClass)
          val maybes = bySize.filter(c => c.getParameterTypes.zip(args).forall { case (k, a) => isAssignable(k, a) })
          if (maybes.size == 1) maybes.head
          else if (bySize.size == 1)
            fail(s"One constructor takes ${args.size} parameters but ${
              bySize.head.getParameterTypes.zip(args).collect { case (k, a) if !isAssignable(k, a) => s"$k != ${a.getClass}" }.mkString("; ")
            }.")
          else
            fail(s"Constructor must accept arg list (${args.map(_.getClass.getName).mkString(", ")}): ${path}")
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
  def classAsStream(className: String) = self.getResourceAsStream {
    if (className endsWith ".class") className
    else s"${className.replace('.', '/')}.class"  // classNameToPath
  }

  /** Run the main method of a class to be loaded by this classloader */
  def run(objectName: String, arguments: Seq[String]): Unit = {
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

object RichClassLoader {
  implicit def wrapClassLoader(loader: ClassLoader): RichClassLoader = new RichClassLoader(loader)
}

/** A wrapper around java.lang.ClassLoader to lower the annoyance
 *  of java reflection.
 */
trait ScalaClassLoader extends JClassLoader {
  private def wrap = new RichClassLoader(this)
  /** Executing an action with this classloader as context classloader */
  def asContext[T](action: => T): T = wrap.asContext(action)

  /** Load and link a class with this classloader */
  def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] = wrap.tryToLoadClass[T](path)
  /** Load, link and initialize a class with this classloader */
  def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] = wrap.tryToInitializeClass(path)

  /** Create an instance of a class with this classloader */
  def create(path: String): AnyRef = wrap.create(path)

  /** Create an instance with ctor args, or invoke errorFn before throwing. */
  def create[T <: AnyRef : ClassTag](path: String, errorFn: String => Unit)(args: AnyRef*): T =
    wrap.create[T](path, errorFn)(args: _*)

  /** The actual bytes for a class file, or an empty array if it can't be found. */
  def classBytes(className: String): Array[Byte] = wrap.classBytes(className)

  /** An InputStream representing the given class name, or null if not found. */
  def classAsStream(className: String) = wrap.classAsStream(className)

  /** Run the main method of a class to be loaded by this classloader */
  def run(objectName: String, arguments: Seq[String]): Unit = wrap.run(objectName, arguments)
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
  def setContext(cl: JClassLoader) = Thread.currentThread.setContextClassLoader(cl)

  class URLClassLoader(urls: Seq[URL], parent: JClassLoader)
      extends JURLClassLoader(urls.toArray, parent)
         with ScalaClassLoader
         with HasClassPath {
    private[this] var classloaderURLs: Seq[URL] = urls
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

  def fromURLs(urls: Seq[URL], parent: ClassLoader = null): URLClassLoader = {
    new URLClassLoader(urls, if (parent == null) bootClassLoader else parent)
  }

  def fromURLsParallelCapable(urls: Seq[URL], parent: ClassLoader = null): JURLClassLoader = {
    new JURLClassLoader(urls.toArray, if (parent == null) bootClassLoader else parent)
  }

  /** True if supplied class exists in supplied path */
  def classExists(urls: Seq[URL], name: String): Boolean =
    (fromURLs(urls) tryToLoadClass name).isDefined

  /** Finding what jar a clazz or instance came from */
  def originOfClass(x: Class[_]): Option[URL] =
    Option(x.getProtectionDomain.getCodeSource) flatMap (x => Option(x.getLocation))

  private[this] val bootClassLoader: ClassLoader = {
    if (!util.Properties.isJavaAtLeast("9")) null
    else {
      try {
        MethodHandles.lookup().findStatic(classOf[ClassLoader], "getPlatformClassLoader", MethodType.methodType(classOf[ClassLoader])).invoke()
      } catch {
        case _: Throwable =>
          null
      }
    }


  }
}
