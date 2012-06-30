/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.runtime

import java.lang.{Class => jClass}
import java.lang.reflect.{ InvocationTargetException, UndeclaredThrowableException }

/** A few java-reflection oriented utility functions useful during reflection bootstrapping.
 */
object ReflectionUtils {
  // Unwraps some chained exceptions which arise during reflective calls.
  def unwrapThrowable(x: Throwable): Throwable = x match {
    case  _: InvocationTargetException |      // thrown by reflectively invoked method or constructor
          _: ExceptionInInitializerError |    // thrown when running a static initializer (e.g. a scala module constructor)
          _: UndeclaredThrowableException |   // invocation on a proxy instance if its invocation handler's `invoke` throws an exception
          _: ClassNotFoundException |         // no definition for a class instantiated by name
          _: NoClassDefFoundError             // the definition existed when the executing class was compiled, but can no longer be found
            if x.getCause != null =>
              unwrapThrowable(x.getCause)
    case _ => x
  }
  // Transforms an exception handler into one which will only receive the unwrapped
  // exceptions (for the values of wrap covered in unwrapThrowable.)
  def unwrapHandler[T](pf: PartialFunction[Throwable, T]): PartialFunction[Throwable, T] = {
    case ex if pf isDefinedAt unwrapThrowable(ex)   => pf(unwrapThrowable(ex))
  }

  private def systemProperties: Iterator[(String, String)] = {
    import scala.collection.JavaConverters._
    System.getProperties.asScala.iterator
  }

  private def inferBootClasspath: String = (
    systemProperties find (_._1 endsWith ".boot.class.path") map (_._2) getOrElse ""
  )

  def show(cl: ClassLoader): String = {
    def isAbstractFileClassLoader(clazz: Class[_]): Boolean = {
      if (clazz == null) return false
      if (clazz.getName == "scala.tools.nsc.interpreter.AbstractFileClassLoader") return true
      return isAbstractFileClassLoader(clazz.getSuperclass)
    }
    def inferClasspath(cl: ClassLoader): String = cl match {
      case cl: java.net.URLClassLoader =>
        (cl.getURLs mkString ",")
      case cl if cl != null && isAbstractFileClassLoader(cl.getClass) =>
        cl.asInstanceOf[{val root: scala.reflect.internal.AbstractFileApi}].root.canonicalPath
      case null =>
        inferBootClasspath
      case _ =>
        "<unknown>"
    }
    cl match {
      case cl if cl != null =>
        "%s of type %s with classpath [%s] and parent being %s".format(cl, cl.getClass, inferClasspath(cl), show(cl.getParent))
      case null =>
        "primordial classloader with boot classpath [%s]".format(inferClasspath(cl))
    }
  }

  def singletonInstance(cl: ClassLoader, className: String): AnyRef = {
    val name = if (className endsWith "$") className else className + "$"
    val clazz = java.lang.Class.forName(name, true, cl)
    val singleton = clazz getField "MODULE$" get null
    singleton
  }

  // Retrieves the MODULE$ field for the given class name.
  def singletonInstanceOpt(cl: ClassLoader, className: String): Option[AnyRef] =
    try Some(singletonInstance(cl, className))
    catch { case _: ClassNotFoundException  => None }

  def invokeFactory(cl: ClassLoader, className: String, methodName: String, args: AnyRef*): AnyRef = {
    val singleton = singletonInstance(cl, className)
    val method = singleton.getClass.getMethod(methodName, classOf[ClassLoader])
    method.invoke(singleton, args: _*)
  }

  def invokeFactoryOpt(cl: ClassLoader, className: String, methodName: String, args: AnyRef*): Option[AnyRef] =
    try Some(invokeFactory(cl, className, methodName, args: _*))
    catch { case _: ClassNotFoundException  => None }
}
