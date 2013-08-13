/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.runtime

import java.lang.{Class => jClass}
import java.lang.reflect.{ Method, InvocationTargetException, UndeclaredThrowableException }

/** A few java-reflection oriented utility functions useful during reflection bootstrapping.
 */
private[scala] object ReflectionUtils {
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

  def show(cl: ClassLoader): String = {
    import scala.language.reflectiveCalls

    def isAbstractFileClassLoader(clazz: Class[_]): Boolean = {
      if (clazz == null) return false
      if (clazz.getName == "scala.tools.nsc.interpreter.AbstractFileClassLoader") return true
      return isAbstractFileClassLoader(clazz.getSuperclass)
    }
    def inferClasspath(cl: ClassLoader): String = cl match {
      case cl: java.net.URLClassLoader =>
        (cl.getURLs mkString ",")
      case cl if cl != null && isAbstractFileClassLoader(cl.getClass) =>
        cl.asInstanceOf[{val root: scala.reflect.io.AbstractFile}].root.canonicalPath
      case null =>
        val loadBootCp = (flavor: String) => scala.util.Properties.propOrNone(flavor + ".boot.class.path")
        loadBootCp("sun") orElse loadBootCp("java") getOrElse "<unknown>"
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

  def staticSingletonInstance(cl: ClassLoader, className: String): AnyRef = {
    val name = if (className endsWith "$") className else className + "$"
    val clazz = java.lang.Class.forName(name, true, cl)
    staticSingletonInstance(clazz)
  }

  def staticSingletonInstance(clazz: Class[_]): AnyRef = clazz getField "MODULE$" get null

  def innerSingletonInstance(outer: AnyRef, className: String): AnyRef = {
    val accessorName = if (className endsWith "$") className.substring(0, className.length - 1) else className
    def singletonAccessor(clazz: Class[_]): Option[Method] =
      if (clazz == null) None
      else {
        val declaredAccessor = clazz.getDeclaredMethods.filter(_.getName == accessorName).headOption
        declaredAccessor orElse singletonAccessor(clazz.getSuperclass)
      }

    val accessor = singletonAccessor(outer.getClass) getOrElse { throw new NoSuchMethodException(s"${outer.getClass.getName}.$accessorName") }
    accessor setAccessible true
    accessor invoke outer
  }

  def isTraitImplementation(fileName: String) = fileName endsWith "$class.class"

  def scalacShouldntLoadClassfile(fileName: String) = isTraitImplementation(fileName)

  def scalacShouldntLoadClass(name: scala.reflect.internal.SymbolTable#Name) = scalacShouldntLoadClassfile(name + ".class")
}
