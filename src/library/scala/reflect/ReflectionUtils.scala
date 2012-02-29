/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect

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

  def singletonInstance(className: String, cl: ClassLoader = getClass.getClassLoader): AnyRef = {
    val name = if (className endsWith "$") className else className + "$"
    val clazz = java.lang.Class.forName(name, true, cl)
    val singleton = clazz getField "MODULE$" get null
    singleton
  }

  // Retrieves the MODULE$ field for the given class name.
  def singletonInstanceOpt(className: String, cl: ClassLoader = getClass.getClassLoader): Option[AnyRef] =
    try Some(singletonInstance(className, cl))
    catch { case _: ClassNotFoundException  => None }
}
