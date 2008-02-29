/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant.sabbus

import java.io.File
import java.net.URL
import java.lang.reflect.InvocationTargetException

class Compiler(classpath: Array[URL], val settings: CompilerSettings) {

  private lazy val classLoader: ClassLoader =
    new java.net.URLClassLoader(classpath, null)

  private lazy val foreignCompilerName: String =
    "scala.tools.ant.sabbus.ForeignCompiler"
  private lazy val foreignCompiler: AnyRef =
    classLoader.loadClass(foreignCompilerName).newInstance.asInstanceOf[AnyRef]

  foreignInvoke("args_$eq", Array(classOf[String]), Array(settings.toArgs))

  private def foreignInvoke(method: String, types: Array[Class[T] forSome { type T }] , args: Array[AnyRef]) =
    try {
      foreignCompiler.getClass.getMethod(method, types).invoke(foreignCompiler, args)
    }
    catch {
      case e: InvocationTargetException => throw e.getCause
    }

  def compile(files: Array[File]): (Int, Int) = //(errors, warnings)
    try {
      val result =
        foreignInvoke("compile", Array(classOf[Array[File]]), Array(files)).asInstanceOf[Int]
      (result >> 16, result & 0x00FF)
    }
    catch {
      case ex: Exception =>
        throw CompilationFailure(ex.getMessage, ex)
    }

}
