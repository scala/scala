/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.ant.sabbus

import java.io.File
import java.net.URL
import java.lang.reflect.InvocationTargetException
import scala.reflect.internal.util.ScalaClassLoader

class Compiler(classpath: Array[URL], val settings: Settings)
{
  val foreignCompilerName: String = "scala.tools.ant.sabbus.ForeignCompiler"
  private lazy val classLoader = ScalaClassLoader fromURLs classpath
  private lazy val foreignCompiler: AnyRef = classLoader create foreignCompilerName

  private def settingsArray: Array[String] = settings.toArgs.toArray
  foreignInvoke("args_$eq", Array(classOf[Array[String]]), Array(settingsArray))

  private def foreignInvoke(method: String, types: Array[Class[_]], args: Array[AnyRef]) =
    try foreignCompiler.getClass.getMethod(method, types: _*).invoke(foreignCompiler, args: _*)
    catch {
      case e: InvocationTargetException => throw e.getCause
    }

  def compile(files: Array[File]): (Int, Int) = //(errors, warnings)
    try {
      foreignInvoke("args_$eq", Array(classOf[Array[String]]), Array(settingsArray))
      val result =
        foreignInvoke("compile", Array(classOf[Array[File]]), Array(files)).asInstanceOf[Int]
      (result >> 16, result & 0x00FF)
    }
    catch {
      case ex: Exception => throw CompilationFailure(ex.getMessage, ex)
    }
}
