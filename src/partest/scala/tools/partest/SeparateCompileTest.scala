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

package scala.tools.partest

import java.io.File
import scala.tools.nsc._

abstract class SeparateCompileTest extends DirectTest {
  def classesDir(i: Int): File =
    new File(new File(sys.props("partest.output")), s"classes$i")

  // Override this
  def sourcess: List[List[String]]

  def check(global: Global)(source: String, result: Either[Unit, global.CompilationUnit]): Unit =
    result match {
      case Left(_) => sys.error("compilation error")
      case _       => ()
    }

  def show() = 
    sourcess.zipWithIndex foreach { case (sources, idx) =>
      classesDir(idx).mkdirs
      val testOutput = Directory(classesDir(idx))
      val settings = newSettings(
        (if (idx > 0) List("-cp", (0 to idx - 1).toList.map(classesDir).mkString(File.pathSeparator))
        else Nil) :::
        List("-usejavacp", "-d", testOutput.path))
      val global: Global = newCompiler(settings)
      val result = compilationUnitsEither(global)(sources: _*)
      result match {
        case Left(_)      => sources.foreach(check(global)(_, Left(())))
        case Right(units) => sources.lazyZip(units).foreach((s, u) => check(global)(s, Right(u)))
      }
    }

  def compilationUnitsEither(global: Global)(sourceCodes: String*): Either[Unit, List[global.CompilationUnit]] = {
    val units = sourceFilesToCompiledUnits(global)(newSources(sourceCodes: _*))
    if (global.reporter.hasErrors) {
      global.reporter.flush()
      Left(())
    }
    else Right(units)
  }

  override def code: String = ""
}
