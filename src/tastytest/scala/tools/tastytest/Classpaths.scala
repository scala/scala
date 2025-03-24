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

package scala.tools.tastytest

import scala.util.Properties
import java.io.File.pathSeparatorChar

object Classpaths {

  private def classpathProp(name: String) =
    Properties.propOrNone(name).map(_.split(pathSeparatorChar).filter(_.nonEmpty).toList).getOrElse(Nil)

  def dottyCompiler: List[String] = classpathProp("tastytest.classpaths.dottyCompiler")

  def scalaReflect: List[String] = classpathProp("tastytest.classpaths.scalaReflect")

  def dottyLibrary: List[String] = classpathProp("tastytest.classpaths.dottyLibrary")

}
