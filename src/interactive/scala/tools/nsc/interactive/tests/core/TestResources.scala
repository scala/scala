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

package scala.tools.nsc.interactive.tests.core

import scala.tools.nsc.io.Path
import scala.reflect.internal.util.SourceFile

/** Resources used by the test. */
private[tests] trait TestResources extends TestSettings {
  /** collected source files that are to be used by the test runner */
  protected lazy val sourceFiles: Array[SourceFile] = SourcesCollector(baseDir / sourceDir, isScalaOrJavaSource)

  private def isScalaOrJavaSource(file: Path): Boolean = file.extension == "scala" | file.extension == "java"
}
