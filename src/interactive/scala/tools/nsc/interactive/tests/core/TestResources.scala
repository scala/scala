/* NSC -- new Scala compiler
 * Copyright 2011-2014 LAMP/EPFL
 * @author  Martin Odersky
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