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

/** Common settings for the test. */
private[tests] trait TestSettings {
  protected final val TIMEOUT = 30000 // timeout in milliseconds

  /** The root directory for this test suite, usually the test kind ("test/files/presentation"). */
  protected val outDir = Path(System.getProperty("partest.cwd", "."))

  /** The base directory for this test, usually a subdirectory of "test/files/presentation/" */
  protected val baseDir = Option(System.getProperty("partest.testname")).map(outDir / _).getOrElse(Path("."))

  /** Where source files are placed. */
  protected val sourceDir = "src"

  protected implicit val reporter: Reporter = ConsoleReporter
}
