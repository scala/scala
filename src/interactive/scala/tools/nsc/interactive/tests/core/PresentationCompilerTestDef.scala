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

import scala.reflect.internal.util.Position

trait PresentationCompilerTestDef {

  private[tests] def runTest(): Unit

  protected def withResponseDelimiter(block: => Unit)(implicit reporter: Reporter): Unit = {
    def printDelimiter() = reporter.println("=" * 80)
    printDelimiter()
    block
    printDelimiter()
  }

  protected def format(pos: Position): String =
    (if(pos.isDefined) "(%d,%d)".format(pos.line, pos.column) else "<no position>")
}
