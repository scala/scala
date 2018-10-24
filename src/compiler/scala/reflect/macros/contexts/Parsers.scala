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

package scala.reflect.macros
package contexts

import scala.tools.nsc.reporters.StoreReporter

trait Parsers {
  self: Context =>
  import global._

  def parse(code: String) = {
    val sreporter = new StoreReporter()
    val oldReporter = global.reporter
    try {
      global.reporter = sreporter
      val parser = newUnitParser(new CompilationUnit(newSourceFile(code, "<macro>")))
      val tree = gen.mkTreeOrBlock(parser.parseStatsOrPackages())
      sreporter.infos.foreach {
        case sreporter.Info(pos, msg, sreporter.ERROR) => throw ParseException(pos, msg)
        case _ =>
      }
      tree
    } finally global.reporter = oldReporter
  }
}
