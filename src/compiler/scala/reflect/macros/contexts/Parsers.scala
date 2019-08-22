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

import scala.reflect.internal.Reporter
import scala.reflect.internal.util.FreshNameCreator
import scala.tools.nsc.reporters.StoreReporter, StoreReporter.Info

trait Parsers {
  self: Context =>
  import global._

  def parse(code: String) = {
    val sreporter = new StoreReporter(globalSettings)
    val oldReporter = reporter
    try {
      reporter = sreporter
      val parser = newUnitParser(new CompilationUnit(newSourceFile(code, "<macro>")) {
        override implicit val fresh: FreshNameCreator = currentFreshNameCreator
      })
      val tree = gen.mkTreeOrBlock(parser.parseStatsOrPackages())
      sreporter.infos.foreach {
        case Info(pos, msg, Reporter.ERROR) => throw ParseException(pos, msg)
        case _ =>
      }
      tree
    } finally reporter = oldReporter
  }
}
