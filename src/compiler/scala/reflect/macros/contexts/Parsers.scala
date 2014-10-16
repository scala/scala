/* NSC -- new Scala compiler
 * Copyright 2012-2014 LAMP/EPFL
 * @author  Martin Odersky
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
      }
      tree
    } finally global.reporter = oldReporter
  }
}