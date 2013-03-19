package scala.reflect.reify
package utils

import scala.tools.nsc.Global

trait Utils extends NodePrinters
               with Extractors
               with SymbolTables
               with StdAttachments {

  val global: Global
  val typer: global.analyzer.Typer

  lazy val reifier: Reifier { val global: Utils.this.global.type } = getReifier
  def getReifier: Reifier { val global: Utils.this.global.type } = ???
  def hasReifier = false

  val reifyDebug: Boolean     = global.settings.Yreifydebug
  val reifyCopypaste: Boolean = global.settings.Yreifycopypaste
  val reifyTrace = scala.tools.nsc.util.trace when reifyDebug
}
