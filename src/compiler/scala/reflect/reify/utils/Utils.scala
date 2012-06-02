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

  val reifyDebug = global.settings.Yreifydebug.value
  val reifyCopypaste = global.settings.Yreifycopypaste.value
  val reifyTrace = scala.tools.nsc.util.trace when reifyDebug
}