package scala.tools.nsc.tasty

import scala.reflect.internal.SymbolTable

trait TastyUniverse { self =>
  val symbolTable: SymbolTable

  final implicit val symbolTablePrecise: self.symbolTable.type = self.symbolTable

  final def logTasty(str: => String): Unit = {
    import symbolTable._
    if (settings.debugTasty) reporter.echo(NoPosition, str)
  }
}
