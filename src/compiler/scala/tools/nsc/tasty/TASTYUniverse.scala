package scala.tools.nsc.tasty

import scala.reflect.internal.SymbolTable
import scala.tools.nsc.tasty.TastyBuffer.NameRef

trait TASTYUniverse { self =>
  val symbolTable: SymbolTable
  final implicit val symbolTablePrecise: self.symbolTable.type = self.symbolTable
}

trait TASTYNameTable { self: TASTYUniverse =>
  val nameAtRef: NameRef => self.symbolTable.TermName
}
