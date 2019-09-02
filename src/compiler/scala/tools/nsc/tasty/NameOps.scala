package scala.tools.nsc.tasty

import scala.reflect.internal.SymbolTable
object NameOps {
  implicit def mkNameDecorator(implicit symbolTable: SymbolTable): symbolTable.Name => NameDecorator = name =>
    new NameDecorator(symbolTable)(name.asInstanceOf)

  class NameDecorator(private val symbolTable: SymbolTable)(private val name: symbolTable.Name) {
    def isConstructorName: Boolean = symbolTable.nme.isConstructorName(name)
  }
}
