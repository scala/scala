package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.Names.TastyName

trait NameOps extends TastyKernel {
  object NameOps {
    implicit class NameDecorator(name: Name) {
      def isConstructorName: Boolean = symbolTable.nme.isConstructorName(name)
    }
  }

  implicit class TastyNameDecorator(private val tastyName: TastyName) {
    def toTermName: TermName = tastyName.export match {
      case ""  => termNames.EMPTY
      case raw => mkTermName(raw)
    }
  }
}
