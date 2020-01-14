package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.Names.TastyName
import scala.tools.nsc.tasty.TastyUniverse

trait NameOps extends TastyKernel { self: TastyUniverse =>
  import TastyName._

  object NameOps {
    implicit class NameDecorator(name: Name) {
      def isConstructorName: Boolean = symbolTable.nme.isConstructorName(name)
    }
  }

  implicit class TastyNameDecorator(private val tastyName: TastyName) {
    def toEncodedTermName: TermName = tastyName match {
      case Empty       => termNames.EMPTY
      case Constructor => nme.CONSTRUCTOR
      case EmptyPkg    => nme.EMPTY_PACKAGE_NAME
      case RootClass   => nme.ROOT
      case name        => mkTermName(name.encoded)
    }
  }

}
