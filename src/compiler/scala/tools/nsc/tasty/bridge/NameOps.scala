package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.Names.TastyName
import scala.tools.nsc.tasty.TastyUniverse

trait NameOps { self: TastyUniverse =>
  import TastyName._

  def isConstructorName(name: Name) = symbolTable.nme.isConstructorName(name)

  def encodeAsTermName(tastyName: TastyName): TermName = tastyName match {
    case Empty          => termNames.EMPTY
    case Constructor    => nme.CONSTRUCTOR
    case EmptyPkg       => nme.EMPTY_PACKAGE_NAME
    case RootClass      => nme.ROOT
    case WildcardName() => nme.WILDCARD
    case name           => mkTermName(name.encoded)
  }

  def encodeTastyName(tastyName: TastyName, isTerm: Boolean): Name = {
    val encoded = encodeAsTermName(tastyName)
    if (isTerm) encoded else encoded.toTypeName
  }

}
