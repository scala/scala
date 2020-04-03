package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyName
import scala.tools.nsc.tasty.TastyUniverse

trait NameOps { self: TastyUniverse =>
  import TastyName._

  def isConstructorName(name: Name) = symbolTable.nme.isConstructorName(name)

  private def encodeAsTermName(tastyName: TastyName): TermName = tastyName match {
    case Empty          => termNames.EMPTY
    case Constructor    => nme.CONSTRUCTOR
    case EmptyPkg       => nme.EMPTY_PACKAGE_NAME
    case RootClass      => nme.ROOT
    case WildcardName() => nme.WILDCARD
    case name           => mkTermName(name.encoded)
  }

  private def encodeAsTypeName(tastyName: TastyName): TypeName = tastyName match {
    case RepeatedClass => tpnme.REPEATED_PARAM_CLASS_NAME
    case name          => encodeAsTermName(name).toTypeName
  }

  def encodeTastyNameAsTerm(tastyName: TastyName): TermName = encodeAsTermName(tastyName.stripSignedPart)
  def encodeTastyNameAsType(tastyName: TastyName): TypeName = encodeAsTypeName(tastyName.stripSignedPart)

  def encodeTastyName(tastyName: TastyName, isTerm: Boolean): Name =
    if (isTerm) encodeTastyNameAsTerm(tastyName)
    else encodeTastyNameAsType(tastyName)

}
