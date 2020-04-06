package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyName
import scala.tools.nsc.tasty.TastyUniverse

trait NameOps { self: TastyUniverse =>
  import self.{symbolTable => u}
  import TastyName._

  def isConstructorName(name: Name) = u.nme.isConstructorName(name)

  private def encodeAsTermName(tastyName: TastyName): TermName = tastyName match {
    case Empty          => u.termNames.EMPTY
    case Constructor    => nme.CONSTRUCTOR
    case EmptyPkg       => nme.EMPTY_PACKAGE_NAME
    case RootClass      => nme.ROOT
    case WildcardName() => u.nme.WILDCARD
    case name           => u.TermName(name.encoded)
  }

  private def encodeAsTypeName(tastyName: TastyName): TypeName = tastyName match {
    case RepeatedClass => u.tpnme.REPEATED_PARAM_CLASS_NAME
    case name          => encodeAsTermName(name).toTypeName
  }

  def encodeTastyNameAsTerm(tastyName: TastyName): TermName = encodeAsTermName(tastyName.stripSignedPart)
  def encodeTastyNameAsType(tastyName: TastyName): TypeName = encodeAsTypeName(tastyName.stripSignedPart)

  def encodeTastyName(tastyName: TastyName, isTerm: Boolean): Name =
    if (isTerm) encodeTastyNameAsTerm(tastyName)
    else encodeTastyNameAsType(tastyName)

  object nme {
    final val Or: TastyName.SimpleName = TastyName.SimpleName("|")
    final val And: TastyName.SimpleName = TastyName.SimpleName("&")
    final val EMPTY: TermName = u.nme.EMPTY
    final val CONSTRUCTOR: TermName = u.nme.CONSTRUCTOR
    final val ROOT: TermName = u.nme.ROOT
    final val ROOTPKG: TermName = u.nme.ROOTPKG
    final val EMPTY_PACKAGE_NAME: TermName = u.nme.EMPTY_PACKAGE_NAME
  }

}
