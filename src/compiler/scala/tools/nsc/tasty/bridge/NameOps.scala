package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyName
import scala.tools.nsc.tasty.TastyUniverse

trait NameOps { self: TastyUniverse =>
  import self.{symbolTable => u}
  import TastyName._

  def isConstructorName(name: TastyName) = name == TastyName.Constructor || name == TastyName.MixinConstructor // u.nme.isConstructorName(encodeTastyName(name))

  private def encodeAsTermName(tastyName: TastyName): u.TermName = tastyName match {
    case Empty          => u.termNames.EMPTY
    case Constructor    => u.nme.CONSTRUCTOR
    case EmptyPkg       => u.nme.EMPTY_PACKAGE_NAME
    case Root           => u.nme.ROOT
    case WildcardName() => u.nme.WILDCARD
    case name           => u.TermName(name.encoded)
  }

  private def encodeAsTypeName(tastyName: TypeName): u.TypeName = tastyName match {
    case RepeatedClass => u.tpnme.REPEATED_PARAM_CLASS_NAME
    case name          => encodeAsTermName(name.toTermName).toTypeName
  }

  def encodeTypeName(name: TypeName): u.TypeName = encodeAsTypeName(name)
  def encodeTermName(name: TastyName): u.TermName = encodeAsTermName(name.stripSignedPart)

  def encodeTastyName(name: TastyName): u.Name = name match {
    case name: TypeName => encodeTypeName(name)
    case name           => encodeTermName(name)
  }

  object tpnme {
    final val Or: TypeName = TastyName.SimpleName("|").toTypeName
    final val And: TypeName = TastyName.SimpleName("&").toTypeName
  }

}
