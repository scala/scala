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
    case RootClass      => u.nme.ROOT
    case WildcardName() => u.nme.WILDCARD
    case name           => u.TermName(name.encoded)
  }

  private def encodeAsTypeName(tastyName: TastyName): u.TypeName = tastyName match {
    case RepeatedClass => u.tpnme.REPEATED_PARAM_CLASS_NAME
    case name          => encodeAsTermName(name).toTypeName
  }

  def encodeTypeName(name: TypeName): u.TypeName = encodeAsTypeName(name.toTermName.stripSignedPart)
  def encodeTermName(name: TastyName): u.TermName = encodeAsTermName(name.toTermName.stripSignedPart)

  def encodeTastyName(name: TastyName): u.Name = name match {
    case name: TypeName => encodeTypeName(name)
    case name           => encodeTermName(name)
  }

  object nme {
    final val Or: TastyName.SimpleName = TastyName.SimpleName("|")
    final val And: TastyName.SimpleName = TastyName.SimpleName("&")
  }

}
