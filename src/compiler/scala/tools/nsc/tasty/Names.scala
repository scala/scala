package scala.tools.nsc.tasty

import scala.annotation.tailrec
import scala.reflect.NameTransformer

object Names {

  object TastyName {

    // TODO tasty: cache chars for Names. SimpleName acts as a cursor

    final case class SimpleName(raw: String)                                                extends TastyName
    final case class ModuleName(base: TastyName)                                            extends TastyName
    final case class QualifiedName(qual: TastyName, sep: SimpleName, selector: SimpleName)  extends TastyName
    final case class SignedName(qual: TastyName, sig: Signature.MethodSignature[TastyName]) extends TastyName
    final case class UniqueName(qual: TastyName, sep: SimpleName, num: Int)                 extends TastyName
    final case class DefaultName(qual: TastyName, num: Int)                                 extends TastyName
    final case class VariantName(qual: TastyName, contravariant: Boolean)                   extends TastyName
    final case class PrefixName(prefix: SimpleName, qual: TastyName)                        extends TastyName

    final val Empty: SimpleName = SimpleName("")
    final val PathSep: SimpleName = SimpleName(".")
    final val ExpandedSep: SimpleName = SimpleName("$$")
    final val ExpandPrefixSep: SimpleName = SimpleName("$")
    final val InlinePrefix: SimpleName = SimpleName("inline$")
    final val SuperPrefix: SimpleName = SimpleName("super$")
    final val Constructor: SimpleName = SimpleName("<init>")

    final val DefaultGetterStr     = "$default$"
    final val DefaultGetterInitStr = NameTransformer.encode("<init>") + DefaultGetterStr

    trait NameEncoder[U] {
      final def encode[O](name: TastyName)(init: => U, finish: U => O): O = finish(traverse(init, name))
      def traverse(u: U, name: TastyName): U
    }

    trait StringBuilderEncoder extends NameEncoder[StringBuilder] {
      final def encode(name: TastyName): String = name match {
        case SimpleName(raw) => raw
        case _               => super.encode(name)(new StringBuilder(25), _.toString)
      }
    }

    /** Converts a name to a representation closest to source code.
     */
    object SourceEncoder extends StringBuilderEncoder {
      def traverse(sb: StringBuilder, name: TastyName): StringBuilder = name match {
        case name: SimpleName    => sb.append(name.raw)
        case name: ModuleName    => traverse(sb, name.base)
        case name: SignedName    => traverse(sb, name.qual)
        case name: UniqueName    => traverse(traverse(sb, name.qual), name.sep).append(name.num)
        case name: DefaultName   => traverse(sb, name.qual).append(DefaultGetterStr).append(name.num + 1)
        case name: VariantName   => traverse(sb.append(if (name.contravariant) '-' else '+'), name.qual)
        case name: QualifiedName => traverse(traverse(traverse(sb, name.qual), name.sep), name.selector)
        case name: PrefixName    => traverse(traverse(sb, name.prefix), name.qual)
      }
    }

    /** Displays formatted information about the structure of the name
     */
    object DebugEncoder extends StringBuilderEncoder {

      def traverse(sb: StringBuilder, name: TastyName): StringBuilder = name match {

        case SimpleName(raw)          => sb.append(raw)
        case DefaultName(qual, num)   => traverse(sb, qual).append("[Default ").append(num + 1).append(']')
        case PrefixName(prefix, qual) => traverse(traverse(sb, qual).append("[Prefix "), prefix).append(']')
        case ModuleName(name)         => traverse(sb, name).append("[ModuleClass]")
        case SignedName(name,sig)     => sig.mergeShow(traverse(sb, name).append("[Signed ")).append(']')

        case VariantName(qual, contra) =>
          traverse(sb, qual).append("[Variant ").append(if (contra) '-' else '+').append(']')

        case QualifiedName(qual, sep, name) =>
          traverse(traverse(traverse(sb, qual).append("[Qualified "), sep).append(' '), name).append(']')

        case UniqueName(qual, sep, num) =>
          traverse(traverse(sb, qual).append("[Unique "), sep).append(' ').append(num).append(']')

      }

    }

    /** Encodes names as expected by the Scala Reflect SymbolTable
     */
    object ScalaNameEncoder extends NameEncoder[StringBuilder] {

      final def encode(name: TastyName): String = name match {
        case SimpleName(raw) => escapeSimple(raw)
        case _               => super.encode(name)(new StringBuilder(25), _.toString)
      }

      final def escapeSimple(raw: String) = raw match {
        case raw @ "<init>" => raw
        case raw            => NameTransformer.encode(raw)
      }

      def traverse(sb: StringBuilder, name: TastyName): StringBuilder = name match {
        case name: SimpleName    => sb.append(escapeSimple(name.raw))
        case name: ModuleName    => traverse(sb, name.base)
        case name: SignedName    => traverse(sb, name.qual)
        case name: UniqueName    => traverse(sb, name.qual).append(name.sep.raw).append(name.num)
        case name: VariantName   => traverse(sb, name.qual)
        case name: QualifiedName => traverse(traverse(sb, name.qual).append(name.sep.raw), name.selector)
        case name: PrefixName    => traverse(sb.append(name.prefix), name.qual)

        case name: DefaultName if name.qual == Constructor => sb.append(DefaultGetterInitStr).append(name.num + 1)

        case name: DefaultName => traverse(sb, name.qual).append(DefaultGetterStr).append(name.num + 1)
      }

    }

  }

  /** class to represent Names as defined in TASTy, with methods to extract scala identifiers
   */
  sealed trait TastyName extends Product with Serializable { self =>
    import TastyName._

    final override def toString: String = source

    final def isModuleName: Boolean = self.isInstanceOf[ModuleName]
    final def isDefaultName: Boolean = self.isInstanceOf[DefaultName]

    final def asSimpleName: SimpleName = self match {
      case self: SimpleName => self
      case _                => throw new AssertionError(s"not simplename: ${self.debug}")
    }

    /** The name as as expected by the Scala Reflect SymbolTable
     */
    final def encoded: String = ScalaNameEncoder.encode(self)

    /** The name as represented in source code
     */
    final def source: String = SourceEncoder.encode(self)

    /** Debug information about the structure of the name.
     */
    final def debug: String = DebugEncoder.encode(self)

    final def stripModulePart: TastyName = self match {
      case ModuleName(name) => name
      case name @ (
        _:SimpleName
      | _:QualifiedName
      | _:SignedName
      | _:UniqueName
      | _:DefaultName
      | _:VariantName
      | _:PrefixName
      ) => name
    }

    final def signature: Signature[TastyName] = self match {
      case SignedName(_, signature) => signature
      case (
        _:SimpleName
      | _:QualifiedName
      | _:ModuleName
      | _:UniqueName
      | _:DefaultName
      | _:VariantName
      | _:PrefixName
      ) => Signature.NotAMethod
    }
  }
}
