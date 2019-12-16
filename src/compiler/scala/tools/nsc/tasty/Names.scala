package scala.tools.nsc.tasty

import scala.annotation.tailrec

object Names {

  object TastyName {

    // TODO tasty: cache chars for Names. SimpleName acts as a cursor

    final case class SimpleName(raw: String)                                                extends TastyName
    final case class ModuleName(base: TastyName)                                            extends TastyName
    final case class QualifiedName(qual: TastyName, sep: SimpleName, name: SimpleName)      extends TastyName
    final case class SignedName(qual: TastyName, sig: Signature.MethodSignature[TastyName]) extends TastyName
    final case class UniqueName(qual: TastyName, sep: SimpleName, num: Int)                 extends TastyName
    final case class DefaultName(qual: TastyName, num: Int)                                 extends TastyName
    final case class VariantName(qual: TastyName, contravariant: Boolean)                   extends TastyName
    final case class PrefixName(prefix: SimpleName, qual: TastyName)                        extends TastyName

    val Empty: SimpleName = SimpleName("")
    val PathSep: SimpleName = SimpleName(".")
    val ExpandedSep: SimpleName = SimpleName("$$")
    val ExpandPrefixSep: SimpleName = SimpleName("$")
    val InlinePrefix: SimpleName = SimpleName("inline$")
    val SuperPrefix: SimpleName = SimpleName("super$")

  }

  /** class to represent Names as defined in TASTy, with methods to extract scala identifiers
   */
  sealed trait TastyName extends Product with Serializable { self =>
    import TastyName._

    /** Standard representation of this as a [[scala.Product]].
     */
    final def show: String = runtime.ScalaRunTime._toString(self)

    final override def toString: String = debugTasty

    final def isModuleName: Boolean = self.isInstanceOf[ModuleName]

    final def asSimpleName: SimpleName = self match {
      case self: SimpleName => self
      case _                => throw new AssertionError(s"not simplename: ${self.show}")
    }

    final def export: String = {
      self match {
        case SimpleName(raw) => raw
        case _ =>
          val sb = new StringBuilder(10)
          def inner(name: TastyName): Unit = name match {
            case SimpleName(raw)                => sb.append(raw)
            case QualifiedName(qual, sep, name) => inner(qual); inner(sep); inner(name)
            case ModuleName(name)               => inner(name)
            case SignedName(name,_)             => inner(name)
            case UniqueName(qual, sep, num)     => inner(qual); inner(sep); sb.append(num)
            case DefaultName(qual, num)         => inner(qual); sb.append("$default$"); sb.append(num + 1)
            case VariantName(qual, contra)      => sb.append(if (contra) '-' else '+'); inner(qual)
            case PrefixName(prefix, qual)       => inner(prefix); inner(qual)
          }
          inner(self)
          sb.toString
      }
    }

    /** How to display the name in a TASTy file.
     */
    final def debugTasty: String = {
      self match {
        case name: SimpleName => name.raw
        case _ =>
          val sb = new StringBuilder(10)
          def inner(name: TastyName): Unit = name match {
            case name: SimpleName               => sb.append(name.raw)
            case QualifiedName(qual, sep, name) => inner(qual); sb.append("[Qualified "); inner(sep); sb.append(' '); inner(name); sb.append(']')
            case ModuleName(name)               => inner(name); sb.append("[ModuleClass]")
            case SignedName(name,sig)           => inner(name); sb.append("[Signed "); sig.mergeShow(sb).append(']')
            case UniqueName(qual, sep, num)     => inner(qual); sb.append("[Unique "); inner(sep); sb.append(' ').append(num).append(']')
            case DefaultName(qual, num)         => inner(qual); sb.append("[Default "); sb.append(num + 1).append(']')
            case VariantName(qual, contra)      => inner(qual); sb.append("[Variant ").append(if (contra) '-' else '+').append(']')
            case PrefixName(prefix, qual)       => inner(qual); sb.append("[Prefix "); inner(prefix); sb.append(']')
          }
          inner(self)
          sb.toString
      }
    }

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
