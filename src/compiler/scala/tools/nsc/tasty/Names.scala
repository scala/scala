package scala.tools.nsc.tasty
import scala.annotation.tailrec

object Names {

  object TastyName {
    final case class SimpleName(raw: String)                                                extends TastyName
    final case class ModuleName(base: TastyName)                                            extends TastyName
    final case class QualifiedName(qual: TastyName, name: SimpleName)                       extends TastyName
    final case class SignedName(qual: TastyName, sig: Signature.MethodSignature[TastyName]) extends TastyName
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

    /** Get the part of this name that represents a Scala identifier
     */
    @tailrec
    final def identifierPart: SimpleName = self match {
      case name: SimpleName       => name
      case QualifiedName(_, name) => name
      case ModuleName(name)       => name.identifierPart
      case SignedName(name,_)     => name.identifierPart
    }

    /** Get the optional qualifier if this name forms a path
     */
    @tailrec
    final def qualifierPart: Option[TastyName] = self match {
      case name: SimpleName       => None
      case QualifiedName(qual, _) => Some(qual)
      case ModuleName(name)       => name.qualifierPart
      case SignedName(name,_)     => name.qualifierPart
    }

    /** How to display the name in a TASTy file.
     */
    final def debugTasty: String = {
      self match {
        case name: SimpleName => name.raw
        case _ =>
          val sb = new StringBuilder(10)
          def inner(name: TastyName): Unit = name match {
            case name: SimpleName          => sb.append(name.raw)
            case QualifiedName(qual, name) => inner(qual); sb.append("[Qualified . "); inner(name); sb.append(']')
            case ModuleName(name)          => inner(name); sb.append("[ModuleClass]")
            case SignedName(name,sig)      => inner(name); sb.append("[Signed "); sb.append(sig.show); sb.append(']')
          }
          inner(self)
          sb.toString
      }
    }

    final def stripModulePart: TastyName = self match {
      case ModuleName(name)                                       => name
      case name @ (_:SimpleName | _:QualifiedName | _:SignedName) => name
    }

    final def signature: Signature[TastyName] = self match {
      case SignedName(_, signature)                       => signature
      case _:SimpleName | _:QualifiedName |  _:ModuleName => Signature.NotAMethod
    }
  }
}
