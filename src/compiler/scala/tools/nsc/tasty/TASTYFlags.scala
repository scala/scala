package scala.tools.nsc.tasty
import scala.collection.immutable.BitSet
import scala.collection.mutable

/**Flags from TASTy with no equivalent in scalac
 */
trait TASTYFlags { self =>
  type TASTYFlagSet

  val EmptyTASTYFlagSet: TASTYFlagSet

  val Erased: TASTYFlagSet
  val Internal: TASTYFlagSet
  val Inline: TASTYFlagSet
  val InlineProxy: TASTYFlagSet
  val Opaque: TASTYFlagSet
  val Scala2x: TASTYFlagSet
  val Extension: TASTYFlagSet
  val Given: TASTYFlagSet
  val Exported: TASTYFlagSet

  def map[A](f: TASTYFlagSet => A)(as: TASTYFlagSet): Iterable[A]
  def union(as: TASTYFlagSet, bs: TASTYFlagSet): TASTYFlagSet
  def has(set: TASTYFlagSet, mask: TASTYFlagSet): Boolean
  def remove(set: TASTYFlagSet, mask: TASTYFlagSet): TASTYFlagSet

  final def showTASTY(set: TASTYFlagSet): String = set match {
    case EmptyTASTYFlagSet => "Empty"
    case Erased            => "Erased"
    case Internal          => "Internal"
    case Inline            => "Inline"
    case InlineProxy       => "InlineProxy"
    case Opaque            => "Opaque"
    case Scala2x           => "Scala2x"
    case Extension         => "Extension"
    case Given             => "Given"
    case Exported          => "Exported"
    case multi             => map(showTASTY)(multi).mkString(" | ")
  }

  implicit final class TASTYFlagSetOps(private val flagset: TASTYFlagSet) {
    @inline final def map[A](f: TASTYFlagSet => A): Iterable[A] = self.map(f)(flagset)
    @inline final def | (other: TASTYFlagSet): TASTYFlagSet     = self.union(flagset, other)
    @inline final def has (mask: TASTYFlagSet): Boolean         = self.has(flagset, mask)
    @inline final def &~ (mask: TASTYFlagSet): TASTYFlagSet     = self.remove(flagset, mask)
    @inline final def isEmpty: Boolean                          = flagset == EmptyTASTYFlagSet
  }
}

object TASTYFlags {
  val Live: TASTYFlags = new TASTYFlags {

    type TASTYFlagSet = BitSet

    private[this] val cache = mutable.ArrayBuffer.empty[TASTYFlagSet]

    val EmptyTASTYFlagSet = BitSet.empty

    val Erased       = register()
    val Internal     = register()
    val Inline       = register()
    val InlineProxy  = register()
    val Opaque       = register()
    val Scala2x      = register()
    val Extension    = register()
    val Given        = register()
    val Exported     = register()

    cache.trimToSize()

    @inline final def map[A](f: TASTYFlagSet => A)(as: TASTYFlagSet) = as.view.map(cache andThen f)
    @inline final def union(a: TASTYFlagSet, b: TASTYFlagSet)        = a concat b
    @inline final def has(set: TASTYFlagSet, mask: TASTYFlagSet)     = (set intersect mask).nonEmpty
    @inline final def remove(set: TASTYFlagSet, mask: TASTYFlagSet)  = set diff mask

    private[this] def register(): TASTYFlagSet = {
      val set = EmptyTASTYFlagSet + cache.size
      cache += set
      set
    }
  }
}