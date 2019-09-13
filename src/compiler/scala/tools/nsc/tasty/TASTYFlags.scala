package scala.tools.nsc.tasty
import scala.collection.immutable.BitSet

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

  def or(as: TASTYFlagSet, bs: TASTYFlagSet): TASTYFlagSet 
  def has(set: TASTYFlagSet, mask: TASTYFlagSet): Boolean

  implicit class TASTYFlagSetOps(private val flagset: TASTYFlagSet) {
    @inline def | (other: TASTYFlagSet): TASTYFlagSet = self.or(flagset, other)
    @inline def has (mask: TASTYFlagSet): Boolean     = self.has(flagset, mask)
  }
}

object TASTYFlags {
  val Live: TASTYFlags = new TASTYFlags {
    type TASTYFlagSet = BitSet

    val EmptyTASTYFlagSet: TASTYFlagSet = BitSet.empty

    val Erased: TASTYFlagSet       = EmptyTASTYFlagSet + 0
    val Internal: TASTYFlagSet     = EmptyTASTYFlagSet + 1
    val Inline: TASTYFlagSet       = EmptyTASTYFlagSet + 2
    val InlineProxy: TASTYFlagSet  = EmptyTASTYFlagSet + 3
    val Opaque: TASTYFlagSet       = EmptyTASTYFlagSet + 4
    val Scala2x: TASTYFlagSet      = EmptyTASTYFlagSet + 5
    val Extension: TASTYFlagSet    = EmptyTASTYFlagSet + 6
    val Given: TASTYFlagSet        = EmptyTASTYFlagSet + 7
    val Exported: TASTYFlagSet     = EmptyTASTYFlagSet + 8

    def or(a: TASTYFlagSet, b: TASTYFlagSet): TASTYFlagSet  = a union b
    def has(set: TASTYFlagSet, mask: TASTYFlagSet): Boolean = (set diff mask).nonEmpty
  }
}