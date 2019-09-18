package scala.tools.nsc.tasty

/**Flags from TASTy with no equivalent in scalac
 */
trait TASTYFlags { self =>
  type TASTYFlagSet
  type SingletonSets[_]

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
  val NoInits: TASTYFlagSet

  protected def union(as: TASTYFlagSet, bs: TASTYFlagSet): TASTYFlagSet
  protected def intersect(as: TASTYFlagSet, bs: TASTYFlagSet): TASTYFlagSet
  protected def equal(set: TASTYFlagSet, other: TASTYFlagSet): Boolean
  protected def remove(set: TASTYFlagSet, mask: TASTYFlagSet): TASTYFlagSet
  protected def toSingletonSets(multiset: TASTYFlagSet): SingletonSets[TASTYFlagSet]
  protected def map[A](set: SingletonSets[TASTYFlagSet], f: TASTYFlagSet => A): Iterable[A]
}

object TASTYFlags {

  type TASTYFlagSet     = Live.TASTYFlagSet
  type SingletonSets[T] = Live.SingletonSets[T]

  implicit final class TASTYFlagSetAPI(private val flagset: TASTYFlagSet) extends AnyVal {

    final def toSingletonSets: SingletonSets[TASTYFlagSet]          = Live.toSingletonSets(flagset)
    final def |(other: TASTYFlagSet): TASTYFlagSet                  = Live.union(flagset, other)
    final def &(mask: TASTYFlagSet): TASTYFlagSet                   = Live.intersect(flagset, mask)
    final def ===(set: TASTYFlagSet): Boolean                       = Live.equal(flagset, set)
    final def &~(mask: TASTYFlagSet): TASTYFlagSet                  = Live.remove(flagset, mask)
    final def isEmpty: Boolean                                      = flagset === Live.EmptyTASTYFlagSet
    final def is(mask: TASTYFlagSet): Boolean                       = (flagset & mask).nonEmpty
    final def is(mask: TASTYFlagSet, butNot: TASTYFlagSet): Boolean = is(mask) && not(butNot)
    final def not(mask: TASTYFlagSet): Boolean                      = !is(mask)
    final def nonEmpty: Boolean                                     = !isEmpty
    final def except(mask: TASTYFlagSet): (Boolean, TASTYFlagSet)   = (is(mask), flagset &~ mask)

    final def show: String = {
      import Live._
      if (flagset.isEmpty) "EmptyTASTYFlagSet"
      else flagset.toSingletonSets.map {
        case f if f === Erased      => "Erased"
        case f if f === Internal    => "Internal"
        case f if f === Inline      => "Inline"
        case f if f === InlineProxy => "InlineProxy"
        case f if f === Opaque      => "Opaque"
        case f if f === Scala2x     => "Scala2x"
        case f if f === Extension   => "Extension"
        case f if f === Given       => "Given"
        case f if f === Exported    => "Exported"
        case f if f === NoInits     => "NoInits"
      }.mkString(" | ")
    }

  }

  implicit final class SingletonSetsAPI(private val flagsets: SingletonSets[TASTYFlagSet]) extends AnyVal {
    final def map[A](f: TASTYFlagSet => A): Iterable[A] = Live.map(flagsets, f)
  }

  val Live: TASTYFlags = new TASTYFlags {

    type TASTYFlagSet = Int
    type SingletonSets[X] = X

    val EmptyTASTYFlagSet = 0
    val Erased            = 1 << 0
    val Internal          = 1 << 1
    val Inline            = 1 << 2
    val InlineProxy       = 1 << 3
    val Opaque            = 1 << 4
    val Scala2x           = 1 << 5
    val Extension         = 1 << 6
    val Given             = 1 << 7
    val Exported          = 1 << 8
    val NoInits           = 1 << 9

    final def union(a: TASTYFlagSet, b: TASTYFlagSet)       = a | b
    final def intersect(a: TASTYFlagSet, b: TASTYFlagSet)   = a & b
    final def equal(set: TASTYFlagSet, other: TASTYFlagSet) = set == other
    final def remove(set: TASTYFlagSet, mask: TASTYFlagSet) = set & ~mask
    final def toSingletonSets(set: TASTYFlagSet)            = set

    final def map[A](set: SingletonSets[TASTYFlagSet], f: TASTYFlagSet => A) = {
      val buf = Iterable.newBuilder[A]
      var i = 0
      while (i <= 9) {
        val flag = 1 << i
        if ((flag & set) != 0) {
          buf += f(flag)
        }
        i += 1
      }
      buf.result
    }
  }
}
