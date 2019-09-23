package scala.tools.nsc.tasty

/**Flags from TASTy with no equivalent in scalac
 */
trait TastyFlags { self =>
  type TastyFlagSet
  type SingletonSets[_]

  val EmptyFlags: TastyFlagSet
  val Erased: TastyFlagSet
  val Internal: TastyFlagSet
  val Inline: TastyFlagSet
  val InlineProxy: TastyFlagSet
  val Opaque: TastyFlagSet
  val Scala2x: TastyFlagSet
  val Extension: TastyFlagSet
  val Given: TastyFlagSet
  val Exported: TastyFlagSet
  val NoInits: TastyFlagSet

  protected def union(as: TastyFlagSet, bs: TastyFlagSet): TastyFlagSet
  protected def intersect(as: TastyFlagSet, bs: TastyFlagSet): TastyFlagSet
  protected def equal(set: TastyFlagSet, other: TastyFlagSet): Boolean
  protected def remove(set: TastyFlagSet, mask: TastyFlagSet): TastyFlagSet
  protected def toSingletonSets(multiset: TastyFlagSet): SingletonSets[TastyFlagSet]
  protected def map[A](set: SingletonSets[TastyFlagSet], f: TastyFlagSet => A): Iterable[A]
}

object TastyFlags {

  type TastyFlagSet     = Live.TastyFlagSet
  type SingletonSets[T] = Live.SingletonSets[T]

  implicit final class TastyFlagSetAPI(private val flagset: TastyFlagSet) extends AnyVal {

    final def toSingletonSets: SingletonSets[TastyFlagSet]          = Live.toSingletonSets(flagset)
    final def |(other: TastyFlagSet): TastyFlagSet                  = Live.union(flagset, other)
    final def &(mask: TastyFlagSet): TastyFlagSet                   = Live.intersect(flagset, mask)
    final def ===(set: TastyFlagSet): Boolean                       = Live.equal(flagset, set)
    final def &~(mask: TastyFlagSet): TastyFlagSet                  = Live.remove(flagset, mask)
    final def unary_! : Boolean                                     = flagset === Live.EmptyFlags
    final def is(mask: TastyFlagSet): Boolean                       = (flagset & mask).hasFlags
    final def is(mask: TastyFlagSet, butNot: TastyFlagSet): Boolean = if (!butNot) is(mask) else is(mask) && not(butNot)
    final def not(mask: TastyFlagSet): Boolean                      = !is(mask)
    final def hasFlags: Boolean                                     = !(!flagset)
    final def except(mask: TastyFlagSet): (Boolean, TastyFlagSet)   = (is(mask), flagset &~ mask)

    final def show: String = {
      import Live._
      if (!flagset) "EmptyFlags"
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

  implicit final class SingletonSetsAPI(private val flagsets: SingletonSets[TastyFlagSet]) extends AnyVal {
    final def map[A](f: TastyFlagSet => A): Iterable[A] = Live.map(flagsets, f)
  }

  val Live: TastyFlags = new TastyFlags {

    type TastyFlagSet = Int
    type SingletonSets[X] = X

    val EmptyFlags = 0
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

    final def union(a: TastyFlagSet, b: TastyFlagSet)       = a | b
    final def intersect(a: TastyFlagSet, b: TastyFlagSet)   = a & b
    final def equal(set: TastyFlagSet, other: TastyFlagSet) = set == other
    final def remove(set: TastyFlagSet, mask: TastyFlagSet) = set & ~mask
    final def toSingletonSets(set: TastyFlagSet)            = set

    final def map[A](set: SingletonSets[TastyFlagSet], f: TastyFlagSet => A) = {
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
