package scala.tools.nsc.tasty

/**Flags from TASTy with no equivalent in scalac
 */
object TastyFlags {

  val EmptyFlags: TastyFlagSet  = TastyFlagSet(0)
  val Erased: TastyFlagSet      = TastyFlagSet(1 << 0)
  val Internal: TastyFlagSet    = TastyFlagSet(1 << 1)
  val Inline: TastyFlagSet      = TastyFlagSet(1 << 2)
  val InlineProxy: TastyFlagSet = TastyFlagSet(1 << 3)
  val Opaque: TastyFlagSet      = TastyFlagSet(1 << 4)
  val Scala2x: TastyFlagSet     = TastyFlagSet(1 << 5)
  val Extension: TastyFlagSet   = TastyFlagSet(1 << 6)
  val Given: TastyFlagSet       = TastyFlagSet(1 << 7)
  val Exported: TastyFlagSet    = TastyFlagSet(1 << 8)
  val NoInits: TastyFlagSet     = TastyFlagSet(1 << 9)

  case class TastyFlagSet private[TastyFlags](private val flags: Int) extends AnyVal {
    def toSingletonSets: SingletonSets                        = SingletonSets(flags)
    def |(other: TastyFlagSet): TastyFlagSet                  = TastyFlagSet(flags | other.flags)
    def &(mask: TastyFlagSet): TastyFlagSet                   = TastyFlagSet(flags & mask.flags)
    def &~(mask: TastyFlagSet): TastyFlagSet                  = TastyFlagSet(flags & ~mask.flags)
    def unary_! : Boolean                                     = this == EmptyFlags
    def is(mask: TastyFlagSet): Boolean                       = (this & mask).hasFlags
    def is(mask: TastyFlagSet, butNot: TastyFlagSet): Boolean = if (!butNot) is(mask) else is(mask) && not(butNot)
    def not(mask: TastyFlagSet): Boolean                      = !is(mask)
    def hasFlags: Boolean                                     = !(!this)
    def except(mask: TastyFlagSet): (Boolean, TastyFlagSet)   = is(mask) -> (this &~ mask)
  }

  case class SingletonSets private[TastyFlags](private val set: Int) extends AnyVal {
    def map[A](f: TastyFlagSet => A): Iterable[A] = {
      val buf = Iterable.newBuilder[A]
      var i = 0
      while (i <= 9) {
        val flag = 1 << i
        if ((flag & set) != 0) {
          buf += f(TastyFlagSet(flag))
        }
        i += 1
      }
      buf.result
    }
  }

}
