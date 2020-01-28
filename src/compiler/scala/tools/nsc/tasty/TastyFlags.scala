package scala.tools.nsc.tasty

/**Flags from TASTy with no equivalent in scalac
 */
object TastyFlags {

  private[this] val maxFlag = 11

  final val EmptyTastyFlags: TastyFlagSet = TastyFlagSet(0)
  final val Erased: TastyFlagSet          = TastyFlagSet(1 << 0)
  final val Internal: TastyFlagSet        = TastyFlagSet(1 << 1)
  final val Inline: TastyFlagSet          = TastyFlagSet(1 << 2)
  final val InlineProxy: TastyFlagSet     = TastyFlagSet(1 << 3)
  final val Opaque: TastyFlagSet          = TastyFlagSet(1 << 4)
  final val Scala2x: TastyFlagSet         = TastyFlagSet(1 << 5)
  final val Extension: TastyFlagSet       = TastyFlagSet(1 << 6)
  final val Given: TastyFlagSet           = TastyFlagSet(1 << 7)
  final val Exported: TastyFlagSet        = TastyFlagSet(1 << 8)
  final val NoInits: TastyFlagSet         = TastyFlagSet(1 << 9)
  final val TastyMacro: TastyFlagSet      = TastyFlagSet(1 << 10)
  final val Enum: TastyFlagSet            = TastyFlagSet(1 << maxFlag)

  case class TastyFlagSet private[TastyFlags](private val flags: Int) extends AnyVal {
    def toSingletonSets: SingletonSets                        = SingletonSets(flags)
    def |(other: TastyFlagSet): TastyFlagSet                  = TastyFlagSet(flags | other.flags)
    def &(mask: TastyFlagSet): TastyFlagSet                   = TastyFlagSet(flags & mask.flags)
    def &~(mask: TastyFlagSet): TastyFlagSet                  = TastyFlagSet(flags & ~mask.flags)
    def unary_! : Boolean                                     = this.flags == 0
    def is(mask: TastyFlagSet): Boolean                       = (this & mask) == mask
    def isOneOf(mask: TastyFlagSet): Boolean                  = (this & mask).hasFlags
    def is(mask: TastyFlagSet, butNot: TastyFlagSet): Boolean = if (!butNot) is(mask) else is(mask) && not(butNot)
    def not(mask: TastyFlagSet): Boolean                      = !isOneOf(mask)
    def hasFlags: Boolean                                     = this.flags != 0
    def except(mask: TastyFlagSet): (Boolean, TastyFlagSet)   = is(mask) -> (this &~ mask)
  }

  case class SingletonSets private[TastyFlags](private val set: Int) extends AnyVal {
    def map[A](f: TastyFlagSet => A): Iterable[A] = {
      val buf = Iterable.newBuilder[A]
      var i = 0
      while (i <= maxFlag) {
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
