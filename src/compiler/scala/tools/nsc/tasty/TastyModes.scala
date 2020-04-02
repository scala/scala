package scala.tools.nsc.tasty

/**Flags To Control traversal of Tasty
 */
object TastyModes {

  final val EmptyTastyMode: TastyMode = TastyMode(0)
  final val InParents: TastyMode      = TastyMode(1 << 0)

  case class TastyMode(val toInt: Int) extends AnyVal {

    def |(other: TastyMode): TastyMode = TastyMode(toInt | other.toInt)
    def &(mask: TastyMode): TastyMode  = TastyMode(toInt & mask.toInt)
    def is(mask: TastyMode): Boolean   = (this & mask) == mask

  }

}
