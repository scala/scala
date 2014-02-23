import scala.language.experimental.macros
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}

object Module {
  type BBC = BlackboxContext
  type RBBC = BBC { type PrefixType = C }
  type WBC = WhiteboxContext
  type RWBC = WBC { type PrefixType = C }

  class BlackboxBundle(val c: BBC) {
    import c.universe._
    def impl = q"${c.prefix}"
  }

  class RefinedBlackboxBundle(val c: RBBC) {
    import c.universe._
    def impl = reify(c.prefix.splice)
  }

  class WhiteboxBundle(val c: WBC) {
    import c.universe._
    def impl = q"${c.prefix}"
  }

  class RefinedWhiteboxBundle(val c: RWBC) {
    import c.universe._
    def impl = reify(c.prefix.splice)
  }
}

class C {
  def blackbox: C = macro Module.BlackboxBundle.impl
  def refinedBlackbox: C = macro Module.RefinedBlackboxBundle.impl
  def whitebox: C = macro Module.WhiteboxBundle.impl
  def refinedWhitebox: C = macro Module.RefinedWhiteboxBundle.impl
  override def toString = "C"
}