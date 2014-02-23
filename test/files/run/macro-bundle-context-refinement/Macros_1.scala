import scala.language.experimental.macros
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}

class BlackboxBundle(val c: BlackboxContext { type PrefixType = C }) {
  import c.universe._
  def impl = reify(c.prefix.splice)
}

class WhiteboxBundle(val c: WhiteboxContext { type PrefixType = C }) {
  import c.universe._
  def impl = reify(c.prefix.splice)
}

class C {
  def blackbox: C = macro BlackboxBundle.impl
  def whitebox: C = macro WhiteboxBundle.impl
  override def toString = "C"
}