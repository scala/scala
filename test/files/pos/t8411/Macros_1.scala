import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def defaultZeroCase(pf: PartialFunction[Int, Int]): PartialFunction[Int, Int] = macro impl
  def impl(c: Context)(pf: c.Tree) = { import c.universe._
    val q"{ case ..$cases }" = pf
    q"{ case ..$cases case _ => 0 }"
  }
}
