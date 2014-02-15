import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    def detectFlags(sym: TermSymbol): String = {
      (sym.isVal, sym.isVar, !sym.isVal && !sym.isVar && !sym.isLazy, sym.isLazy).toString
    }
    q"println(${detectFlags(c.internal.enclosingOwner.asTerm)}); 42"
  }

  def foo: Int = macro impl
}