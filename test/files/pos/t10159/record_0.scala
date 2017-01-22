import language.dynamics, language.experimental.macros
import reflect.macros.whitebox.Context

object Record extends Dynamic {
  def selectDynamic(name: String): Any = macro impl
  def impl(c: Context)(name: c.Tree): c.Tree = {
    import c.universe._
    internal.setType(q"()", c.typecheck(tq"{type T = Int}", mode = c.TYPEmode).tpe)
  }
}