import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._

    def test(tree: Tree, mode: c.TypecheckMode): String = {
      try c.typecheck(tree, mode, silent = false).tpe.toString
      catch { case c.TypecheckException(_, msg) => msg }
    }

    q"""
      println(${test(q"List(1, 2)", c.TERMmode)})
      println(${test(q"List", c.TERMmode)})
      println(${test(q"RuntimeException", c.TERMmode)})
      println(${test(tq"List[Int]", c.TYPEmode)})
      println(${test(tq"List", c.TYPEmode)})
      println(${test(q"List", c.TYPEmode)})
      println(${test(q"List(1, 2)", c.TYPEmode)})
    """
  }
  def foo: Unit = macro impl
}