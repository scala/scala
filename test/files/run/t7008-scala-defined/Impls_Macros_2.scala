import language.experimental.macros
import scala.reflect.macros.BlackboxContext

object Macros {
  def impl(c: BlackboxContext) = {
    import c.universe._
    val decls = c.typeOf[ScalaClassWithCheckedExceptions_1[_]].declarations.toList
    val s = decls.sortBy(_.name.toString).map(decl => (s"${decl.name}: ${decl.annotations}")).mkString(scala.compat.Platform.EOL)
    reify(println(c.Expr[String](Literal(Constant(s))).splice))
  }

  def foo = macro impl
}