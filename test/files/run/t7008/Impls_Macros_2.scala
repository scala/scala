import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val decls = c.typeOf[JavaClassWithCheckedExceptions_1[_]].decls.toList
    val s = decls.sortBy(_.name.toString).map(decl => (s"${decl.name}: ${decl.annotations}")).mkString(scala.compat.Platform.EOL)
    reify(println(c.Expr[String](Literal(Constant(s))).splice))
  }

  def foo = macro impl
}