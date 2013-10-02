import language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def impl(c: Context) = {
    val decls = c.typeOf[ScalaClassWithCheckedExceptions_1[_]].declarations.toList
    val s = decls.sortBy(_.name.toString).map(decl => (s"${decl.name}: ${decl.annotations}")).mkString(scala.compat.Platform.EOL)
    c.universe.reify(println(c.literal(s).splice))
  }

  def foo = macro impl
}