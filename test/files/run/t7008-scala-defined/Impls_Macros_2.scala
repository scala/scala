import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import System.{lineSeparator => EOL}

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val decls = c.typeOf[ScalaClassWithCheckedExceptions_1[_]].decls.toList
    decls.foreach(_.info)
    decls.foreach(_.annotations.foreach(_.tpe))
    val s = decls.sortBy(_.name.toString).map(decl => (s"${decl.name}: ${decl.annotations}")).mkString(EOL)
    reify(println(c.Expr[String](Literal(Constant(s))).splice))
  }

  def foo: Unit = macro impl
}
