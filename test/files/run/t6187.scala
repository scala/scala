import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
import language.experimental.macros, reflect.macros.Context
def macroImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[List[T]] = {
  val r = c.universe.reify { List(t.splice) }
  c.Expr[List[T]]( c.resetLocalAttrs(r.tree) )
}
def demo[T](t: T): List[T] = macro macroImpl[T]
def m[T](t: T): List[List[T]] =
  demo( List((t,true)) collect { case (x,true) => x } )
m(List(1))
// Showing we haven't added unreachable warnings
List(1) collect { case x => x }
List("") collect { case x => x }
  """.trim
}
