object Macros {
  def foo(xs: Int*) = macro Impls.foo
}

object Test extends App {
  import scala.reflect.mirror._
  val tree = Apply(Select(Ident("Macros"), newTermName("foo")), List(Typed(Apply(Ident(definitions.ListModule), List(Literal(Constant(1)), Literal(Constant(2)))), Ident(tpnme.WILDCARD_STAR))))
  try Expr(tree).eval
  catch { case ex: Throwable =>  println(ex.getMessage) }
}