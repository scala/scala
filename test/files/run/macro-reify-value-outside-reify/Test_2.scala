object Test extends App {
  import scala.reflect.mirror._
  val tree = Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant(42))))
  try println(Expr(tree).eval)
  catch { case ex: Throwable =>  println(ex.getMessage) }
}
