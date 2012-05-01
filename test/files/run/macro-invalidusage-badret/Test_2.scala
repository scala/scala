object Test extends App {
  import scala.reflect.mirror._
  val tree = Typed(Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant(42)))), Ident(newTypeName("String")))
  try Expr(tree).eval
  catch { case ex: Throwable =>  println(ex.getMessage) }
}