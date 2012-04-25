object Test extends App {
  import scala.reflect.mirror._
  val tree = Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant(42))))
  println(Expr(tree).eval)
}
