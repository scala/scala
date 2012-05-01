object Test extends App {
  import scala.reflect.mirror._
  val q = New(AppliedTypeTree(Select(Select(Select(Ident("scala"), newTermName("collection")), newTermName("slick")), newTypeName("Queryable")), List(Ident("Int"))))
  val x = ValDef(NoMods, newTermName("x"), Ident("Int"), EmptyTree)
  val fn = Function(List(x), Apply(Select(Ident(newTermName("x")), newTermName("$plus")), List(Literal(Constant("5")))))
  val tree = Apply(Select(q, newTermName("map")), List(fn))
  try Expr(tree).eval
  catch { case ex: Throwable =>  println(ex.getMessage) }
}