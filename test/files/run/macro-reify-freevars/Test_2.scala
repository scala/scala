object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val q = New(AppliedTypeTree(Select(Select(Select(Ident(newTermName("scala")), newTermName("collection")), newTermName("slick")), newTypeName("Queryable")), List(Ident(newTermName("Int")))))
  val x = ValDef(NoMods, newTermName("x"), Ident(newTermName("Int")), EmptyTree)
  val fn = Function(List(x), Apply(Select(Ident(newTermName("x")), newTermName("$plus")), List(Literal(Constant("5")))))
  val tree = Apply(Select(q, newTermName("map")), List(fn))
  try cm.mkToolBox().eval(tree)
  catch { case ex: Throwable =>  println(ex.getMessage) }
}