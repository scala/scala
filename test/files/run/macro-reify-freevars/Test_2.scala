object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val q = New(AppliedTypeTree(Select(Select(Select(Ident(TermName("scala")), TermName("collection")), TermName("slick")), TypeName("Queryable")), List(Ident(TermName("Int")))))
  val x = ValDef(NoMods, TermName("x"), Ident(TermName("Int")), EmptyTree)
  val fn = Function(List(x), Apply(Select(Ident(TermName("x")), TermName("$plus")), List(Literal(Constant("5")))))
  val tree = Apply(Select(q, TermName("map")), List(fn))
  try cm.mkToolBox().eval(tree)
  catch { case ex: Throwable =>  println(ex.getMessage) }
}