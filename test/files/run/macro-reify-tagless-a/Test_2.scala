object Test extends App {
  //val list: List[String] = Macros.foo("hello world")
  //println(list)

  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val tpt = AppliedTypeTree(Ident(definitions.ListClass), List(Ident(definitions.StringClass)))
  val rhs = Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant("hello world"))))
  val list = ValDef(NoMods, newTermName("list"), tpt, rhs)
  val tree = Block(list, Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Ident(list.name))))
  try cm.mkToolBox().runExpr(tree)
  catch { case ex: Throwable =>  println(ex.getMessage) }
}
