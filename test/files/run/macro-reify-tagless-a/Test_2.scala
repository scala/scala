object Test extends App {
  //val list: List[String] = Macros.foo("hello world")
  //println(list)

  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val tpt = AppliedTypeTree(Ident(definitions.ListClass), List(Ident(definitions.StringClass)))
  val rhs = Apply(Select(Ident(TermName("Macros")), TermName("foo")), List(Literal(Constant("hello world"))))
  val list = ValDef(NoMods, TermName("list"), tpt, rhs)
  val tree = Block(List(list), Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Ident(list.name))))
  try cm.mkToolBox().eval(tree)
  catch { case ex: Throwable =>  println(ex.getMessage) }
}
