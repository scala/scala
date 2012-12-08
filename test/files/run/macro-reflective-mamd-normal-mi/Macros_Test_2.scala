//object Macros {
//  def foo(x: Int) = macro Impls.foo
//}

object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.universe.Flag._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox

  val macrobody = Select(Ident(TermName("Impls")), TermName("foo"))
  val macroparam = ValDef(NoMods, TermName("x"), TypeTree(definitions.IntClass.toType), EmptyTree)
  val macrodef = DefDef(Modifiers(MACRO), TermName("foo"), Nil, List(List(macroparam)), TypeTree(), macrobody)
  val modulector = DefDef(NoMods, nme.CONSTRUCTOR, Nil, List(List()), TypeTree(), Block(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())))
  val module = ModuleDef(NoMods, TermName("Macros"), Template(Nil, emptyValDef, List(modulector, macrodef)))
  val macroapp = Apply(Select(Ident("Macros"), TermName("foo")), List(Literal(Constant(42))))
  val tree = Block(macrodef, module, macroapp)
  val toolbox = cm.mkToolBox(options = "-language:experimental.macros")
  println(toolbox.eval(tree))
}
