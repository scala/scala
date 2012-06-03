//object Macros {
//  def foo(x: Int) = macro Impls.foo
//}

object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.universe.Flag._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox

  val macrobody = Select(Ident(newTermName("Impls")), newTermName("foo"))
  val macroparam = ValDef(NoMods, newTermName("x"), TypeTree(definitions.IntClass.asType), EmptyTree)
  val macrodef = DefDef(Modifiers(MACRO), newTermName("foo"), Nil, List(List(macroparam)), TypeTree(), macrobody)
  val modulector = DefDef(NoMods, nme.CONSTRUCTOR, Nil, List(List()), TypeTree(), Block(Apply(Select(Super(This(EmptyTypeName), EmptyTypeName), nme.CONSTRUCTOR), List())))
  val module = ModuleDef(NoMods, newTermName("Macros"), Template(Nil, emptyValDef, List(modulector, macrodef)))
  val macroapp = Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant(42))))
  val tree = Block(macrodef, module, macroapp)
  val toolbox = cm.mkToolBox(options = "-language:experimental.macros")
  println(toolbox.runExpr(tree))
}
