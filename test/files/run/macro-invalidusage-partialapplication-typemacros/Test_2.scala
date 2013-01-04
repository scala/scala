object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val ListOfNil = List(List())
  val foo = Apply(Select(Ident(TermName("Macros")), TypeName("Foo")), List(Literal(Constant(2))))
  val ctor = DefDef(NoMods, nme.CONSTRUCTOR, Nil, ListOfNil, TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
  val valdef = ValDef(NoMods, TermName("foo"), foo.duplicate, Literal(Constant(null)))
  val classDef = ClassDef(NoMods, TypeName("C"), Nil, Template(List(foo.duplicate), emptyValDef, List(ctor, valdef)))
  try cm.mkToolBox().eval(Block(List(classDef), Literal(Constant(()))))
  catch { case ex: Throwable =>  println(ex.getMessage) }
}