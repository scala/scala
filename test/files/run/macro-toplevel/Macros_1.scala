import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val msg = "I've been created from " + c.macroApplication
    val Block(List(synthetic: ClassDef), _) = reify{ class SomeUniqueName { def hello = c.literal(msg).splice } }.tree
    val ref = c.topLevelRef(synthetic.name) orElse c.introduceTopLevel(nme.EMPTY_PACKAGE_NAME.toString, synthetic)
    c.Expr[String](Select(Apply(Select(New(ref), nme.CONSTRUCTOR), List()), TermName("hello")))
  }

  def foo = macro impl
  def foo2 = macro impl
}
