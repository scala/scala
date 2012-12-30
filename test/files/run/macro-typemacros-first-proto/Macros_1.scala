import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl[T: c.WeakTypeTag](c: Context)(x: c.Expr[Any]): c.Tree = {
    import c.universe._
    val msg = "I've been created from " + c.macroApplication
    val Block(List(synthetic: ClassDef), _) = reify{ trait SomeUniqueName { def hello = c.literal(msg).splice } }.tree
    c.topLevelRef(TypeName("test. " + synthetic.name)) orElse c.introduceTopLevel("test", synthetic)
  }

  type Foo(x: Int) = macro impl[String]
  type Foo[T](x: String) = macro impl[T]
}
