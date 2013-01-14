import scala.reflect.macros.{Context => Ctx}

class C
object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    val Block(List(synthetic: ClassDef), _) = reify{
      class X {
        def x = 2
        println(s"prefix = ${c.literal(c.prefix.toString).splice}")
        println("it works")
      }
    }.tree
    if (!c.existsTopLevel(synthetic.name)) c.introduceTopLevel(nme.EMPTY_PACKAGE_NAME.toString, synthetic)
    c.universe.Ident(c.universe.TypeName("X"))
  }
}