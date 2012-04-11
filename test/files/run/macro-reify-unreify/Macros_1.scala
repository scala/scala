import scala.reflect.makro.{Context => Ctx}

object Macros {
  def foo(s: String) = macro Impls.foo

  object Impls {
    def foo(c: Ctx)(s: c.Expr[String]) = {
      import c.mirror._

      val world = c.reifyTree(c.reflectMirrorPrefix, s.tree)
      val greeting = c.reifyTree(c.reflectMirrorPrefix, c.typeCheck(Apply(Select(Literal(Constant("hello ")), newTermName("$plus")), List(c.unreifyTree(world)))))
      val typedGreeting = Expr[String](greeting)

      c.reify {
        println("hello " + s.eval + " = " + typedGreeting.eval)
      }
    }
  }
}