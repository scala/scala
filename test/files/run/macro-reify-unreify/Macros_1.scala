import scala.reflect.makro.{Context => Ctx}

object Macros {
  def foo(s: String) = macro Impls.foo

  object Impls {
    def foo(c: Ctx)(s: c.Expr[String]) = {
      import c.universe._

      val world = c.reifyTree(c.runtimeUniverse, EmptyTree, s.tree)
      val greeting = c.reifyTree(c.runtimeUniverse, EmptyTree, c.typeCheck(Apply(Select(Literal(Constant("hello ")), newTermName("$plus")), List(c.unreifyTree(world)))))
      val typedGreeting = c.Expr[String](greeting)

      c.reify {
        println("hello " + s.splice + " = " + typedGreeting.splice)
      }
    }
  }
}