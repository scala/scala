import scala.reflect.makro.{Context => Ctx}

object Macros {
  def fooEval(s: String) = macro Impls.fooEval
  def fooValue(s: String) = macro Impls.fooValue

  object Impls {
    def fooEval(c: Ctx)(s: c.Expr[String]) = c.reify {
      println("hello " + s.eval)
      println("hello " + s.eval)
    }

    def fooValue(c: Ctx)(s: c.Expr[String]) = c.reify {
      {
        println("hello " + s.value)
        def sayHello = println(s.value)
        sayHello
      }
      println("hello " + s.eval);
      {
        println("hello " + s.eval)
      }
    }
  }
}