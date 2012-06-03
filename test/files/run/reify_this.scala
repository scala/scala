import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

trait Eval {
  def eval(tree: Expr[_]) = tree.eval
}

object Test extends App with Eval {
  // select a value from package
  eval(reify{println("foo")})
  eval(reify{println((new Object).toString == (new Object).toString)})

  // select a type from package
  eval(reify{val x: Any = 2; println(x)})
  eval(reify{val x: Object = "bar"; println(x)})

  // select a value from module
  val x = 2
  eval(reify{println(x)})
}