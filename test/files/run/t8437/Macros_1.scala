import scala.language.experimental.macros
import scala.reflect.macros._

abstract class AbstractBundle(val c: blackbox.Context) {
  import c.Expr
  import c.universe._
  def foo: Expr[Int] = Expr[Int](q"5")
}

class ConcreteBundle(override val c: blackbox.Context) extends AbstractBundle(c) {
  import c.Expr
  val bar: Expr[Int] = foo
}

object InvokeBundle {
  def foo: Int = macro ConcreteBundle.foo // nope
  def bar: Int = macro ConcreteBundle.bar // yep
}