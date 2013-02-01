import scala.language.dynamics

class Dyn extends Dynamic {
  def selectDynamic(s: String): Dyn = new Dyn
  def get[T]: T = null.asInstanceOf[T]
}

object Foo {
  val dyn = new Dyn
  dyn.foo.bar.baz.get[String]
}
