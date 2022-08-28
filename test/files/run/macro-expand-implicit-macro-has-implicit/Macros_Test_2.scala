import scala.language.experimental.macros
object Test extends App {
  implicit val x: Int = 42
  def foo(implicit x: Int): Unit = macro Impls.foo
  foo
}
