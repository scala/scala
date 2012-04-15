trait Base {
  def foo = macro Impls.foo
}

object Macros extends Base

class Macros extends Base

object Test extends App {
  (new Base {}).foo
  Macros.foo
  new Macros().foo
}