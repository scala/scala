trait Foo {
  object bar {
    private[this] def fn() = 5
  }
}

trait Foo2 {
  object bip {
    def fn() = 10
  }
}

class Bob extends AnyRef with Foo with Foo2 {
  import bip._
  import bar._
  
  def go() = fn()
}
