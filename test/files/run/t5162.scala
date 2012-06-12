// In run, rather than pos, to check for problems like SI-4283
object O1 {
  private[O1] class Base {
    def foo: Int = 0
  }
  class Mediator extends Base
}

object O2 {
  class Derived extends O1.Mediator {
    override def foo: Int = super.foo
  }
}

object Test {
  def main(args: Array[String]) {
    new O2.Derived().foo
  }
}
