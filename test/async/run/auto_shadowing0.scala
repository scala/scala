import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { test

  trait Foo

  trait Bar

  def test: Any = test(new C)
  @autoawait def asyncBoundary: String = ""
  @async
  def test(foo: Foo): Foo = foo match {
    case foo: Bar =>
      val foo2: Foo with Bar = new Foo with Bar {}
      asyncBoundary
      null match {
        case _ => foo2
      }
    case other    => foo
  }

  class C extends Foo with Bar
}
