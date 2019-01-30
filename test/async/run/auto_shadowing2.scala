import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { test

  trait Base;

  trait Foo[T <: Base] {
    @autoawait def func: Option[Foo[T]] = None
  }

  class Sub extends Base

  trait Bar extends Foo[Sub]

  def test: Any = test(new Bar {})
  @async
  def test[T <: Base](foo: Foo[T]): Foo[T] = foo match {
    case foo: Bar =>
      val res = foo.func
      res match {
        case _ =>
      }
      foo
    case other    => foo
  }
  test(new Bar {})
}
