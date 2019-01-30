import scala.tools.nsc.transform.async.user.{async, autoawait}

trait Base

class Sub extends Base

trait Foo[T <: Base] {
  @autoawait def func: Option[Foo[T]] = None
}

trait Bar extends Foo[Sub]

object Test extends App { assert(test)
  @async def func[T <: Base](foo: Foo[T]): Foo[T] = foo match { // the whole pattern match will be wrapped with async{ }
    case foo: Bar =>
      val res = foo.func // will be rewritten into: await(foo.func)
      res match {
        case Some(v) => v // this will report type mismtach
        case other   => foo
      }
    case other    => foo
  }
  def test: Boolean = {
    val b = new Bar {};
    func(b) == b
  }
}
