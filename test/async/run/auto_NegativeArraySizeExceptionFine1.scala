import scala.tools.nsc.transform.async.user.{async, autoawait}

case class FixedFoo(foo: Int)

class Foobar(val foo: Int, val bar: Double) {
  @autoawait
  @async def getValue = 4.2
  @autoawait
  @async def func(f: Any) = {
    new Foobar(foo = f match {
      case FixedFoo(x) => x
      case _           => 2
    },
                bar = getValue)
  }
}

object Test extends App { test
  @async def test() = new Foobar(0, 0).func(4)
}
