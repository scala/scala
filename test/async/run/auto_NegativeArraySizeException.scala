import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { test
  def foo(foo: Any, bar: Any) = ()
  @autoawait def getValue = 4.2
  @async def func(f: Any) = {
    foo(f match { case _ if "".isEmpty => 2 }, getValue);
  }

  @async
  def test() = func(4)
}
