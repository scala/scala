package tastytest

object TestCtxFns {

  import CtxFns._

  class TestContext extends Context {
    val logs = collection.mutable.ListBuffer.empty[String]
    def puts[T](t: T): Unit = logs += t.toString
  }

  implicit val ctx = new TestContext

  def test1: Unit = {
    puts(23)(ctx)
    puts(true)(ctx)
    puts("Hello")(ctx)
    assert(ctx.logs.toList == List("23", "true", "Hello"))
  }

  def test2: Unit = {
    val box = new CtxBox[Contextual]
    box.foo[Unit]
  }

}
