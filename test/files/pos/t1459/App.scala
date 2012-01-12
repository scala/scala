package foo
import base._

object App extends scala.App {
  class Concrete extends AbstractBase {
     override def doStuff(params:java.lang.String*): Unit = println("doStuff invoked")
  }

  val impl = new Concrete

  //succeeds
  impl.doStuff(null)

  val caller = new Caller

  // fails with AbstractMethodError
  caller.callDoStuff(impl)
}
