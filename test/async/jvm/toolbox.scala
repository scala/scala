import tools.reflect._
import reflect.runtime._
object Test extends App {
  val box = currentMirror.mkToolBox(options = "-Xasync")
  val code =
    """
    import scala.concurrent._, scala.concurrent.duration._, ExecutionContext.Implicits._
    import scala.tools.partest.async._
    val f1 = Future(1)
    val f2 = Future(2)
    val res = Async.async { Async.await(f1) + Async.await(f2) }
    Await.result(res, 1.second)
    """.stripMargin
  assert(box.eval(box.parse(code)) == 3)
}