
import scala.tools.partest._
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.FilteringReporter
import scala.reflect.internal.util.CodeAction

/** Test that compiler enforces maxerrs when given a plain Reporter. */
object Test extends DirectTest {

  override def code = """
    class C {
      def f(vs: Int*) = vs.sum

      def g = f("","","","","","","","","","")
    }
  """.trim

  var store0: UnfilteredStoreReporter = _
  // a reporter that ignores all limits
  def store = store0

  final val limit = 3

  override def show(): Unit = {
    compile()
    assert(store.infos.size == limit, s"${store.infos.size} should be $limit")
  }
  override def newSettings(args: List[String]) = {
    val s = super.newSettings(args)
    s.maxerrs.value = limit
    s
  }
  override def reporter(s: Settings) =
    if (store0 ne null) store0
    else {
      store0 = new UnfilteredStoreReporter(s)
      store0
    }
}

class UnfilteredStoreReporter(s: Settings) extends FilteringReporter {
  import scala.tools.nsc.reporters.StoreReporter._
  import scala.collection.mutable
  import scala.reflect.internal.util.Position

  val infos = new mutable.LinkedHashSet[Info]

  override def settings: Settings = s

  override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit =
    infos += Info(pos, msg, severity, actions)

  override def reset(): Unit = {
    super.reset()
    infos.clear()
  }
}
