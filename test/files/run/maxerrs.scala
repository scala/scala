
import scala.tools.partest._
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.Reporter

/** Test that compiler enforces maxerrs when given a plain Reporter. */
object Test extends DirectTest {

  override def code = """
    class C {
      def f(vs: Int*) = vs.sum

      def g = f("","","","","","","","","","")
    }
  """.trim

  override def extraSettings = "-usejavacp"

  // a reporter that ignores all limits
  lazy val store = new UnfilteredStoreReporter

  final val limit = 3

  override def show(): Unit = {
    compile()
    assert(store.infos.size == limit)
  }
  override def newSettings(args: List[String]) = {
    val s = super.newSettings(args)
    s.maxerrs.value = limit
    s
  }
  override def reporter(s: Settings) = store
}

class UnfilteredStoreReporter extends Reporter {
  import scala.tools.nsc.reporters.StoreReporter._
  import scala.collection.mutable
  import scala.reflect.internal.util.Position

  val infos = new mutable.LinkedHashSet[Info]

  override def info0(pos: Position, msg: String, severity: Severity, force: Boolean) = infos += Info(pos, msg, severity)

  override def reset(): Unit = {
    super.reset()
    infos.clear()
  }
}
