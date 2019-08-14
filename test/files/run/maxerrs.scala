
import scala.tools.partest._
import scala.tools.nsc._
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

object Test extends DirectTest {

  override def code = """
    class C {
      def f(vs: Int*) = vs.sum

      def g = f("","","","","","","","","","")
    }
  """.trim

  override def extraSettings = "-usejavacp"

  // a reporter that ignores all limits
  var store: StoreReporter = _

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
  override def reporter(s: Settings) = { store = new StoreReporter(s); store }
}
