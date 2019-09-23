
import scala.tools.partest.DirectTest
import scala.tools.nsc.reporters.{Reporter, StoreReporter}
import scala.tools.nsc.Settings
import scala.tools.nsc.util.stringFromStream

object Test extends DirectTest {

  override def extraSettings = "-usejavacp -opt:_ -opt-inline-from:** -Vinline _"

  override def reporter(settings: Settings): Reporter = new StoreReporter(settings)

  override def code = "class C { def f = locally(42) }"

  override def show() = {
    val res = stringFromStream { os =>
      Console.withOut(os) {
        assert(compile())
      }
    }
    assert(res.startsWith("Inlining into C.f"))
  }
}
