import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    val c = { class C { override def toString = "C" }; ((new C, new C { def f = 2 })) }
  """
}
