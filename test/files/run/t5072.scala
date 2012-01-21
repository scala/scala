import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
class C
Thread.currentThread.getContextClassLoader.loadClass(classOf[C].getName)
  """
}
