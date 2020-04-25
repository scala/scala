import scala.tools.partest._
import scala.tools.testkit.AssertUtil.assertThrows

object Test extends DirectTest {
  override def extraSettings: String = s"-usejavacp -cp ${testOutput.path} -Ystop-after:typer"

  def code = "class C { A.f }"

  def show(): Unit = assertThrows[OutOfMemoryError](compile(), _ == "OOM")
}
