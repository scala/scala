
import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def code = ""

  private def bogusTemp = s"${testOutput.toString}/bogus" // no need to obfuscate -${System.currentTimeMillis}

  override def extraSettings = s"${super.extraSettings} -Ygen-asmp $bogusTemp"

  override def show() = assert(!compile())
}
