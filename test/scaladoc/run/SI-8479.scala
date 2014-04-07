import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.tools.partest.ScaladocModelTest
import java.net.{URI, URL}
import java.io.File

object Test extends ScaladocModelTest {

  override def code =
    """
      |object Test {
      |  val x = new SparkContext(master = "")
      |}
      |
      |class SparkContext(config: Any) {
      |
      |  /** Scaladoc comment */
      |  def this(
      |      master: String,
      |      appName: String = "") = this(null)
      |}
      |
      |
    """.stripMargin

  override def scaladocSettings = ""

  def testModel(rootPackage: Package) {
    // it didn't crash
  }
}
