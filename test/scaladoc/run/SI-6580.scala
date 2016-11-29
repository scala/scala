import scala.tools.nsc.doc
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def scaladocSettings = ""
  override def code = """

  object Test {
    /** Here z(1) is defined as follows:
      * <br>
      *   <img src='http://example.com/fig1.png'>
      * <br>
      * plus z(1) times
      * <br>
      *   <img src='http://example.com/fig2.png'>
      * <br>
      * equals QL of something
      */
    def f = 1
  }

  """

  def testModel(rootPackage: Package) {
    import access._

    val f = rootPackage._object("Test")._method("f")
    println(f.comment.get.short)
  }
}
