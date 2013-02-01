import scala.tools.nsc.doc
import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.html.page.Index
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def scaladocSettings = ""
  override def code = """
    class STAR
    class Star
  """

  def testModel(rootPackage: Package) {
    model match {
      case Some(universe) => {
        val index = IndexModelFactory.makeIndex(universe)

        val indexPage = new Index(universe, index)
        val letters = indexPage.letters
        assert(letters.length > 1)
        assert(letters(0).toString == "<span>#</span>")
      }
      case _ => assert(false)
    }
  }
}
