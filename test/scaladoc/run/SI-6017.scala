import scala.tools.nsc.doc
import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.html.page.{Index, ReferenceIndex}
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
        // Because "STAR" and "Star" are different
        assert(index.firstLetterIndex('s').keys.toSeq.length == 2)
      }
      case _ => assert(false)
    }
  }
}
