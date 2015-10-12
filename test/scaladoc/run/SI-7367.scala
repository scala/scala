import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
  class annot() extends annotation.StaticAnnotation {
    def this(a: Any) = this()
  }

  @annot(0)
  class B
  """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    val annotations = root._class("B").annotations
    assert(annotations.size == 1)
    assert(annotations(0).annotationClass == root._class("annot"))
    val args = annotations(0).arguments
    assert(args.size == 1)
    assert(args(0).value.expression == "0")
  }
}
