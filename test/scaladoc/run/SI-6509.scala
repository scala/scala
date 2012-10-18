import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile: String = "SI-6509.scala"

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val main = rootPackage._package("test")._package("scaladoc")._package("template")._package("owners")
    val X = main._trait("X")
    val Y = main._trait("Y")
    val Z = main._trait("Z")
    val T = main._trait("T")

    def checkTemplateOwner(d: DocTemplateEntity) =
      for (mbr <- List("Symbol", "TypeSymbol", "TermSymbol", "MethodSymbol")) {
        val tpl = d._absTypeTpl(mbr).inTemplate
        assert(tpl == X, tpl + " == X")
      }

    for (tpl <- List(X, Y, Z, T))
      checkTemplateOwner(tpl)
  }
}