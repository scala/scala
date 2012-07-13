import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile = "SI-3314-diagrams.scala"

  // no need for special settings
  def scaladocSettings = "-diagrams"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    val base = rootPackage._package("scala")._package("test")._package("scaladoc")

    val diagrams = base._package("diagrams")
    val templates = List(diagrams._trait("WeekDayTraitWithDiagram"), diagrams._class("WeekDayClassWithDiagram"), diagrams._object("WeekDayObjectWithDiagram"))

    for (template <- templates) {
      testDiagram(template, template.contentDiagram, 8, 7)
      val subtemplates = List("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun").map(template._object(_))
      for (subtemplate <- subtemplates)
        testDiagram(subtemplate, subtemplate.inheritanceDiagram, 2, 1)
    }
  }
}