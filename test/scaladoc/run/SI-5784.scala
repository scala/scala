import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile: String = "SI-5784.scala"

  // no need for special settings
  def scaladocSettings = "-diagrams"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val main = rootPackage._package("test")._package("templates")

    val String = main._aliasTypeTpl("String")
    assert(String.companion.isDefined, "test.templates.String should have a pseudo-companion object")

    val Base = main._trait("Base")
    assert(Base.members.filter(_.inDefinitionTemplates.head == Base).length == 5, Base.members.filter(_.inDefinitionTemplates.head == Base).length + " == 5")
    assert(Base.members.collect{case d: DocTemplateEntity => d}.length == 4, Base.members.collect{case d: DocTemplateEntity => d}.length == 4)
    testDiagram(Base, Base.contentDiagram, 2, 1)

    val BaseT = Base._absTypeTpl("T")
    val Foo = Base._trait("Foo")
    assert(BaseT.members.filter(_.inDefinitionTemplates.head == Base).length == 0, BaseT.members.filter(_.inDefinitionTemplates.head == Base).length + " == 0")
    assert(BaseT.members.map(_.name).sorted == Foo.members.map(_.name).sorted, BaseT.members.map(_.name).sorted + " == " + Foo.members.map(_.name).sorted)
    assert(BaseT.companion.isDefined, "test.templates.Base.T should have a pseudo-companion object")
    testDiagram(BaseT, BaseT.inheritanceDiagram, 2, 1)

    val Api = main._trait("Api")
    assert(Api.members.filter(_.inDefinitionTemplates.head == Api).length == 2, Api.members.filter(_.inDefinitionTemplates.head == Api).length + " == 2") // FooApi and override type T
    assert(Api.members.collect{case d: DocTemplateEntity => d}.length == 5, Api.members.collect{case d: DocTemplateEntity => d}.length == 5)
    testDiagram(Api, Api.contentDiagram, 3, 2)

    val ApiT = Api._absTypeTpl("T")
    val FooApi = Api._trait("FooApi")
    assert(ApiT.members.filter(_.inDefinitionTemplates.head == Api).length == 0, ApiT.members.filter(_.inDefinitionTemplates.head == Api).length + " == 0")
    assert(ApiT.members.map(_.name).sorted == FooApi.members.map(_.name).sorted, ApiT.members.map(_.name).sorted + " == " + FooApi.members.map(_.name).sorted)
    assert(ApiT.companion.isDefined, "test.templates.Api.T should have a pseudo-companion object")
    testDiagram(ApiT, ApiT.inheritanceDiagram, 2, 1)
  }
}