import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile = "SI-4360.scala"

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("prefix")

    val TEST = base._package("pack1")._package("c")._class("TEST")
    val fooCA = TEST._method("fooCA")
    val fooCB = TEST._method("fooCB")
    val fooCS = TEST._method("fooCS")
    val fooCL = TEST._method("fooCL")
    val fooPA = TEST._method("fooPA")
    val fooPB = TEST._method("fooPB")
    val fooPC = TEST._method("fooPC")

    val expected = List(
      (fooCA, "Z", 1),
      (fooCB, "B.Z", 1),
      (fooCS, "pack2.Z.Z", 1),
      (fooCL, "L.Z", 1),
      (fooPA, "a.C", 1),
      (fooPB, "b.C", 1),
      (fooPC, "C", 1)
    )

    for ((method, name, refs) <- expected) {
      assert(method.valueParams(0)(0).resultType.name == name,
             method.valueParams(0)(0).resultType.name + " == " + name + " (in " + method.qualifiedName + ")")
      assert(method.valueParams(0)(0).resultType.refEntity.size == refs,
             method.valueParams(0)(0).resultType.refEntity.size + " == " + refs + " (in " + method.qualifiedName + ")")
    }

    val A = base._package("pack1")._package("c")._class("A")
    assert(A.linearizationTypes(0).name == "pack1.A",   A.linearizationTypes(0).name + " == pack1.A")
    assert(A.linearizationTypes(0).refEntity.size == 1, A.linearizationTypes(0).refEntity.size + " == 1")
  }
}