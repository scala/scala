import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
    |/** See [[scala.collection.Map]] */
    |object Test {
    |  def foo(param: Any) {}
    |  def bar(l: List[String]) {}  // Cannot link to type aliases
    |  def barr(l: scala.collection.immutable.List[Any]) {}
    |  def baz(d: java.util.Date) {} // Not known, even externaly
    |}""".stripMargin

  def scalaScaladoc = "http://my.scala.scaladoc/index.html"

  override def scaladocSettings = "-external-urls scala=" + scalaScaladoc

  def testModel(rootPackage: Package) {
    import access._
    val test = rootPackage._object("Test")

    def check(memberDef: Def, expected: Int) {
      val externals = memberDef.valueParams(0)(0).resultType.refEntity collect {
        case (_, (LinkToExternal(name, url), _)) => assert(url.contains(scalaScaladoc)); name
      }
      assert(externals.size == expected)
    }

    check(test._method("foo"), 1)
    check(test._method("bar"), 0)
    check(test._method("barr"), 2)
    check(test._method("baz"), 0)

    assert(countLinks(test.comment.get, _.link.isInstanceOf[LinkToExternal]) == 1)
  }
}
