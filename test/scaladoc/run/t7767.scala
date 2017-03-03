import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
    class CEarly extends { /**CEarly_Doc_foo*/ val foo = 0 } with AnyRef
    trait TEarly extends { /**TEarly_Doc_foo*/ val foo = 0 } with AnyRef
    class C {
      /**C_Doc_sigInferred*/ val sigInferred = 0
      /**C_Doc_const*/ final val const = 0
      /**C_Doc_varr*/ var varr: Any = null
      /**C_Doc_abs*/ val abs: Int
      /**C_Doc_absVar*/ var absVar: Any
      /**C_Doc_lazyValInferred*/ lazy val lazyValInferred = 0
      /**C_Doc_lazyValConst*/ final lazy val lazyValConst = 0
      /**C_Doc_lazyValUnit*/ lazy val lazyValUnit: Unit = println()
      /**C_Doc_lazyVal*/ lazy val lazyVal: Int = 0
    }
    trait T {
      /**T_Doc_sigInferred*/ val sigInferred = 0
      /**T_Doc_const*/ final val const = 0
      /**T_Doc_varr*/ var varr: Any = null
      /**T_Doc_abs*/ val abs: Int
      /**T_Doc_absVar*/ var absVar: Any
      /**T_Doc_lazyValInferred*/ lazy val lazyValInferred = 0
      /**T_Doc_lazyValConst*/ final lazy val lazyValConst = 0
      /**T_Doc_lazyValUnit*/ lazy val lazyValUnit: Unit = println()
      /**T_Doc_lazyVal*/ lazy val lazyVal: Int = 0
    }"""

  // no need for special settings
  def scaladocSettings = ""

  def assertDoc(classEntity: DocTemplateEntity, valName: String) = {
    import access._
    val comment = classEntity._value(valName).comment.map(_.body.toString.trim).getOrElse("")
    val className = classEntity.name
    val marker = s"${className}_Doc_${valName}"
    assert(comment.contains(marker), s"Expected $marker in comment for $valName in $className, found: $comment.")
  }

  def testModel(rootPackage: Package) = {
    import access._
    assertDoc(rootPackage._class("CEarly"), "foo")
    assertDoc(rootPackage._trait("TEarly"), "foo")

    val valNames = List("sigInferred", "const", "varr", "abs", "absVar", "lazyValInferred", "lazyValConst", "lazyValUnit", "lazyVal")
    val entities = List(rootPackage._class("C"), rootPackage._trait("T"))
    for (e <- entities; vn <- valNames) assertDoc(e, vn)
  }
}
