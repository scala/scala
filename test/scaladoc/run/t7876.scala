import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
  import language.higherKinds
  trait T[M[_]]
  class C extends T[Function0]
  class D extends T[Tuple1]
                      """

  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._
    // did not crash
  }
}
