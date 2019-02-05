import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {

  override def code = """
    trait T {
      /**
       * @usecase def foo: Int
       */
      def foo(implicit err: String): Int = ???
    }
  """

  def scaladocSettings = "-deprecation"
  def testModel(root: Package) = ()
}
