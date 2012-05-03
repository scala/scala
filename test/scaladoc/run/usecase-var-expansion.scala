import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {

  override def code = """
    /**
     * @define Coll `Test`
     */
    class Test[T] {
      /**
       * member $Coll
       * @usecase def foo: $Coll[T]
       *   usecase $Coll
       */
      def foo(implicit err: String): Test[T] = sys.error(err)
    }

    /** @define Coll {{{some `really` < !! >> invalid $$$ thing}}} */
    class Test2[T] extends Test[Int]
  """

  def scaladocSettings = ""
  def testModel(root: Package) = ()
}
