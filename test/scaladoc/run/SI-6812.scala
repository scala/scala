import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {

  override def code = """
    import scala.reflect.macros.BlackboxContext
    import language.experimental.macros

    object Macros {
      def impl(c: BlackboxContext) = c.literalUnit
      def foo: Unit = macro impl
    }

    class C {
      def bar = Macros.foo
    }
  """

  def scaladocSettings = ""
  override def extraSettings = super.extraSettings + " -Ymacro-no-expand"
  def testModel(root: Package) = ()
}
