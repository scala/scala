import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {

  override def code = """
    import scala.reflect.macros.blackbox.Context
    import language.experimental.macros

    object Macros {
      def impl(c: Context) = c.literalUnit
      def foo: Unit = macro impl
    }

    class C {
      def bar = Macros.foo
    }
  """

  def scaladocSettings = ""
  override def extraSettings = super.extraSettings + " -Ymacro-no-expand -deprecation"
  def testModel(root: Package) = ()
}
