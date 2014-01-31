import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  def comment = "Do an amazing feat of computation."

  override def code = s"""
    //package funcs { object `package` { }}  // also fails
    package object funcs {
      /** $comment */
      def f(): Int = ???
    }
    //package funcs { class Foo }            // ok this way
  """

  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._

    val f = rootPackage._package("funcs")._method("f")
    assert(f.resultType.name == "Int", s"${f.resultType.name} == Int")
    assert(extractCommentText(f.comment.get) contains comment, "Bad comment")
  }

  override protected def done(): Unit = ()
}
