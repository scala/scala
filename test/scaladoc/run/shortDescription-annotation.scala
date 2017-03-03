import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
    package a

    /** This comment should not appear
     *  @shortDescription This one should appear
     */
    class Foo {
      /** This comment should appear */
      def foo: Int = 1

      /** This comment should not appear
       *  @shortDescription This comment should appear
       */
      def goo: Int = 2
    }
  """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import scala.tools.nsc.doc.base.comment._
    import access._

    def inlineToStr(inl: Inline): String = inl match {
      case Chain(items)              => items flatMap (inlineToStr(_)) mkString ""
      case Italic(in)                => inlineToStr(in)
      case Bold(in)                  => inlineToStr(in)
      case Underline(in)             => inlineToStr(in)
      case Monospace(in)             => inlineToStr(in)
      case Text(text)                => text
      case Summary(in)               => inlineToStr(in)
      case EntityLink(Text(text), _) => text
      case _                         => inl.toString
    }

    val foo = rootPackage._package("a")._class("Foo")

    // Assert that the class has the correct short description
    val classDesc = inlineToStr(foo.comment.get.short)
    assert(classDesc == "This one should appear", classDesc)

    // Assert that the `foo` method has the correct short description
    val fooDesc = inlineToStr(foo._method("foo").comment.get.short)
    assert(fooDesc == "This comment should appear", fooDesc)

    // Assert that the `goo` method has the correct short description
    val gooDesc = inlineToStr(foo._method("goo").comment.get.short)
    assert(gooDesc == "This comment should appear", gooDesc)
  }
}
