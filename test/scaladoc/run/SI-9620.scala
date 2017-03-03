import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
    package a

    trait Foo[S] {
      def foo(t: S): Int = 123
    }

    /** Boo with only one foo method, hopefully!
     * @hideImplicitConversion BooShouldNotAppearIsFoo
     */
    trait Boo[T]

    object Boo {
      sealed trait ShouldNotAppear
      implicit class BooShouldNotAppearIsFoo(boo: Boo[ShouldNotAppear]) extends Foo[ShouldNotAppear]
      implicit class BooLongIsFoo(boo: Boo[Long]) extends Foo[Long]
    }
  """

  def scaladocSettings = "-implicits"

  def testModel(rootPackage: Package) = {
    import access._

    // Assert Boo only has one implicit conversion
    val boo = rootPackage._package("a")._trait("Boo")
    val conversions = boo._conversions("a.Boo.BooShouldNotAppearIsFoo") ++ boo._conversions("a.Boo.BooLongIsFoo")
    assert(conversions.length == 1, conversions.length + " == 1")

    // Assert that the implicit conversion is not "BooShouldNotAppearIsFoo"
    assert(conversions.head.conversionShortName == "BooLongIsFoo",
           conversions.head.conversionShortName + " == BooLongIsFoo")

    // Assert that the same for full path
    assert(conversions.head.conversionQualifiedName == "a.Boo.BooLongIsFoo",
           conversions.head.conversionQualifiedName + " == a.Boo.BooLongIsFoo")
  }
}
