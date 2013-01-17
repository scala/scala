import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.tools.partest.ScaladocModelTest
import java.net.{URI, URL}
import java.io.File

object Test extends ScaladocModelTest {

  override def code =
    """
        /** See:
         *  - [[scala.collection.Map]] Simple linking
         *  - [[scala.collection.immutable.::]] Linking with symbolic name
         *  - [[scala.Int]].toLong Linking to a class
         *  - [[scala.Predef]] Linking to an object
         *  - [[scala.Int.toLong]] Linking to a method
         *  - [[scala]] Linking to a package
         *  - [[scala.AbstractMethodError]] Linking to a member in the package object
         *  - [[scala.Predef.String]] Linking to a member in an object
         *
         *  Don't look at:
         *  - [[scala.NoLink]] Not linking :)
         */
        object Test {
          def foo(param: Any) {}
          def barr(l: scala.collection.immutable.List[Any]) {}
          def bar(l: List[String]) {}   // TODO: Should be able to link to type aliases
          def baz(d: java.util.Date) {} // Should not be resolved
        }
    """

  def scalaURL = "http://bog.us"

  override def scaladocSettings = {
    val scalaLibUri = getClass.getClassLoader.getResource("scala/Function1.class").getPath.split("!")(0)
    val scalaLibPath = new URI(scalaLibUri).getPath
    val externalArg = s"$scalaLibPath#$scalaURL"
    "-no-link-warnings -doc-external-doc " + externalArg
  }

  def testModel(rootPackage: Package) {
    import access._
    val test = rootPackage._object("Test")

    def check(memberDef: Def, expected: Int) {
      val externals = memberDef.valueParams(0)(0).resultType.refEntity collect {
        case (_, (LinkToExternal(name, url), _)) => assert(url.contains(scalaURL)); name
      }
      assert(externals.size == expected)
    }

    check(test._method("foo"), 1)
    check(test._method("bar"), 0)
    check(test._method("barr"), 2)
    check(test._method("baz"), 0)

    val expectedUrls = collection.mutable.Set[String](
                         "scala.collection.Map",
                         "scala.collection.immutable.::",
                         "scala.Int",
                         "scala.Predef$",
                         "scala.Int@toLong:Long",
                         "scala.package",
                         "scala.package@AbstractMethodError=AbstractMethodError",
                         "scala.Predef$@String=String"
                       ).map(scalaURL + "/index.html#" + _)

    def isExpectedExternalLink(l: EntityLink) = l.link match {
      case LinkToExternal(name, url) => assert(expectedUrls contains url, url); true
      case _ => false
    }

    assert(countLinks(test.comment.get, isExpectedExternalLink) == 8,
           countLinks(test.comment.get, isExpectedExternalLink) + " == 8")
  }
}
