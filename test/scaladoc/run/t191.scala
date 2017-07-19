import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.doc.html.Page
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
    val samplePath = getClass.getClassLoader.getResource("scala/Function1.class").getPath
    val scalaLibPath = if(samplePath.contains("!")) { // in scala-library.jar
      val scalaLibUri = samplePath.split("!")(0)
      new URI(scalaLibUri).getPath
    } else { // individual class files on disk
      samplePath.replace('\\', '/').dropRight("scala/Function1.class".length)
    }
    s"-no-link-warnings -doc-external-doc $scalaLibPath#$scalaURL"
  }

  def testModel(rootPackage: Package) {
    import access._
    val test = rootPackage._object("Test")

    def check(memberDef: Def, expected: Int) {
      val externals = memberDef.valueParams(0)(0).resultType.refEntity collect {
        case (_, (LinkToExternalTpl(name, url, _), _)) => assert(url.contains(scalaURL)); name
      }
      assert(externals.size == expected)
    }

    check(test._method("foo"), 1)
    check(test._method("bar"), 0)
    check(test._method("barr"), 2)
    check(test._method("baz"), 0)

    val expectedUrls = collection.mutable.Set[String](
                         "scala/collection/Map",
                         "scala/collection/immutable/$colon$colon",
                         "scala/Int",
                         "scala/Predef$",
                         "scala/Int#toLong:Long",
                         "scala/index",
                         "scala/index#AbstractMethodError=AbstractMethodError",
                         "scala/Predef$#String=String"
                      ).map( _.split("#").toSeq ).map({
                        case Seq(one)      => scalaURL + "/" + one + ".html"
                        case Seq(one, two) => scalaURL + "/" + one + ".html#" + two
                      })

    def isExpectedExternalLink(l: EntityLink) = l.link match {
      case LinkToExternalTpl(name, baseUrlString, tpl: TemplateEntity) =>
        val baseUrl = new URI(Page.makeUrl(baseUrlString, Page.templateToPath(tpl)))
        val url = if (name.isEmpty) baseUrl
                  else new URI(baseUrl.getScheme, baseUrl.getSchemeSpecificPart, name)
        assert(expectedUrls contains url.toString, url.toString + " " + expectedUrls)
        true
      case _ => false
    }

    assert(countLinks(test.comment.get, isExpectedExternalLink) == 8,
           countLinks(test.comment.get, isExpectedExternalLink) + " == 8")
  }
}
