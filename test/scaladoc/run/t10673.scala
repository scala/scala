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
        trait Foo extends AnyRef

        class Bar extends scala.collection.immutable.Seq[Nothing]
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
    def showParents(e: MemberTemplateEntity): Unit = {
      e.parentTypes.foreach(_._2.refEntity.foreach {
        case (_, (LinkToMember(mbr, tpl), _))          => println(s"found link for member $mbr to $tpl")
        case (_, (LinkToTpl(tpl), _))                  => println(s"found link $tpl")
        case (_, (LinkToExternalTpl(name, _, tpl), _)) => println(s"'$name' links to $tpl")
        case (_, (Tooltip(name), _))                   => println(s"'$name' no link!")
      })
    }

    showParents(rootPackage._trait("Foo"))
    showParents(rootPackage._class("Bar"))
  }
}
