import java.net.URI

import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
      package scala.test.scaladoc.T8857

      /**
       *  A link:
       *
       * [[scala.Option$ object Option]].
       */
      class A
    """

  def scalaURL = "http://www.scala-lang.org/api/current/"

  // a non-canonical path to scala-library.jar should still work
  override def scaladocSettings = {
    val samplePath = getClass.getClassLoader.getResource("scala/Function1.class").getPath.replace('\\', '/')
    val scalaLibPath = if(samplePath.contains("!")) { // in scala-library.jar
      val scalaLibUri = samplePath.split("!")(0)
      val p = new URI(scalaLibUri).getPath
      // this is a bit fragile (depends on the scala library being in build/pack as produced by ant)
      p.replace("/pack/lib/scala-library.jar", "/pack/bin/../lib/scala-library.jar")
    } else { // individual class files on disk
      val p = samplePath.dropRight("scala/Function1.class".length + 1)
      p + "/.." + p.takeRight(p.length - p.lastIndexOf('/'))
    }
    s"-doc-external-doc $scalaLibPath#$scalaURL"
  }

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val a = rootPackage._package("scala")._package("test")._package("scaladoc")._package("T8857")._class("A")

    val links = countLinks(a.comment.get, _.link.isInstanceOf[LinkToExternal])
    assert(links == 1, links + " ==  1 (the links to external in class A)")
  }
}
