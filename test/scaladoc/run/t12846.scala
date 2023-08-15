
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile = "`t12846`.scala"

  // resourcePath has the form ../resources in the test, but that's ok for relativizing
  override def scaladocSettings = s"-Xlint -doc-source-url http://acme.com/source/€{FILE_PATH_EXT}#L€{FILE_LINE} -sourcepath $resourcePath"

  override def code = ???

  def testModel(root: Package) = {
    import access._
    println(root._package("example")._class("X-Upload-Content-Type").sourceUrl)
  }

  // `makeUniverse` takes either file names or source code, so we override to specify file name
  override def model = newDocFactory.makeUniverse(Left(List(s"$resourcePath/$resourceFile")))
}
