import scala.tools.nsc.doc.Universe
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocJavaModelTest

object Test extends ScaladocJavaModelTest {

  override def resourceFile = "SI-4826.java"
  override def scaladocSettings = "-no-java-comments"

  def testModel(rootPackage: Package) = {
    import access._

    val base = rootPackage._package("test")._package("scaladoc")
    val clazz = base._class("JavaComments")
    val method = clazz._method("answer")

    assert(clazz.comment == None)
    assert(method.comment == None)
  }
}
