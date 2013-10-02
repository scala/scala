import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {
  override def resourceFile = "package-object-res.scala"
  override def scaladocSettings = ""
  def testModel(root: Package) = {
    import access._

    val p = root._package("test")
    println(p.linearizationTemplates)
    println(p.linearizationTypes)
    println(p.inSource)
  }
}

