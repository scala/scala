import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {
  override def resourceFile = "t10621.scala"
  override def scaladocSettings = ""
  def testModel(root: Package) = {
    import access._

    val pkg = root._package("test")
    val baseTrait    = pkg._trait("BaseTrait")
    val baseClass    = pkg._class("BaseClass")
    val test         = pkg._class("Test")

    def printBoth(supr: DocTemplateEntity, name: String) = {
      println(supr._member(name).comment.map(_.body))
      println(test._member(name).comment.map(_.body))
      println()
    }

    printBoth(baseTrait, "someInt")
    printBoth(baseTrait, "anotherInt")
    printBoth(baseTrait, "yetMoreInt")
    printBoth(baseTrait, "theLastInt")
    printBoth(baseClass, "doohickey")
    printBoth(baseClass, "whatzit")
    printBoth(baseClass, "fiddle")
    printBoth(baseClass, "surprise")
  }
}