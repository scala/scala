import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {
  override def resourceFile = "negative-defaults.scala"
  override def scaladocSettings = ""
  def testModel(root: Package) = {
    import access._

    val pkg = root._package("test")
    val intparam    = pkg._object("Test")._method("int").valueParams.head.head
    val longparam   = pkg._object("Test")._method("long").valueParams.head.head
    val floatparam  = pkg._object("Test")._method("float").valueParams.head.head
    val doubleparam = pkg._object("Test")._method("double").valueParams.head.head
    val spacesparam = pkg._object("Test")._method("spaces").valueParams.head.head

    println(intparam.defaultValue)
    println(longparam.defaultValue)
    println(floatparam.defaultValue)
    println(doubleparam.defaultValue)
    println(spacesparam.defaultValue)
  }
}
