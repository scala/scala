import scala.tools.partest.BytecodeTest
import scala.collection.JavaConversions._

object Test extends BytecodeTest {

  def show: Unit = {
    println(classAttrsNames)
    println(traitAttrsNames)
    println(objectClass1AttrsNames)
    println(objectClass2AttrsNames)
  }

  def classAttrsNames = attrsNames("TestAttrClass")
  def traitAttrsNames = attrsNames("TestAttrTrait")
  def objectClass1AttrsNames = attrsNames("TestAttrObj")
  def objectClass2AttrsNames = attrsNames("TestAttrObj$")

  def attrsNames(clsName: String): String = {
    val classNode = loadClassNode(clsName)
    classNode.attrs map (_.`type`) mkString (" ")
  }
}

class TestAttrClass
object TestAttrObj
trait TestAttrTrait