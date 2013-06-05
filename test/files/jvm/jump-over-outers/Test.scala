import scala.tools.partest.BytecodeTest
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("C_1$MyActor$$anonfun$doSomeWork$1")
    val cachedField = classNode.fields.asScala.find( f => f.name.startsWith("cachedOuter$") ).get
    assert(cachedField.desc == "LC_1;")
    val outerFieldOpt = classNode.fields.asScala.find( f => f.name.startsWith("outer$") )
    assert(outerFieldOpt.isEmpty)
  }
}

