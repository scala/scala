
//> using options -opt:none
//
import scala.tools.partest.BytecodeTest
import scala.tools.testkit.ASMConverters.instructionsFromMethod
import org.junit.Assert.assertEquals

object Test extends BytecodeTest {
  def show(): Unit = {
    val classNode = loadClassNode("C")
    val f = getMethod(classNode, "f")
    val g = getMethod(classNode, "g")
    assertEquals(instructionsFromMethod(f), instructionsFromMethod(g))
    //sameBytecode(f, g) // prints
    new C().run() // should not crash
  }
}

class C {
  import scala.util.control.NonFatal
  def x = 42
  def f = try x catch { case NonFatal(e) => e.printStackTrace(); -1 }
  def g = try x catch { case e: Throwable if NonFatal(e) => e.printStackTrace(); -1 }

  def run(): Unit = {
    val any: Any = 42

    any match {
      case NonFatal(e) => ???
      case _ =>
    }
  }
}
