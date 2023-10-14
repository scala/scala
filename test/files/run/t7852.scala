// scalac: -opt:l:none
//
import scala.tools.partest.BytecodeTest
import scala.tools.asm
import scala.tools.asm.util._
import scala.tools.nsc.util.stringFromWriter
import scala.jdk.CollectionConverters._

object Test extends BytecodeTest {
  val nullChecks = Set(asm.Opcodes.IFNONNULL, asm.Opcodes.IFNULL)

  def show(): Unit = {
    def test(methodName: String, expected: Int): Unit = {
      val classNode = loadClassNode("Lean")
      val methodNode = getMethod(classNode, methodName)
      val got = countNullChecks(methodNode.instructions)
      assert(got == expected, s"$methodName: expected $expected but got $got comparisons")
    }
    test("string", expected = 0)
    test("module", expected = 0)
    test("moduleIndirect", expected = 2)
    test("nilModuleViaScalaPackageAlias", expected = 0)
    test("moduleViaScalaPackageAlias", expected = 2)
  }

  def countNullChecks(insnList: asm.tree.InsnList): Int =
    insnList.iterator.asScala.map(_.getOpcode).count(nullChecks)
}

class Lean {
  def string: Unit = {
    "" == toString
  }

  def module: Unit = {
    scala.collection.immutable.Nil == (toString: Any)
  }

  def moduleIndirect: Unit = {
    val n: Nil.type = null
    n == (toString: Any) // still need null checks here.
  }

  def nilModuleViaScalaPackageAlias: Unit = {
    Nil == (toString: Any)
  }

  def moduleViaScalaPackageAlias: Unit = {
    Right == (toString: Any)
  }
}
