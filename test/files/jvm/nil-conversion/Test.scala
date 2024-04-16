//> using options -opt:none
import scala.tools.partest.BytecodeTest

import scala.tools.asm
import asm.Opcodes._
import asm.tree.{InsnList, AbstractInsnNode, FieldInsnNode, InsnNode}
import scala.collection.JavaConverters._

object Test extends BytecodeTest {

  val module = "MODULE$" // nme.MODULE_INSTANCE_FIELD.decoded

  def checkModuleLoad(what: String, x: AbstractInsnNode): Unit =
    x match {
      case f: FieldInsnNode =>
        assert(f.name == module)
        assert(f.owner == what)
        assert(f.desc == s"L$what;")
        assert(f.getOpcode == GETSTATIC)
    }
  def checkReturn(x: AbstractInsnNode): Unit =
    x match {
      case i: InsnNode => assert(i.getOpcode == ARETURN)
    }

  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    verifyNilConversion(getMethod(classNode, "foo").instructions)
    verifyNilConversion(getMethod(classNode, "bar").instructions)
    //verifyNilConversion(getMethod(classNode, "baz").instructions)  // requires extraordinary dispensation
  }

  def verifyNilConversion(insnList: InsnList): Unit = {
    val all = insnList.iterator.asScala
    checkModuleLoad("scala/collection/immutable/Nil$", all.next())
    checkReturn(all.next())
    assert(!all.hasNext)
  }
}
