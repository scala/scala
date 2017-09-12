import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.ClassReader
import asm.tree
import scala.collection.JavaConverters._
import scala.reflect.{classTag, ClassTag}

object Test extends BytecodeTest {
  def internalName[T <: AnyRef : ClassTag]: String =
    classTag[T].runtimeClass.getName.replace('.', '/')

  def isBoxOrUnbox(node: tree.AbstractInsnNode): Boolean = node match {
    case method: tree.MethodInsnNode
      if method.getOpcode == asm.Opcodes.INVOKESTATIC &&
          method.owner == internalName[scala.runtime.BoxesRunTime] &&
          method.name.contains ("boxTo") => true
    case _ => false
  }

  def isCheckcast(node: tree.AbstractInsnNode): Boolean =
    node.getOpcode == asm.Opcodes.CHECKCAST

  def isInstanceof(node: tree.AbstractInsnNode): Boolean =
    node.getOpcode == asm.Opcodes.INSTANCEOF

  def show(): Unit = {
    val mb = loadClassNode("Matchbox$")
    val fooB = getMethod(mb, "foo$mBc$sp")
    val fooL = getMethod(mb, "foo$mJc$sp")
    val barB = getMethod(mb, "bar$mBc$sp")
    val barL = getMethod(mb, "bar$mJc$sp")
    val bazB = getMethod(mb, "baz$mBc$sp")
    val bazL = getMethod(mb, "baz$mJc$sp")
    val quux = getMethod(mb, "quux")

    List(fooB, fooL, barB, barL, bazB, bazL, quux) foreach { meth =>
      val boxunbox   = meth.instructions.iterator.asScala count isBoxOrUnbox
      val checkcast  = meth.instructions.iterator.asScala count isCheckcast
      val instanceof = meth.instructions.iterator.asScala count isInstanceof
      println(s"$boxunbox $checkcast $instanceof")
    }
  }
}