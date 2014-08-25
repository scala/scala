package scala.tools.nsc.backend.jvm

import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{AbstractInsnNode, LabelNode, ClassNode, MethodNode}
import scala.tools.partest.ASMConverters
import scala.collection.JavaConverters._

object CodeGenTools {
  import ASMConverters._

  def genMethod( flags: Int = Opcodes.ACC_PUBLIC,
                 name: String = "m",
                 descriptor: String = "()V",
                 genericSignature: String = null,
                 throwsExceptions: Array[String] = null)(body: Instruction*): MethodNode = {
    val node = new MethodNode(flags, name, descriptor, genericSignature, throwsExceptions)
    applyToMethod(node, body.toList)
    node
  }

  def wrapInClass(method: MethodNode): ClassNode = {
    val cls = new ClassNode()
    cls.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC, "C", null, "java/lang/Object", null)
    cls.methods.add(method)
    cls
  }
}
