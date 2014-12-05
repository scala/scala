/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.{tailrec, switch}
import scala.collection.mutable
import scala.reflect.internal.util.Collections._
import scala.tools.asm.Opcodes
import scala.tools.asm.tree._
import scala.collection.convert.decorateAsScala._

object BytecodeUtils {

  object Goto {
    def unapply(instruction: AbstractInsnNode): Option[JumpInsnNode] = {
      if (instruction.getOpcode == Opcodes.GOTO) Some(instruction.asInstanceOf[JumpInsnNode])
      else None
    }
  }

  object JumpNonJsr {
    def unapply(instruction: AbstractInsnNode): Option[JumpInsnNode] = {
      if (isJumpNonJsr(instruction)) Some(instruction.asInstanceOf[JumpInsnNode])
      else None
    }
  }

  object ConditionalJump {
    def unapply(instruction: AbstractInsnNode): Option[JumpInsnNode] = {
      if (isConditionalJump(instruction)) Some(instruction.asInstanceOf[JumpInsnNode])
      else None
    }
  }

  object VarInstruction {
    def unapply(instruction: AbstractInsnNode): Option[VarInsnNode] = {
      if (isVarInstruction(instruction)) Some(instruction.asInstanceOf[VarInsnNode])
      else None
    }

  }

  def isJumpNonJsr(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    // JSR is deprecated in classfile version 50, disallowed in 51. historically, it was used to implement finally.
    op == Opcodes.GOTO || isConditionalJump(instruction)
  }

  def isConditionalJump(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    (op >= Opcodes.IFEQ && op <= Opcodes.IF_ACMPNE) || op == Opcodes.IFNULL || op == Opcodes.IFNONNULL
  }

  def isReturn(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    op >= Opcodes.IRETURN && op <= Opcodes.RETURN
  }

  def isVarInstruction(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    (op >= Opcodes.ILOAD  && op <= Opcodes.ALOAD) || (op >= Opcodes.ISTORE && op <= Opcodes.ASTORE)
  }

  def isExecutable(instruction: AbstractInsnNode): Boolean = instruction.getOpcode >= 0

  def nextExecutableInstruction(instruction: AbstractInsnNode, alsoKeep: AbstractInsnNode => Boolean = Set()): Option[AbstractInsnNode] = {
    var result = instruction
    do { result = result.getNext }
    while (result != null && !isExecutable(result) && !alsoKeep(result))
    Option(result)
  }

  def sameTargetExecutableInstruction(a: JumpInsnNode, b: JumpInsnNode): Boolean = {
    // Compare next executable instead of the the labels. Identifies a, b as the same target:
    //   LabelNode(a)
    //   LabelNode(b)
    //   Instr
    nextExecutableInstruction(a.label) == nextExecutableInstruction(b.label)
  }

  def removeJumpAndAdjustStack(method: MethodNode, jump: JumpInsnNode) {
    val instructions = method.instructions
    val op = jump.getOpcode
    if ((op >= Opcodes.IFEQ && op <= Opcodes.IFGE) || op == Opcodes.IFNULL || op == Opcodes.IFNONNULL) {
      instructions.insert(jump, getPop(1))
    } else if ((op >= Opcodes.IF_ICMPEQ && op <= Opcodes.IF_ICMPLE) || op == Opcodes.IF_ACMPEQ || op == Opcodes.IF_ACMPNE) {
      instructions.insert(jump, getPop(1))
      instructions.insert(jump, getPop(1))
    } else {
      // we can't remove JSR: its execution does not only jump, it also adds a return address to the stack
      assert(jump.getOpcode == Opcodes.GOTO)
    }
    instructions.remove(jump)
  }

  def finalJumpTarget(source: JumpInsnNode): LabelNode = {
    @tailrec def followGoto(label: LabelNode, seenLabels: Set[LabelNode]): LabelNode = nextExecutableInstruction(label) match {
      case Some(Goto(dest)) =>
        if (seenLabels(dest.label)) dest.label
        else followGoto(dest.label, seenLabels + dest.label)

      case _ => label
    }
    followGoto(source.label, Set(source.label))
  }

  def negateJumpOpcode(jumpOpcode: Int): Int = (jumpOpcode: @switch) match {
    case Opcodes.IFEQ      => Opcodes.IFNE
    case Opcodes.IFNE      => Opcodes.IFEQ

    case Opcodes.IFLT      => Opcodes.IFGE
    case Opcodes.IFGE      => Opcodes.IFLT

    case Opcodes.IFGT      => Opcodes.IFLE
    case Opcodes.IFLE      => Opcodes.IFGT

    case Opcodes.IF_ICMPEQ => Opcodes.IF_ICMPNE
    case Opcodes.IF_ICMPNE => Opcodes.IF_ICMPEQ

    case Opcodes.IF_ICMPLT => Opcodes.IF_ICMPGE
    case Opcodes.IF_ICMPGE => Opcodes.IF_ICMPLT

    case Opcodes.IF_ICMPGT => Opcodes.IF_ICMPLE
    case Opcodes.IF_ICMPLE => Opcodes.IF_ICMPGT

    case Opcodes.IF_ACMPEQ => Opcodes.IF_ACMPNE
    case Opcodes.IF_ACMPNE => Opcodes.IF_ACMPEQ

    case Opcodes.IFNULL    => Opcodes.IFNONNULL
    case Opcodes.IFNONNULL => Opcodes.IFNULL
  }

  def getPop(size: Int): InsnNode = {
    val op = if (size == 1) Opcodes.POP else Opcodes.POP2
    new InsnNode(op)
  }

  def labelReferences(method: MethodNode): Map[LabelNode, Set[AnyRef]] = {
    val res = mutable.Map.empty[LabelNode, Set[AnyRef]]
    def add(l: LabelNode, ref: AnyRef) = if (res contains l) res(l) = res(l) + ref else res(l) = Set(ref)

    method.instructions.iterator().asScala foreach {
      case jump: JumpInsnNode           => add(jump.label, jump)
      case line: LineNumberNode         => add(line.start, line)
      case switch: LookupSwitchInsnNode => switch.labels.asScala.foreach(add(_, switch)); add(switch.dflt, switch)
      case switch: TableSwitchInsnNode  => switch.labels.asScala.foreach(add(_, switch)); add(switch.dflt, switch)
      case _ =>
    }
    if (method.localVariables != null) {
      method.localVariables.iterator().asScala.foreach(l => { add(l.start, l); add(l.end, l) })
    }
    if (method.tryCatchBlocks != null) {
      method.tryCatchBlocks.iterator().asScala.foreach(l => { add(l.start, l); add(l.handler, l); add(l.end, l) })
    }

    res.toMap
  }

  def substituteLabel(reference: AnyRef, from: LabelNode, to: LabelNode): Unit = {
    def substList(list: java.util.List[LabelNode]) = {
      foreachWithIndex(list.asScala.toList) { case (l, i) =>
        if (l == from) list.set(i, to)
      }
    }
    reference match {
      case jump: JumpInsnNode           => jump.label = to
      case line: LineNumberNode         => line.start = to
      case switch: LookupSwitchInsnNode => substList(switch.labels); if (switch.dflt == from) switch.dflt = to
      case switch: TableSwitchInsnNode  => substList(switch.labels); if (switch.dflt == from) switch.dflt = to
      case local: LocalVariableNode     =>
        if (local.start == from) local.start = to
        if (local.end == from) local.end = to
      case handler: TryCatchBlockNode   =>
        if (handler.start == from) handler.start = to
        if (handler.handler == from) handler.handler = to
        if (handler.end == from) handler.end = to
    }
  }
}
