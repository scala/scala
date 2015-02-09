/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.{tailrec, switch}
import scala.collection.mutable
import scala.reflect.internal.util.Collections._
import scala.tools.asm.tree.analysis._
import scala.tools.asm.{MethodWriter, ClassWriter, Label, Opcodes}
import scala.tools.asm.tree._
import scala.collection.convert.decorateAsScala._
import GenBCode._
import scala.collection.convert.decorateAsScala._
import scala.collection.convert.decorateAsJava._
import scala.tools.nsc.backend.jvm.BTypes._

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

  def isConstructor(methodNode: MethodNode): Boolean = {
    methodNode.name == INSTANCE_CONSTRUCTOR_NAME || methodNode.name == CLASS_CONSTRUCTOR_NAME
  }

  def isStaticMethod(methodNode: MethodNode): Boolean = (methodNode.access & Opcodes.ACC_STATIC) != 0

  def isAbstractMethod(methodNode: MethodNode): Boolean = (methodNode.access & Opcodes.ACC_ABSTRACT) != 0

  def isSynchronizedMethod(methodNode: MethodNode): Boolean = (methodNode.access & Opcodes.ACC_SYNCHRONIZED) != 0

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

  /**
   * In order to run an Analyzer, the maxLocals / maxStack fields need to be available. The ASM
   * framework only computes these values during bytecode generation.
   *
   * Since there's currently no better way, we run a bytecode generator on the method and extract
   * the computed values. This required changes to the ASM codebase:
   *   - the [[MethodWriter]] class was made public
   *   - accessors for maxLocals / maxStack were added to the MethodWriter class
   *
   * We could probably make this faster (and allocate less memory) by hacking the ASM framework
   * more: create a subclass of MethodWriter with a /dev/null byteVector. Another option would be
   * to create a separate visitor for computing those values, duplicating the functionality from the
   * MethodWriter.
   */
  def computeMaxLocalsMaxStack(method: MethodNode) {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    val excs = method.exceptions.asScala.toArray
    val mw = cw.visitMethod(method.access, method.name, method.desc, method.signature, excs).asInstanceOf[MethodWriter]
    method.accept(mw)
    method.maxLocals = mw.getMaxLocals
    method.maxStack = mw.getMaxStack
  }

  def removeLineNumberNodes(classNode: ClassNode): Unit = {
    for (m <- classNode.methods.asScala) removeLineNumberNodes(m.instructions)
  }

  def removeLineNumberNodes(instructions: InsnList): Unit = {
    val iter = instructions.iterator()
    while (iter.hasNext) iter.next() match {
      case _: LineNumberNode => iter.remove()
      case _ =>
    }
  }

  def cloneLabels(methodNode: MethodNode): Map[LabelNode, LabelNode] = {
    methodNode.instructions.iterator().asScala.collect({
      case labelNode: LabelNode => (labelNode, newLabelNode)
    }).toMap
  }

  /**
   * Create a new [[LabelNode]] with a correctly associated [[Label]].
   */
  def newLabelNode: LabelNode = {
    val label = new Label
    val labelNode = new LabelNode(label)
    label.info = labelNode
    labelNode
  }

  /**
   * Clone the instructions in `methodNode` into a new [[InsnList]], mapping labels according to
   * the `labelMap`. Returns the new instruction list and a map from old to new instructions.
   */
  def cloneInstructions(methodNode: MethodNode, labelMap: Map[LabelNode, LabelNode]): (InsnList, Map[AbstractInsnNode, AbstractInsnNode]) = {
    val javaLabelMap = labelMap.asJava
    val result = new InsnList
    var map = Map.empty[AbstractInsnNode, AbstractInsnNode]
    for (ins <- methodNode.instructions.iterator.asScala) {
      val cloned = ins.clone(javaLabelMap)
      result add cloned
      map += ((ins, cloned))
    }
    (result, map)
  }

  /**
   * Clone the local variable descriptors of `methodNode` and map their `start` and `end` labels
   * according to the `labelMap`.
   */
  def cloneLocalVariableNodes(methodNode: MethodNode, labelMap: Map[LabelNode, LabelNode], prefix: String): List[LocalVariableNode] = {
    methodNode.localVariables.iterator().asScala.map(localVariable => new LocalVariableNode(
      prefix + localVariable.name,
      localVariable.desc,
      localVariable.signature,
      labelMap(localVariable.start),
      labelMap(localVariable.end),
      localVariable.index
    )).toList
  }

  /**
   * Clone the local try/catch blocks of `methodNode` and map their `start` and `end` and `handler`
   * labels according to the `labelMap`.
   */
  def cloneTryCatchBlockNodes(methodNode: MethodNode, labelMap: Map[LabelNode, LabelNode]): List[TryCatchBlockNode] = {
    methodNode.tryCatchBlocks.iterator().asScala.map(tryCatch => new TryCatchBlockNode(
      labelMap(tryCatch.start),
      labelMap(tryCatch.end),
      labelMap(tryCatch.handler),
      tryCatch.`type`
    )).toList
  }

  class BasicAnalyzer(methodNode: MethodNode, classInternalName: InternalName) {
    val analyzer = new Analyzer(new BasicInterpreter)
    analyzer.analyze(classInternalName, methodNode)
    def frameAt(instruction: AbstractInsnNode): Frame[BasicValue] = analyzer.getFrames()(methodNode.instructions.indexOf(instruction))
  }

  implicit class `frame extensions`[V <: Value](val frame: Frame[V]) extends AnyVal {
    def peekDown(n: Int): V = {
      val topIndex = frame.getStackSize - 1
      frame.getStack(topIndex - n)
    }
  }
}
