/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.Collections._
import scala.tools.asm.Opcodes._
import scala.tools.asm.commons.CodeSizeEvaluator
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis._
import scala.tools.asm.{Label, Type}
import scala.tools.nsc.backend.jvm.GenBCode._
import scala.tools.nsc.backend.jvm.analysis.InstructionStackEffect

object BytecodeUtils {

  // https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.9.1
  final val maxJVMMethodSize = 65535

  // 5% margin, more than enough for the instructions added by the inliner (store / load args, null check for instance methods)
  final val maxMethodSizeAfterInline = maxJVMMethodSize - (maxJVMMethodSize / 20)

  object Goto {
    def unapply(instruction: AbstractInsnNode): Option[JumpInsnNode] = {
      if (instruction.getOpcode == GOTO) Some(instruction.asInstanceOf[JumpInsnNode])
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
    def unapply(instruction: AbstractInsnNode): Option[(AbstractInsnNode, Int)] = {
      if (isLoadStoreOrRet(instruction)) Some((instruction, instruction.asInstanceOf[VarInsnNode].`var`))
      else if (instruction.getOpcode == IINC) Some((instruction, instruction.asInstanceOf[IincInsnNode].`var`))
      else None
    }

  }

  def isJumpNonJsr(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    // JSR is deprecated in classfile version 50, disallowed in 51. historically, it was used to implement finally.
    op == GOTO || isConditionalJump(instruction)
  }

  def isConditionalJump(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    (op >= IFEQ && op <= IF_ACMPNE) || op == IFNULL || op == IFNONNULL
  }

  def isReturn(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    op >= IRETURN && op <= RETURN
  }

  def isLoad(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    op >= ILOAD  && op <= ALOAD
  }

  def isStore(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    op >= ISTORE && op <= ASTORE
  }

  def isLoadStoreOrRet(instruction: AbstractInsnNode): Boolean = isLoad(instruction) || isStore(instruction) || instruction.getOpcode == RET

  def isLoadOrStore(instruction: AbstractInsnNode): Boolean = isLoad(instruction) || isStore(instruction)

  def isStaticCall(instruction: AbstractInsnNode): Boolean = {
    instruction.getOpcode == INVOKESTATIC
  }

  def isVirtualCall(instruction: AbstractInsnNode): Boolean = {
    val op = instruction.getOpcode
    // invokespecial
    op == INVOKESPECIAL || op == INVOKEVIRTUAL || op == INVOKEINTERFACE
  }

  def isCall(instruction: AbstractInsnNode): Boolean = {
    isStaticCall(instruction) || isVirtualCall(instruction)
  }

  def isExecutable(instruction: AbstractInsnNode): Boolean = instruction.getOpcode >= 0

  def isConstructor(methodNode: MethodNode): Boolean = {
    methodNode.name == INSTANCE_CONSTRUCTOR_NAME || methodNode.name == CLASS_CONSTRUCTOR_NAME
  }

  def isPublicMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_PUBLIC) != 0

  def isPrivateMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_PRIVATE) != 0

  def isStaticMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_STATIC) != 0

  def isAbstractMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_ABSTRACT) != 0

  def isSynchronizedMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_SYNCHRONIZED) != 0

  def isNativeMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_NATIVE) != 0

  def isVarargsMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_VARARGS) != 0

  def isSyntheticMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_SYNTHETIC) != 0

  // cross-jdk
  def hasCallerSensitiveAnnotation(methodNode: MethodNode): Boolean =
    methodNode.visibleAnnotations != null &&
    methodNode.visibleAnnotations.stream.filter(ann =>
      ann.desc == "Lsun/reflect/CallerSensitive;" || ann.desc == "Ljdk/internal/reflect/CallerSensitive;"
    ).findFirst.isPresent

  def isFinalClass(classNode: ClassNode): Boolean = (classNode.access & ACC_FINAL) != 0

  def isInterface(classNode: ClassNode): Boolean = (classNode.access & ACC_INTERFACE) != 0

  def isFinalMethod(methodNode: MethodNode): Boolean = (methodNode.access & (ACC_FINAL | ACC_PRIVATE | ACC_STATIC)) != 0

  def isStrictfpMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_STRICT) != 0

  def isReference(t: Type): Boolean = t.getSort == Type.OBJECT || t.getSort == Type.ARRAY

  /** Find the nearest preceding node to `insn` which is executable (i.e., not a label / line number)
    * and which is not selected by `stopBefore`. */
  @tailrec def previousExecutableInstruction(insn: AbstractInsnNode, stopBefore: AbstractInsnNode => Boolean = Set()): Option[AbstractInsnNode] = {
    val prev = insn.getPrevious
    if (prev == null || stopBefore(insn)) None
    else if (isExecutable(prev)) Some(prev)
    else previousExecutableInstruction(prev, stopBefore)
  }

  @tailrec def previousLineNumber(insn: AbstractInsnNode): Option[Int] = {
    val prev = insn.getPrevious
    prev match {
      case null => None
      case line: LineNumberNode => Some(line.line)
      case _ => previousLineNumber(prev)
    }
  }

  @tailrec def nextExecutableInstruction(insn: AbstractInsnNode, alsoKeep: AbstractInsnNode => Boolean = Set()): Option[AbstractInsnNode] = {
    val next = insn.getNext
    if (next == null || isExecutable(next) || alsoKeep(next)) Option(next)
    else nextExecutableInstruction(next, alsoKeep)
  }

  @tailrec def nextExecutableInstructionOrLabel(insn: AbstractInsnNode): Option[AbstractInsnNode] = {
    val next = insn.getNext
    if (next == null || isExecutable(next) || next.isInstanceOf[LabelNode]) Option(next)
    else nextExecutableInstructionOrLabel(next)
  }

  def findSingleCall(method: MethodNode, such: MethodInsnNode => Boolean): Option[MethodInsnNode] = {
    @tailrec def noMoreInvoke(insn: AbstractInsnNode): Boolean = {
      insn == null || (!insn.isInstanceOf[MethodInsnNode] && noMoreInvoke(insn.getNext))
    }
    @tailrec def find(insn: AbstractInsnNode): Option[MethodInsnNode] = {
      if (insn == null) None
      else insn match {
        case mi: MethodInsnNode =>
          if (such(mi) && noMoreInvoke(insn.getNext)) Some(mi)
          else None
        case _ =>
          find(insn.getNext)
      }
    }
    find(method.instructions.getFirst)
  }

  def sameTargetExecutableInstruction(a: JumpInsnNode, b: JumpInsnNode): Boolean = {
    // Compare next executable instead of the labels. Identifies a, b as the same target:
    //   LabelNode(a)
    //   LabelNode(b)
    //   Instr
    nextExecutableInstruction(a.label) == nextExecutableInstruction(b.label)
  }

  def removeJumpAndAdjustStack(method: MethodNode, jump: JumpInsnNode): Unit = {
    val instructions = method.instructions
    val op = jump.getOpcode
    if ((op >= IFEQ && op <= IFLE) || op == IFNULL || op == IFNONNULL) {
      instructions.insert(jump, getPop(1))
    } else if ((op >= IF_ICMPEQ && op <= IF_ICMPLE) || op == IF_ACMPEQ || op == IF_ACMPNE) {
      instructions.insert(jump, getPop(1))
      instructions.insert(jump, getPop(1))
    } else {
      // we can't remove JSR: its execution does not only jump, it also adds a return address to the stack
      assert(jump.getOpcode == GOTO, s"Cannot remove JSR instruction in ${method.name} (at ${method.instructions.indexOf(jump)}")
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
    case IFEQ      => IFNE
    case IFNE      => IFEQ

    case IFLT      => IFGE
    case IFGE      => IFLT

    case IFGT      => IFLE
    case IFLE      => IFGT

    case IF_ICMPEQ => IF_ICMPNE
    case IF_ICMPNE => IF_ICMPEQ

    case IF_ICMPLT => IF_ICMPGE
    case IF_ICMPGE => IF_ICMPLT

    case IF_ICMPGT => IF_ICMPLE
    case IF_ICMPLE => IF_ICMPGT

    case IF_ACMPEQ => IF_ACMPNE
    case IF_ACMPNE => IF_ACMPEQ

    case IFNULL    => IFNONNULL
    case IFNONNULL => IFNULL
  }

  def isSize2LoadOrStore(opcode: Int): Boolean = (opcode: @switch) match {
    case LLOAD | DLOAD | LSTORE | DSTORE => true
    case _ => false
  }

  def getPop(size: Int): InsnNode = {
    val op = if (size == 1) POP else POP2
    new InsnNode(op)
  }

  def instructionResultSize(insn: AbstractInsnNode) = InstructionStackEffect.prod(InstructionStackEffect.forClassfile(insn))

  def loadZeroForTypeSort(sort: Int) = (sort: @switch) match {
    case Type.BOOLEAN |
         Type.BYTE |
         Type.CHAR |
         Type.SHORT |
         Type.INT => new InsnNode(ICONST_0)
    case Type.LONG => new InsnNode(LCONST_0)
    case Type.FLOAT => new InsnNode(FCONST_0)
    case Type.DOUBLE => new InsnNode(DCONST_0)
    case Type.OBJECT => new InsnNode(ACONST_NULL)
  }

  /**
   * The number of local variable slots used for parameters and for the `this` reference.
   */
  def parametersSize(methodNode: MethodNode): Int = {
    (Type.getArgumentsAndReturnSizes(methodNode.desc) >> 2) - (if (isStaticMethod(methodNode)) 1 else 0)
  }

  def substituteLabel(reference: AnyRef, from: LabelNode, to: LabelNode): Unit = {
    def substList(list: java.util.List[LabelNode]) = {
      foreachWithIndex(list.asScala.toList) { case (l, i) =>
        if (l == from) list.set(i, to)
      }
    }
    (reference: @unchecked) match {
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

  def callsiteTooLargeAfterInlining(caller: MethodNode, callee: MethodNode): Boolean = {
    // Looking at the implementation of CodeSizeEvaluator, all instructions except tableswitch and
    // lookupswitch are <= 8 bytes. These should be rare enough for 8 to be an OK rough upper bound.
    def roughUpperBound(methodNode: MethodNode): Int = methodNode.instructions.size * 8

    def maxSize(methodNode: MethodNode): Int = {
      val eval = new CodeSizeEvaluator(null)
      methodNode.accept(eval)
      eval.getMaxSize
    }

    (roughUpperBound(caller) + roughUpperBound(callee) > maxMethodSizeAfterInline) &&
      (maxSize(caller) + maxSize(callee) > maxMethodSizeAfterInline)
  }

  def cloneLabels(methodNode: MethodNode): Map[LabelNode, LabelNode] = {
    methodNode.instructions.iterator.asScala.collect({
      case labelNode: LabelNode => (labelNode, newLabelNode)
    }).toMap
  }

  /**
   * Create a new [[LabelNode]] with a correctly associated [[Label]].
   */
  def newLabelNode: LabelNode = {
    val label = new Label
    val labelNode = new LabelNode1(label)
    label.info = labelNode
    labelNode
  }

  /**
   * Clone the local variable descriptors of `methodNode` and map their `start` and `end` labels
   * according to the `labelMap`.
   */
  def cloneLocalVariableNodes(methodNode: MethodNode, labelMap: Map[LabelNode, LabelNode], calleeMethodName: String, localIndexMap: Int => Int): List[LocalVariableNode] = {
    val res = mutable.ListBuffer.empty[LocalVariableNode]
    for (localVariable <- methodNode.localVariables.iterator.asScala) {
      val newIdx = localIndexMap(localVariable.index)
      if (newIdx >= 0) {
        val name =
          if (calleeMethodName.length + localVariable.name.length < BTypes.InlinedLocalVariablePrefixMaxLength) {
            calleeMethodName + "_" + localVariable.name
          } else {
            val parts = localVariable.name.split("_").toVector
            val (methNames, varName) = (calleeMethodName +: parts.init, parts.last)
            // keep at least 5 characters per method name
            val maxNumMethNames = BTypes.InlinedLocalVariablePrefixMaxLength / 5
            val usedMethNames =
              if (methNames.length < maxNumMethNames) methNames
              else {
                val half = maxNumMethNames / 2
                methNames.take(half) ++ methNames.takeRight(half)
              }
            val charsPerMethod = BTypes.InlinedLocalVariablePrefixMaxLength / usedMethNames.length
            usedMethNames.foldLeft("")((res, methName) => res + methName.take(charsPerMethod) + "_") + varName
          }
        res += new LocalVariableNode(
          name,
          localVariable.desc,
          localVariable.signature,
          labelMap(localVariable.start),
          labelMap(localVariable.end),
          newIdx)
      }
    }
    res.toList
  }

  /**
   * Clone the local try/catch blocks of `methodNode` and map their `start` and `end` and `handler`
   * labels according to the `labelMap`.
   */
  def cloneTryCatchBlockNodes(methodNode: MethodNode, labelMap: Map[LabelNode, LabelNode]): List[TryCatchBlockNode] = {
    methodNode.tryCatchBlocks.iterator.asScala.map(tryCatch => new TryCatchBlockNode(
      labelMap(tryCatch.start),
      labelMap(tryCatch.end),
      labelMap(tryCatch.handler),
      tryCatch.`type`
    )).toList
  }

  /**
   * This method is used by optimizer components to eliminate phantom values of instruction
   * that load a value of type `Nothing$` or `Null$`. Such values on the stack don't interact well
   * with stack map frames.
   *
   * For example, `opt.getOrElse(throw e)` is re-written to an invocation of the lambda body, a
   * method with return type `Nothing$`. Similarly for `opt.getOrElse(null)` and `Null$`.
   *
   * During bytecode generation this is handled by BCodeBodyBuilder.adapt. See the comment in that
   * method which explains the issue with such phantom values.
   */
  def fixLoadedNothingOrNullValue(loadedType: Type, loadInstr: AbstractInsnNode, methodNode: MethodNode, bTypes: BTypes): Unit = {
    if (loadedType == bTypes.coreBTypes.srNothingRef.toASMType) {
      methodNode.instructions.insert(loadInstr, new InsnNode(ATHROW))
    } else if (loadedType == bTypes.coreBTypes.srNullRef.toASMType) {
      methodNode.instructions.insert(loadInstr, new InsnNode(ACONST_NULL))
      methodNode.instructions.insert(loadInstr, new InsnNode(POP))
    }
  }

  implicit class AnalyzerExtensions[V <: Value](val analyzer: Analyzer[V]) extends AnyVal {
    def frameAt(instruction: AbstractInsnNode, methodNode: MethodNode): Frame[V] = analyzer.getFrames()(methodNode.instructions.indexOf(instruction))
  }

  implicit class FrameExtensions[V <: Value](val frame: Frame[V]) extends AnyVal {
    /**
     * The value `n` positions down the stack.
     */
    def peekStack(n: Int): V = frame.getStack(frame.getStackSize - 1 - n)

    /**
     * The index of the current stack top.
     */
    def stackTop = frame.getLocals + frame.getStackSize - 1

    /**
     * Gets the value at slot i, where i may be a local or a stack index.
     */
    def getValue(i: Int): V = {
      if (i < frame.getLocals) frame.getLocal(i)
      else frame.getStack(i - frame.getLocals)
    }

    /**
     * Sets the value at slot i, where i may be a local or a stack index.
     */
    def setValue(i: Int, value: V): Unit = {
      if (i < frame.getLocals) frame.setLocal(i, value)
      else frame.setStack(i - frame.getLocals, value)
    }
  }
}
