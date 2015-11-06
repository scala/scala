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
import scala.tools.asm.commons.CodeSizeEvaluator
import scala.tools.asm.tree.analysis._
import scala.tools.asm.{Label, Type}
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree._
import GenBCode._
import scala.collection.convert.decorateAsScala._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.analysis.InstructionStackEffect

object BytecodeUtils {

  // http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.9.1
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

  def isExecutable(instruction: AbstractInsnNode): Boolean = instruction.getOpcode >= 0

  def isConstructor(methodNode: MethodNode): Boolean = {
    methodNode.name == INSTANCE_CONSTRUCTOR_NAME || methodNode.name == CLASS_CONSTRUCTOR_NAME
  }

  def isStaticMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_STATIC) != 0

  def isAbstractMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_ABSTRACT) != 0

  def isSynchronizedMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_SYNCHRONIZED) != 0

  def isNativeMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_NATIVE) != 0

  def hasCallerSensitiveAnnotation(methodNode: MethodNode) = methodNode.visibleAnnotations != null && methodNode.visibleAnnotations.asScala.exists(_.desc == "Lsun/reflect/CallerSensitive;")

  def isFinalClass(classNode: ClassNode): Boolean = (classNode.access & ACC_FINAL) != 0

  def isFinalMethod(methodNode: MethodNode): Boolean = (methodNode.access & (ACC_FINAL | ACC_PRIVATE | ACC_STATIC)) != 0

  def isStrictfpMethod(methodNode: MethodNode): Boolean = (methodNode.access & ACC_STRICT) != 0

  def isReference(t: Type) = t.getSort == Type.OBJECT || t.getSort == Type.ARRAY

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
    if ((op >= IFEQ && op <= IFLE) || op == IFNULL || op == IFNONNULL) {
      instructions.insert(jump, getPop(1))
    } else if ((op >= IF_ICMPEQ && op <= IF_ICMPLE) || op == IF_ACMPEQ || op == IF_ACMPNE) {
      instructions.insert(jump, getPop(1))
      instructions.insert(jump, getPop(1))
    } else {
      // we can't remove JSR: its execution does not only jump, it also adds a return address to the stack
      assert(jump.getOpcode == GOTO)
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

  /**
   * The number of local variable slots used for parameters and for the `this` reference.
   */
  def parametersSize(methodNode: MethodNode): Int = {
    (Type.getArgumentsAndReturnSizes(methodNode.desc) >> 2) - (if (isStaticMethod(methodNode)) 1 else 0)
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

  def codeSizeOKForInlining(caller: MethodNode, callee: MethodNode): Boolean = {
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
   * Clone the local variable descriptors of `methodNode` and map their `start` and `end` labels
   * according to the `labelMap`.
   */
  def cloneLocalVariableNodes(methodNode: MethodNode, labelMap: Map[LabelNode, LabelNode], prefix: String, shift: Int): List[LocalVariableNode] = {
    methodNode.localVariables.iterator().asScala.map(localVariable => new LocalVariableNode(
      prefix + localVariable.name,
      localVariable.desc,
      localVariable.signature,
      labelMap(localVariable.start),
      labelMap(localVariable.end),
      localVariable.index + shift
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

  def isSideEffectFreeCall(insn: MethodInsnNode): Boolean = {
    isScalaBox(insn) || isScalaUnbox(insn) ||
      isJavaBox(insn) || // not java unbox, it may NPE
      isSideEffectFreeConstructorCall(insn)
  }

  private val srBoxesRuntimeName = "scala/runtime/BoxesRunTime"

  def isScalaBox(insn: MethodInsnNode): Boolean = {
    insn.owner == srBoxesRuntimeName && {
      val args = Type.getArgumentTypes(insn.desc)
      args.length == 1 && ((args(0).getSort: @switch) match {
        case Type.BOOLEAN => insn.name == "boxToBoolean"   && insn.desc == "(Z)Ljava/lang/Boolean;"
        case Type.BYTE    => insn.name == "boxToByte"      && insn.desc == "(B)Ljava/lang/Byte;"
        case Type.CHAR    => insn.name == "boxToCharacter" && insn.desc == "(C)Ljava/lang/Character;"
        case Type.SHORT   => insn.name == "boxToShort"     && insn.desc == "(S)Ljava/lang/Short;"
        case Type.INT     => insn.name == "boxToInteger"   && insn.desc == "(I)Ljava/lang/Integer;"
        case Type.LONG    => insn.name == "boxToLong"      && insn.desc == "(J)Ljava/lang/Long;"
        case Type.FLOAT   => insn.name == "boxToFloat"     && insn.desc == "(F)Ljava/lang/Float;"
        case Type.DOUBLE  => insn.name == "boxToDouble"    && insn.desc == "(D)Ljava/lang/Double;"
        case _ => false
      })
    }
  }

  def isScalaUnbox(insn: MethodInsnNode): Boolean = {
    insn.owner == srBoxesRuntimeName && ((Type.getReturnType(insn.desc).getSort: @switch) match {
      case Type.BOOLEAN => insn.name == "unboxToBoolean" && insn.desc == "(Ljava/lang/Object;)Z"
      case Type.BYTE    => insn.name == "unboxToByte"   && insn.desc == "(Ljava/lang/Object;)B"
      case Type.CHAR    => insn.name == "unboxToChar"   && insn.desc == "(Ljava/lang/Object;)C"
      case Type.SHORT   => insn.name == "unboxToShort"  && insn.desc == "(Ljava/lang/Object;)S"
      case Type.INT     => insn.name == "unboxToInt"    && insn.desc == "(Ljava/lang/Object;)I"
      case Type.LONG    => insn.name == "unboxToLong"   && insn.desc == "(Ljava/lang/Object;)J"
      case Type.FLOAT   => insn.name == "unboxToFloat"  && insn.desc == "(Ljava/lang/Object;)F"
      case Type.DOUBLE  => insn.name == "unboxToDouble" && insn.desc == "(Ljava/lang/Object;)D"
      case _ => false
    })
  }

  def isJavaBox(insn: MethodInsnNode): Boolean = {
    insn.name == "valueOf" && {
      val args = Type.getArgumentTypes(insn.desc)
      args.length == 1 && ((args(0).getSort: @switch) match {
        case Type.BOOLEAN => insn.owner == "java/lang/Boolean"   && insn.desc == "(Z)Ljava/lang/Boolean;"
        case Type.BYTE    => insn.owner == "java/lang/Byte"      && insn.desc == "(B)Ljava/lang/Byte;"
        case Type.CHAR    => insn.owner == "java/lang/Character" && insn.desc == "(C)Ljava/lang/Character;"
        case Type.SHORT   => insn.owner == "java/lang/Short"     && insn.desc == "(S)Ljava/lang/Short;"
        case Type.INT     => insn.owner == "java/lang/Integer"   && insn.desc == "(I)Ljava/lang/Integer;"
        case Type.LONG    => insn.owner == "java/lang/Long"      && insn.desc == "(J)Ljava/lang/Long;"
        case Type.FLOAT   => insn.owner == "java/lang/Float"     && insn.desc == "(F)Ljava/lang/Float;"
        case Type.DOUBLE  => insn.owner == "java/lang/Double"    && insn.desc == "(D)Ljava/lang/Double;"
        case _ => false
      })
    }
  }

  // unused objects created by these constructors are eliminated by pushPop
  private val sideEffectFreeConstructors = Set(
    "java/lang/Object()V",
    "java/lang/String()V",
    "java/lang/String(Ljava/lang/String;)V",
    "java/lang/String([C)V",

    "java/lang/Boolean(Z)V",
    "java/lang/Byte(B)V",
    "java/lang/Character(C)V",
    "java/lang/Short(S)V",
    "java/lang/Integer(I)V",
    "java/lang/Long(J)V",
    "java/lang/Float(F)V",
    "java/lang/Double(D)V",

    "scala/runtime/ObjectRef(Ljava/lang/Object;)V",
    "scala/runtime/BooleanRef(Z)V",
    "scala/runtime/ByteRef(B)V",
    "scala/runtime/CharRef(C)V",
    "scala/runtime/ShortRef(S)V",
    "scala/runtime/IntRef(I)V",
    "scala/runtime/LongRef(J)V",
    "scala/runtime/FloatRef(F)V",
    "scala/runtime/DoubleRef(D)V",

    "scala/runtime/VolatileObjectRef(Ljava/lang/Object;)V",
    "scala/runtime/VolatileBooleanRef(Z)V",
    "scala/runtime/VolatileByteRef(B)V",
    "scala/runtime/VolatileCharRef(C)V",
    "scala/runtime/VolatileShortRef(S)V",
    "scala/runtime/VolatileIntRef(I)V",
    "scala/runtime/VolatileLongRef(J)V",
    "scala/runtime/VolatileFloatRef(F)V",
    "scala/runtime/VolatileDoubleRef(D)V"
  ) ++ {
    (1 to 22).map(n => "scala/Tuple" + n + "(" + ("Ljava/lang/Object;" * n) + ")V")
  } ++ {
    Iterator("I", "J", "D").map(t => "scala/Tuple1$mc" + t + "$sp(" + t + ")V")
  } ++ {
    def tuple2Specs = Iterator("I", "J", "D", "C", "Z")
    for (a <- tuple2Specs; b <- tuple2Specs) yield "scala/Tuple2$mc" + a + b + "$sp(" + a + b + ")V"
  }

  def isSideEffectFreeConstructorCall(insn: MethodInsnNode): Boolean = {
    insn.name == INSTANCE_CONSTRUCTOR_NAME && sideEffectFreeConstructors(insn.owner + insn.desc)
  }

  private val classesForSideEffectFreeConstructors = sideEffectFreeConstructors.map(s => s.substring(0, s.indexOf('(')))

  // we only eliminate `NEW C` if the class C has a constructor that we consider side-effect free.
  // removing a `NEW` eliminates a potential NoClassDefFoundError, so we only do it for core classes.
  def isNewForSideEffectFreeConstructor(insn: AbstractInsnNode) = {
    insn.getOpcode == NEW && {
      val ti = insn.asInstanceOf[TypeInsnNode]
      classesForSideEffectFreeConstructors(ti.desc)
    }
  }

  def isBoxedUnit(insn: AbstractInsnNode) = {
    insn.getOpcode == GETSTATIC && {
      val fi = insn.asInstanceOf[FieldInsnNode]
      fi.owner == "scala/runtime/BoxedUnit" && fi.name == "UNIT" && fi.desc == "Lscala/runtime/BoxedUnit;"
    }
  }

  private def buildFunctionTypes(base: String): Set[InternalName] = {
    def primitives = Iterator("B", "S", "I", "J", "C", "F", "D", "Z", "V")
    def ijfd = Iterator("I", "J", "F", "D")
    def ijd = Iterator("I", "J", "D")
    Set.empty[String] ++ {
      (0 to 22).map(base + _)
    } ++ {
      primitives.map(base + "0$mc" + _ + "$sp") // Function0
    } ++ {
      for (a <- ijfd; b <- ijfd) yield base + "1$mc" + a + b + "$sp" // Function1
    } ++ {
      for (a <- ijd; b <- ijd; c <- ijd) yield base + "2$mc" + a + b + c + "$sp" // Function2
    }
  }

  private val srJFunctionTypes: Set[InternalName] = buildFunctionTypes("scala/runtime/java8/JFunction")
  def isrJFunctionType(internalName: InternalName): Boolean = srJFunctionTypes(internalName)

  private val sFunctionTypes: Set[InternalName] = buildFunctionTypes("scala/Function")
  def isScalaFunctionType(internalName: InternalName): Boolean = sFunctionTypes(internalName)

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
