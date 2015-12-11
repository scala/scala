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

  def isNonNullMethodInvocation(mi: MethodInsnNode): Boolean = {
    isJavaBox(mi) || isScalaBox(mi) || isPredefAutoBox(mi) || isRefCreate(mi) || isRefZero(mi)
  }

  private val srBoxesRunTimeName = "scala/runtime/BoxesRunTime"

  private val boxToMethods = Map(
    Type.BOOLEAN -> ("boxToBoolean",   "(Z)Ljava/lang/Boolean;"),
    Type.BYTE    -> ("boxToByte",      "(B)Ljava/lang/Byte;"),
    Type.CHAR    -> ("boxToCharacter", "(C)Ljava/lang/Character;"),
    Type.SHORT   -> ("boxToShort",     "(S)Ljava/lang/Short;"),
    Type.INT     -> ("boxToInteger",   "(I)Ljava/lang/Integer;"),
    Type.LONG    -> ("boxToLong",      "(J)Ljava/lang/Long;"),
    Type.FLOAT   -> ("boxToFloat",     "(F)Ljava/lang/Float;"),
    Type.DOUBLE  -> ("boxToDouble",    "(D)Ljava/lang/Double;"))

  def isScalaBox(insn: MethodInsnNode): Boolean = {
    insn.owner == srBoxesRunTimeName && {
      val args = Type.getArgumentTypes(insn.desc)
      args.length == 1 && (boxToMethods.get(args(0).getSort) match {
        case Some((name, desc)) => name == insn.name && desc == insn.desc
        case _ => false
      })
    }
  }

  def getScalaBox(primitiveType: Type): MethodInsnNode = {
    val (method, desc) = boxToMethods(primitiveType.getSort)
    new MethodInsnNode(INVOKESTATIC, srBoxesRunTimeName, method, desc, /*itf =*/ false)
  }

  private val unboxToMethods = Map(
    Type.BOOLEAN -> ("unboxToBoolean", "(Ljava/lang/Object;)Z"),
    Type.BYTE    -> ("unboxToByte",    "(Ljava/lang/Object;)B"),
    Type.CHAR    -> ("unboxToChar",    "(Ljava/lang/Object;)C"),
    Type.SHORT   -> ("unboxToShort",   "(Ljava/lang/Object;)S"),
    Type.INT     -> ("unboxToInt",     "(Ljava/lang/Object;)I"),
    Type.LONG    -> ("unboxToLong",    "(Ljava/lang/Object;)J"),
    Type.FLOAT   -> ("unboxToFloat",   "(Ljava/lang/Object;)F"),
    Type.DOUBLE  -> ("unboxToDouble",  "(Ljava/lang/Object;)D"))

  def isScalaUnbox(insn: MethodInsnNode): Boolean = {
    insn.owner == srBoxesRunTimeName && (unboxToMethods.get(Type.getReturnType(insn.desc).getSort) match {
      case Some((name, desc)) => name == insn.name && desc == insn.desc
      case _ => false
    })
  }

  def getScalaUnbox(primitiveType: Type): MethodInsnNode = {
    val (method, desc) = unboxToMethods(primitiveType.getSort)
    new MethodInsnNode(INVOKESTATIC, srBoxesRunTimeName, method, desc, /*itf =*/ false)
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

  def isJavaUnbox(insn: MethodInsnNode): Boolean = {
    insn.desc.startsWith("()") && {
      (Type.getReturnType(insn.desc).getSort: @switch) match {
        case Type.BOOLEAN => insn.owner == "java/lang/Boolean"   && insn.name == "booleanValue"
        case Type.BYTE    => insn.owner == "java/lang/Byte"      && insn.name == "byteValue"
        case Type.CHAR    => insn.owner == "java/lang/Character" && insn.name == "charValue"
        case Type.SHORT   => insn.owner == "java/lang/Short"     && insn.name == "shortValue"
        case Type.INT     => insn.owner == "java/lang/Integer"   && insn.name == "intValue"
        case Type.LONG    => insn.owner == "java/lang/Long"      && insn.name == "longValue"
        case Type.FLOAT   => insn.owner == "java/lang/Float"     && insn.name == "floatValue"
        case Type.DOUBLE  => insn.owner == "java/lang/Double"    && insn.name == "doubleValue"
        case _ => false
      }
    }
  }

  def isPredefAutoBox(insn: MethodInsnNode): Boolean = {
    insn.owner == "scala/Predef$" && {
      val args = Type.getArgumentTypes(insn.desc)
      args.length == 1 && ((args(0).getSort: @switch) match {
        case Type.BOOLEAN => insn.name == "boolean2Boolean" && insn.desc == "(Z)Ljava/lang/Boolean;"
        case Type.BYTE    => insn.name == "byte2Byte"       && insn.desc == "(B)Ljava/lang/Byte;"
        case Type.CHAR    => insn.name == "char2Character"  && insn.desc == "(C)Ljava/lang/Character;"
        case Type.SHORT   => insn.name == "short2Short"     && insn.desc == "(S)Ljava/lang/Short;"
        case Type.INT     => insn.name == "int2Integer"     && insn.desc == "(I)Ljava/lang/Integer;"
        case Type.LONG    => insn.name == "long2Long"       && insn.desc == "(J)Ljava/lang/Long;"
        case Type.FLOAT   => insn.name == "float2Float"     && insn.desc == "(F)Ljava/lang/Float;"
        case Type.DOUBLE  => insn.name == "double2Double"   && insn.desc == "(D)Ljava/lang/Double;"
        case _ => false
      })
    }
  }

  def isPredefAutoUnbox(insn: MethodInsnNode): Boolean = {
    insn.owner == "scala/Predef$" && {
      (Type.getReturnType(insn.desc).getSort: @switch) match {
        case Type.BOOLEAN => insn.name == "Boolean2boolean" && insn.desc == "(Ljava/lang/Boolean;)Z"
        case Type.BYTE    => insn.name == "Byte2byte"       && insn.desc == "(Ljava/lang/Byte;)B"
        case Type.CHAR    => insn.name == "Character2char"  && insn.desc == "(Ljava/lang/Character;)C"
        case Type.SHORT   => insn.name == "Short2short"     && insn.desc == "(Ljava/lang/Short;)S"
        case Type.INT     => insn.name == "Integer2int"     && insn.desc == "(Ljava/lang/Integer;)I"
        case Type.LONG    => insn.name == "Long2long"       && insn.desc == "(Ljava/lang/Long;)J"
        case Type.FLOAT   => insn.name == "Float2float"     && insn.desc == "(Ljava/lang/Float;)F"
        case Type.DOUBLE  => insn.name == "Double2double"   && insn.desc == "(Ljava/lang/Double;)D"
        case _ => false
      }
    }
  }

  def isRefCreate(insn: MethodInsnNode): Boolean = {
    insn.name == "create" && {
      val args = Type.getArgumentTypes(insn.desc)
      args.length == 1 && ((args(0).getSort: @switch) match {
        case Type.BOOLEAN => insn.owner == "scala/runtime/BooleanRef" && insn.desc == "(Z)Lscala/runtime/BooleanRef;"                 || insn.owner == "scala/runtime/VolatileBooleanRef" && insn.desc == "(Z)Lscala/runtime/VolatileBooleanRef;"
        case Type.BYTE    => insn.owner == "scala/runtime/ByteRef"    && insn.desc == "(B)Lscala/runtime/ByteRef;"                    || insn.owner == "scala/runtime/VolatileByteRef"    && insn.desc == "(B)Lscala/runtime/VolatileByteRef;"
        case Type.CHAR    => insn.owner == "scala/runtime/CharRef"    && insn.desc == "(C)Lscala/runtime/CharRef;"                    || insn.owner == "scala/runtime/VolatileCharRef"    && insn.desc == "(C)Lscala/runtime/VolatileCharRef;"
        case Type.SHORT   => insn.owner == "scala/runtime/ShortRef"   && insn.desc == "(S)Lscala/runtime/ShortRef;"                   || insn.owner == "scala/runtime/VolatileShortRef"   && insn.desc == "(S)Lscala/runtime/VolatileShortRef;"
        case Type.INT     => insn.owner == "scala/runtime/IntRef"     && insn.desc == "(I)Lscala/runtime/IntRef;"                     || insn.owner == "scala/runtime/VolatileIntRef"     && insn.desc == "(I)Lscala/runtime/VolatileIntRef;"
        case Type.LONG    => insn.owner == "scala/runtime/LongRef"    && insn.desc == "(J)Lscala/runtime/LongRef;"                    || insn.owner == "scala/runtime/VolatileLongRef"    && insn.desc == "(J)Lscala/runtime/VolatileLongRef;"
        case Type.FLOAT   => insn.owner == "scala/runtime/FloatRef"   && insn.desc == "(F)Lscala/runtime/FloatRef;"                   || insn.owner == "scala/runtime/VolatileFloatRef"   && insn.desc == "(F)Lscala/runtime/VolatileFloatRef;"
        case Type.DOUBLE  => insn.owner == "scala/runtime/DoubleRef"  && insn.desc == "(D)Lscala/runtime/DoubleRef;"                  || insn.owner == "scala/runtime/VolatileDoubleRef"  && insn.desc == "(D)Lscala/runtime/VolatileDoubleRef;"
        case Type.OBJECT  => insn.owner == "scala/runtime/ObjectRef"  && insn.desc == "(Ljava/lang/Object;)Lscala/runtime/ObjectRef;" || insn.owner == "scala/runtime/VolatileObjectRef"  && insn.desc == "(Ljava/lang/Object;)Lscala/runtime/VolatileObjectRef;"
        case _ => false
      })
    }
  }

  private val jlObjectType = Type.getType("Ljava/lang/Object;")

  private val runtimeRefClassesAndTypes = Map(
    ("scala/runtime/BooleanRef", Type.BOOLEAN_TYPE),
    ("scala/runtime/ByteRef",    Type.BYTE_TYPE),
    ("scala/runtime/CharRef",    Type.CHAR_TYPE),
    ("scala/runtime/ShortRef",   Type.SHORT_TYPE),
    ("scala/runtime/IntRef",     Type.INT_TYPE),
    ("scala/runtime/LongRef",    Type.LONG_TYPE),
    ("scala/runtime/FloatRef",   Type.FLOAT_TYPE),
    ("scala/runtime/DoubleRef",  Type.DOUBLE_TYPE),
    ("scala/runtime/ObjectRef",  jlObjectType),
    ("scala/runtime/VolatileBooleanRef", Type.BOOLEAN_TYPE),
    ("scala/runtime/VolatileByteRef",    Type.BYTE_TYPE),
    ("scala/runtime/VolatileCharRef",    Type.CHAR_TYPE),
    ("scala/runtime/VolatileShortRef",   Type.SHORT_TYPE),
    ("scala/runtime/VolatileIntRef",     Type.INT_TYPE),
    ("scala/runtime/VolatileLongRef",    Type.LONG_TYPE),
    ("scala/runtime/VolatileFloatRef",   Type.FLOAT_TYPE),
    ("scala/runtime/VolatileDoubleRef",  Type.DOUBLE_TYPE),
    ("scala/runtime/VolatileObjectRef",  jlObjectType))

  def isRefZero(insn: MethodInsnNode): Boolean = {
    insn.name == "zero" && runtimeRefClassesAndTypes.contains(insn.owner) && insn.desc == "()L" + insn.owner + ";"
  }

  def runtimeRefClassBoxedType(refClass: InternalName): Type = runtimeRefClassesAndTypes(refClass)

  def isModuleLoad(insn: AbstractInsnNode, moduleName: InternalName): Boolean = insn match {
    case fi: FieldInsnNode => fi.getOpcode == GETSTATIC && fi.owner == moduleName && fi.name == "MODULE$" && fi.desc == ("L" + moduleName + ";")
    case _ => false
  }

  def isPredefLoad(insn: AbstractInsnNode) = isModuleLoad(insn, "scala/Predef$")

  private val primitiveBoxConstructors = Set(
    "java/lang/Boolean(Z)V",
    "java/lang/Byte(B)V",
    "java/lang/Character(C)V",
    "java/lang/Short(S)V",
    "java/lang/Integer(I)V",
    "java/lang/Long(J)V",
    "java/lang/Float(F)V",
    "java/lang/Double(D)V")

  def isPrimitiveBoxConstructor(insn: MethodInsnNode): Boolean = {
    insn.name == INSTANCE_CONSTRUCTOR_NAME && primitiveBoxConstructors(insn.owner + insn.desc)
  }

  private val runtimeRefConstructors = Set(
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
    "scala/runtime/VolatileDoubleRef(D)V")

  def isRuntimeRefConstructor(insn: MethodInsnNode): Boolean = {
    insn.name == INSTANCE_CONSTRUCTOR_NAME && runtimeRefConstructors(insn.owner + insn.desc)
  }

  private val tupleConstructors = Set.empty[String] ++ {
    (1 to 22).map(n => "scala/Tuple" + n + "(" + ("Ljava/lang/Object;" * n) + ")V")
  } ++ {
    Iterator("I", "J", "D").map(t => "scala/Tuple1$mc" + t + "$sp(" + t + ")V")
  } ++ {
    def tuple2Specs = Iterator("I", "J", "D", "C", "Z")
    for (a <- tuple2Specs; b <- tuple2Specs) yield "scala/Tuple2$mc" + a + b + "$sp(" + a + b + ")V"
  }

  def isTupleConstructor(insn: MethodInsnNode): Boolean = {
    insn.name == INSTANCE_CONSTRUCTOR_NAME && tupleConstructors(insn.owner + insn.desc)
  }

  // unused objects created by these constructors are eliminated by pushPop
  private val sideEffectFreeConstructors = primitiveBoxConstructors ++
    runtimeRefConstructors ++
    tupleConstructors ++ Set(
    "java/lang/Object()V",
    "java/lang/String()V",
    "java/lang/String(Ljava/lang/String;)V",
    "java/lang/String([C)V"
  )

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
    def ijfdzv = Iterator("I", "J", "F", "D", "Z", "V")
    def ijd = Iterator("I", "J", "D")
    Set.empty[String] ++ {
      (0 to 22).map(base + _)
    } ++ {
      primitives.map(base + "0$mc" + _ + "$sp") // Function0
    } ++ {
      // return type specializations appear first in the name string (alphabetical sorting)
      for (r <- ijfdzv; a <- ijfd) yield base + "1$mc" + r + a + "$sp" // Function1
    } ++ {
      for (r <- ijfdzv; a <- ijd; b <- ijd) yield base + "2$mc" + r + a + b + "$sp" // Function2
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
