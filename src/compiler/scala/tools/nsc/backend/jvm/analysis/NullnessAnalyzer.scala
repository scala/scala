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
package analysis

import java.util

import scala.annotation.switch
import scala.tools.asm.tree.analysis._
import scala.tools.asm.tree._
import scala.tools.asm.{Opcodes, Type}
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._
import scala.tools.nsc.backend.jvm.analysis.BackendUtils._

/**
 * See the package object `analysis` for details on the ASM analysis framework.
 *
 * Some notes on nullness analysis.
 *
 * For an instance method, `this` is non-null at entry. So we have to return a NotNull value when
 * the analyzer is initializing the first frame of a method (see above). This required a change of
 * the analyzer: before it would simply call `interpreter.newValue`, where we don't have the
 * required context. See https://github.com/scala/scala-asm/commit/8133d75032.
 *
 * After some operations we know that a certain value is not null (e.g. the receiver of an instance
 * call). However, the receiver is an value on the stack and consumed while interpreting the
 * instruction - so we can only gain some knowledge if we know that the receiver was an alias of
 * some other local variable or stack slot. Therefore we use the AliasingFrame class.
 */

/**
 * Represents the nullness state for a local variable or stack value.
 *
 * Note that nullness of primitive values is not tracked, it will be always unknown.
 */
sealed abstract class NullnessValue(final val isSize2: Boolean) extends Value {
  /**
   * The size of the slot described by this value. Cannot be 0 because no values are allocated
   * for void-typed slots, see NullnessInterpreter.newValue.
   **/
  def getSize: Int = if (isSize2) 2 else 1

  def merge(other: NullnessValue) = {
    if (this eq other) this
    else if (this eq UnknownValue2) this // the only possible value of size two
    else UnknownValue1
  }

  final override def equals(other: Any) = this eq other.asInstanceOf[Object]

  def invert: NullnessValue = if (this == NullValue) NotNullValue else if (this == NotNullValue) NullValue else this
}

object NullValue     extends NullnessValue(isSize2 = false) { override def toString = "Null"     }
object UnknownValue1 extends NullnessValue(isSize2 = false) { override def toString = "Unknown1" }
object UnknownValue2 extends NullnessValue(isSize2 = true ) { override def toString = "Unknown2" }
object NotNullValue  extends NullnessValue(isSize2 = false) { override def toString = "NotNull"  }

object NullnessValue {
  def unknown(isSize2: Boolean) = if (isSize2) UnknownValue2 else UnknownValue1
  def unknown(insn: AbstractInsnNode) = if (instructionResultSize(insn) == 2) UnknownValue2 else UnknownValue1
}

final class NullnessInterpreter(knownNonNullInvocation: MethodInsnNode => Boolean, modulesNonNull: Boolean, method: MethodNode) extends Interpreter[NullnessValue](Opcodes.ASM5) {
  def newValue(tp: Type): NullnessValue = {
    // ASM loves giving semantics to null. The behavior here is the same as in SourceInterpreter,
    // which is provided by the framework.
    //
    // (1) For the void type, the ASM framework expects newValue to return `null`.
    //     Also, the Frame.returnValue field is `null` for methods with return type void.
    //     Example callsite passing VOID_TYPE: in Analyzer, `newValue(Type.getReturnType(m.desc))`.
    //
    // (2) `tp` may also be `null`. When creating the initial frame, the analyzer invokes
    //     `newValue(null)` for each local variable. We have to return a value of size 1.
    if (tp == Type.VOID_TYPE) null // (1)
    else NullnessValue.unknown(isSize2 = tp != null /*(2)*/ && tp.getSize == 2 )
  }

  override def newParameterValue(isInstanceMethod: Boolean, local: Int, tp: Type): NullnessValue = {
    // For instance methods, the `this` parameter is known to be not null.
    val isThis = local == 0 && (isInstanceMethod || {
      method.parameters != null && !method.parameters.isEmpty && {
        val p = method.parameters.get(0)
        (p.access & Opcodes.ACC_SYNTHETIC) != 0 && p.name == s"$$this"
      }
    })
    if (isThis) NotNullValue
    else super.newParameterValue(isInstanceMethod, local, tp)
  }

  def newOperation(insn: AbstractInsnNode): NullnessValue = (insn.getOpcode: @switch) match {
    case Opcodes.ACONST_NULL => NullValue

    case Opcodes.LDC => insn.asInstanceOf[LdcInsnNode].cst match {
      case _: String | _: Type => NotNullValue
      case _ => NullnessValue.unknown(insn)
    }

    case Opcodes.GETSTATIC =>
      val fi = insn.asInstanceOf[FieldInsnNode]
      if (modulesNonNull && isModuleLoad(fi, _ == fi.owner)) NotNullValue
      else NullnessValue.unknown(insn)

    // for Opcodes.NEW, we use Unknown. The value will become NotNull after the constructor call.
    case _ => NullnessValue.unknown(insn)
  }

  def copyOperation(insn: AbstractInsnNode, value: NullnessValue): NullnessValue = value

  def unaryOperation(insn: AbstractInsnNode, value: NullnessValue): NullnessValue = (insn.getOpcode: @switch) match {
    case Opcodes.CHECKCAST => value

    case Opcodes.NEWARRAY |
         Opcodes.ANEWARRAY => NotNullValue

    case _ => NullnessValue.unknown(insn)
  }

  def binaryOperation(insn: AbstractInsnNode, value1: NullnessValue, value2: NullnessValue): NullnessValue = {
    NullnessValue.unknown(insn)
  }

  def ternaryOperation(insn: AbstractInsnNode, value1: NullnessValue, value2: NullnessValue, value3: NullnessValue): NullnessValue = UnknownValue1

  def naryOperation(insn: AbstractInsnNode, values: util.List[_ <: NullnessValue]): NullnessValue = insn match {
    case mi: MethodInsnNode if knownNonNullInvocation(mi) =>
      NotNullValue

    case _ =>
      if (insn.getOpcode == Opcodes.MULTIANEWARRAY) NotNullValue
      else NullnessValue.unknown(insn)
  }

  def returnOperation(insn: AbstractInsnNode, value: NullnessValue, expected: NullnessValue): Unit = ()

  def merge(a: NullnessValue, b: NullnessValue): NullnessValue = a merge b
}

class NullnessFrame(nLocals: Int, nStack: Int) extends AliasingFrame[NullnessValue](nLocals, nStack) {
  private[this] var ifNullAliases: AliasSet = null

  // Auxiliary constructor required for implementing `NullnessAnalyzer.newFrame`
  def this(src: Frame[_ <: NullnessValue]) = {
    this(src.getLocals, src.getMaxStackSize)
    init(src)
  }

  private def setNullness(s: AliasSet, v: NullnessValue) = {
    val it = s.iterator
    while (it.hasNext)
      this.setValue(it.next(), v)
  }

  override def initJumpTarget(opcode: Int, target: LabelNode): Unit = {
    // when `target` is defined, we're in the case where the branch condition is true
    val conditionTrue = target != null
    if (opcode == Opcodes.IFNULL)
      setNullness(ifNullAliases, if (conditionTrue) NullValue else NotNullValue)
    else if (opcode == Opcodes.IFNONNULL)
      setNullness(ifNullAliases, if (conditionTrue) NotNullValue else NullValue)
  }

  override def execute(insn: AbstractInsnNode, interpreter: Interpreter[NullnessValue]): Unit = {
    import Opcodes._

    ifNullAliases = insn.getOpcode match {
      case IFNULL | IFNONNULL => aliasesOf(this.stackTop)
      case _ => null
    }

    // get the alias set the object that is known to be not-null after this operation.
    // alias sets are mutable / mutated, so after super.execute, this set contains the remaining
    // aliases of the value that becomes not-null.
    val nullCheckedAliases: AliasSet = (insn.getOpcode: @switch) match {
      case IALOAD |
           LALOAD |
           FALOAD |
           DALOAD |
           AALOAD |
           BALOAD |
           CALOAD |
           SALOAD =>
        aliasesOf(this.stackTop - 1)

      case IASTORE |
           FASTORE |
           AASTORE |
           BASTORE |
           CASTORE |
           SASTORE |
           LASTORE |
           DASTORE =>
        aliasesOf(this.stackTop - 2)

      case GETFIELD =>
        aliasesOf(this.stackTop)

      case PUTFIELD =>
        aliasesOf(this.stackTop - 1)

      case INVOKEVIRTUAL |
           INVOKESPECIAL |
           INVOKEINTERFACE =>
        val desc = insn.asInstanceOf[MethodInsnNode].desc
        val numArgs = Type.getArgumentTypes(desc).length
        aliasesOf(this.stackTop - numArgs)

      case INVOKESTATIC =>
        var nullChecked = BackendUtils.argumentsNullCheckedByCallee(insn.asInstanceOf[MethodInsnNode])
        var i = 0
        var res: AliasSet = null
        while (nullChecked > 0) {
          if ((nullChecked & 1L) != 0) {
            val a = aliasesOf(this.stackTop - i)
            if (res == null) res = a
            else a.iterator.foreach(res.+=)
          }
          i += 1
          nullChecked >>= 1
        }
        res

      case ARRAYLENGTH |
           MONITORENTER |
           MONITOREXIT =>
        aliasesOf(this.stackTop)

      case _ =>
        null
    }

    super.execute(insn, interpreter)

    if (nullCheckedAliases != null)
      setNullness(nullCheckedAliases, NotNullValue)
  }
}

class NullnessAnalyzerImpl(methodNode: MethodNode, knownNonNullInvocation: MethodInsnNode => Boolean, modulesNonNull: Boolean)
  extends Analyzer[NullnessValue](new NullnessInterpreter(knownNonNullInvocation, modulesNonNull, methodNode)) {
  // override the `newFrame` methods to make sure the analyzer uses NullnessFrames.
  override def newFrame(nLocals: Int, nStack: Int): NullnessFrame = new NullnessFrame(nLocals, nStack)
  override def newFrame(src: Frame[_ <: NullnessValue]): NullnessFrame = new NullnessFrame(src)
}

class NullnessAnalyzer(methodNode: MethodNode, classInternalName: InternalName, knownNonNullInvocation: MethodInsnNode => Boolean, modulesNonNull: Boolean)
  extends AsmAnalyzer(methodNode, classInternalName, new NullnessAnalyzerImpl(methodNode, knownNonNullInvocation, modulesNonNull))
    with AliasingAsmAnalyzerMarker
