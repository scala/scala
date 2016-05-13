package scala.tools.nsc
package backend.jvm
package analysis

import java.util

import scala.annotation.switch
import scala.tools.asm.{Opcodes, Type}
import scala.tools.asm.tree.{AbstractInsnNode, LdcInsnNode, MethodInsnNode, MethodNode}
import scala.tools.asm.tree.analysis._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils
import BytecodeUtils._

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
 *
 * TODO:
 * Finally, we'd also like to exploit the knowledge gained from `if (x == null)` tests: x is known
 * to be null in one branch, not null in the other. This will make use of alias tracking as well.
 * We still have to figure out how to do this exactly in the analyzer framework.
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
}

object NullValue     extends NullnessValue(isSize2 = false) { override def toString = "Null"     }
object UnknownValue1 extends NullnessValue(isSize2 = false) { override def toString = "Unknown1" }
object UnknownValue2 extends NullnessValue(isSize2 = true ) { override def toString = "Unknown2" }
object NotNullValue  extends NullnessValue(isSize2 = false) { override def toString = "NotNull"  }

object NullnessValue {
  def unknown(isSize2: Boolean) = if (isSize2) UnknownValue2 else UnknownValue1
  def unknown(insn: AbstractInsnNode) = if (BytecodeUtils.instructionResultSize(insn) == 2) UnknownValue2 else UnknownValue1
}

final class NullnessInterpreter(bTypes: BTypes, method: MethodNode) extends Interpreter[NullnessValue](Opcodes.ASM5) {
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
        (p.access & Opcodes.ACC_SYNTHETIC) != 0 && p.name == "$this"
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
    case mi: MethodInsnNode if bTypes.backendUtils.isNonNullMethodInvocation(mi) =>
      NotNullValue

    case _ =>
      if (insn.getOpcode == Opcodes.MULTIANEWARRAY) NotNullValue
      else NullnessValue.unknown(insn)
  }

  def returnOperation(insn: AbstractInsnNode, value: NullnessValue, expected: NullnessValue): Unit = ()

  def merge(a: NullnessValue, b: NullnessValue): NullnessValue = a merge b
}

class NullnessFrame(nLocals: Int, nStack: Int) extends AliasingFrame[NullnessValue](nLocals, nStack) {
  // Auxiliary constructor required for implementing `NullnessAnalyzer.newFrame`
  def this(src: Frame[_ <: NullnessValue]) {
    this(src.getLocals, src.getMaxStackSize)
    init(src)
  }

  override def execute(insn: AbstractInsnNode, interpreter: Interpreter[NullnessValue]): Unit = {
    import Opcodes._

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

      case ARRAYLENGTH |
           MONITORENTER |
           MONITOREXIT =>
        aliasesOf(this.stackTop)

      case _ =>
        null
    }

    super.execute(insn, interpreter)

    if (nullCheckedAliases != null) {
      val it = nullCheckedAliases.iterator
      while (it.hasNext)
        this.setValue(it.next(), NotNullValue)
    }
  }
}

/**
 * This class is required to override the `newFrame` methods, which makes makes sure the analyzer
 * uses NullnessFrames.
 */
class NullnessAnalyzer(bTypes: BTypes, method: MethodNode) extends Analyzer[NullnessValue](new NullnessInterpreter(bTypes, method)) {
  override def newFrame(nLocals: Int, nStack: Int): NullnessFrame = new NullnessFrame(nLocals, nStack)
  override def newFrame(src: Frame[_ <: NullnessValue]): NullnessFrame = new NullnessFrame(src)
}
