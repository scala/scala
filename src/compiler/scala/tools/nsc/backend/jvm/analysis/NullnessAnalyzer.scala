package scala.tools.nsc
package backend.jvm
package analysis

import java.util

import scala.annotation.switch
import scala.tools.asm.{Type, Opcodes}
import scala.tools.asm.tree.{MethodInsnNode, LdcInsnNode, AbstractInsnNode}
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
 * Type to represent nullness of values.
 */
sealed trait Nullness {
  final def merge(other: Nullness) = if (this == other) this else Unknown
}
case object NotNull extends Nullness
case object Unknown extends Nullness
case object Null    extends Nullness

/**
 * Represents the nullness state for a local variable or stack value.
 *
 * Note that nullness of primitive values is not tracked, it will be always [[Unknown]].
 */
sealed trait NullnessValue extends Value {
  /**
   * The nullness of this value.
   */
  def nullness: Nullness

  /**
   * True if this value is a long or double. The Analyzer framework needs to know
   * the size of each value when interpreting instructions, see `Frame.execute`.
   */
  def isSize2: Boolean
  /**
   * The size of the slot described by this value. Cannot be 0 because no values are allocated
   * for void-typed slots, see NullnessInterpreter.newValue.
   **/
  def getSize: Int = if (isSize2) 2 else 1

  def merge(other: NullnessValue) = NullnessValue(nullness merge other.nullness, isSize2)
}

object NullValue     extends NullnessValue { def nullness = Null;    def isSize2 = false; override def toString = "Null"     }
object UnknownValue1 extends NullnessValue { def nullness = Unknown; def isSize2 = false; override def toString = "Unknown1" }
object UnknownValue2 extends NullnessValue { def nullness = Unknown; def isSize2 = true;  override def toString = "Unknown2" }
object NotNullValue  extends NullnessValue { def nullness = NotNull; def isSize2 = false; override def toString = "NotNull"  }

object NullnessValue {
  def apply(nullness: Nullness, isSize2: Boolean): NullnessValue = {
    if      (nullness == Null)    NullValue
    else if (nullness == NotNull) NotNullValue
    else if (isSize2)             UnknownValue2
    else                          UnknownValue1
  }

  def apply(nullness: Nullness, insn: AbstractInsnNode): NullnessValue = {
    apply(nullness, isSize2 = BytecodeUtils.instructionResultSize(insn) == 2)
  }
}

final class NullnessInterpreter extends Interpreter[NullnessValue](Opcodes.ASM5) {
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
    else NullnessValue(Unknown, isSize2 = tp != null /*(2)*/ && tp.getSize == 2 )
  }

  override def newParameterValue(isInstanceMethod: Boolean, local: Int, tp: Type): NullnessValue = {
    // For instance methods, the `this` parameter is known to be not null.
    if (isInstanceMethod && local == 0) NullnessValue(NotNull, isSize2 = false)
    else super.newParameterValue(isInstanceMethod, local, tp)
  }

  def newOperation(insn: AbstractInsnNode): NullnessValue = {
    val nullness = (insn.getOpcode: @switch) match {
      case Opcodes.ACONST_NULL => Null

      case Opcodes.LDC => insn.asInstanceOf[LdcInsnNode].cst match {
        case _: String | _: Type => NotNull
        case _ => Unknown
      }

      case _ => Unknown
    }

    // for Opcodes.NEW, we use Unknown. The value will become NotNull after the constructor call.
    NullnessValue(nullness, insn)
  }

  def copyOperation(insn: AbstractInsnNode, value: NullnessValue): NullnessValue = value

  def unaryOperation(insn: AbstractInsnNode, value: NullnessValue): NullnessValue = (insn.getOpcode: @switch) match {
    case Opcodes.CHECKCAST => value

    case Opcodes.NEWARRAY |
         Opcodes.ANEWARRAY => NullnessValue(NotNull, isSize2 = false)

    case _ => NullnessValue(Unknown, insn)
  }

  def binaryOperation(insn: AbstractInsnNode, value1: NullnessValue, value2: NullnessValue): NullnessValue = {
    NullnessValue(Unknown, insn)
  }

  def ternaryOperation(insn: AbstractInsnNode, value1: NullnessValue, value2: NullnessValue, value3: NullnessValue): NullnessValue = {
    NullnessValue(Unknown, isSize2 = false)
  }

  def naryOperation(insn: AbstractInsnNode, values: util.List[_ <: NullnessValue]): NullnessValue = (insn.getOpcode: @switch) match {
    case Opcodes.MULTIANEWARRAY =>
      NullnessValue(NotNull, isSize2 = false)

    case _ =>
      // TODO: use a list of methods that are known to return non-null values
      NullnessValue(Unknown, insn)
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
class NullnessAnalyzer extends Analyzer[NullnessValue](new NullnessInterpreter) {
  override def newFrame(nLocals: Int, nStack: Int): NullnessFrame = new NullnessFrame(nLocals, nStack)
  override def newFrame(src: Frame[_ <: NullnessValue]): NullnessFrame = new NullnessFrame(src)
}
