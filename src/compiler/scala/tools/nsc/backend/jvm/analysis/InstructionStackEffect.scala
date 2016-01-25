package scala.tools.nsc
package backend.jvm
package analysis

import scala.annotation.switch
import scala.tools.asm.Opcodes._
import scala.tools.asm.Type
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis.{Frame, Value}
import opt.BytecodeUtils._

object InstructionStackEffect {
  val consShift = 3
  val prodMask = (1 << consShift) - 1

  def cons(i: Int) = i >>> consShift
  def prod(i: Int) = i & prodMask

  private def t(x: Int, y: Int): Int = (x << consShift) | y

  /**
   * Returns the number of stack values consumed and produced by `insn`, encoded in a single `Int`
   * (the `cons` / `prod` extract individual values). The returned values are correct for use in
   * asm's Analyzer framework. For example, a LLOAD instruction produces one stack value. See also
   * doc in `analysis` package object.
   *
   * This method requires the `frame` to be in the state **before** executing / interpreting the
   * `insn`.
   */
  def forAsmAnalysis[V <: Value](insn: AbstractInsnNode, frame: Frame[V]): Int = computeConsProd(insn, forClassfile = false, conservative = false, frame = frame)

  /**
   * Returns the maximal possible growth of the stack when executing `insn`. The returned value
   * is usually the same as expected by asm's Analyzer framework, but it may be larger. For
   * example, consider a POP2 instruction:
   *   - if two size-1 values are popped, then the asm Analyzer consumes two values
   *   - if a size-2 value is popped, the asm Analyzer consumes only one stack slot (see doc in the
   *     `analysis` package object)
   *
   * If a precise result is needed, invoke the `forAsmAnalysis` and provide a `frame` value that
   * allows looking up the sizes of values on the stack.
   */
  def maxStackGrowth(insn: AbstractInsnNode): Int = {
    val prodCons = computeConsProd(insn, forClassfile = false, conservative = true)
    prod(prodCons) - cons(prodCons)
  }

  /**
   * Returns the number of stack values consumed and produced by `insn`, encoded in a single `Int`
   * (the `cons` / `prod` extract individual values).  The returned values are correct for writing
   * into a classfile (see doc on the `analysis` package object).
   */
  def forClassfile(insn: AbstractInsnNode): Int = computeConsProd(insn, forClassfile = true, conservative = false)

  private def invokeConsProd(methodDesc: String, insn: AbstractInsnNode, forClassfile: Boolean): Int = {
    val consumesReceiver = insn.getOpcode != INVOKESTATIC && insn.getOpcode != INVOKEDYNAMIC
    if (forClassfile) {
      val sizes = Type.getArgumentsAndReturnSizes(methodDesc)
      val cons = (sizes >> 2) - (if (consumesReceiver) 0 else 1)
      val prod = sizes & 0x03
      t(cons, prod)
    } else {
      val cons = Type.getArgumentTypes(methodDesc).length + (if (consumesReceiver) 1 else 0)
      val prod = if (Type.getReturnType(methodDesc) == Type.VOID_TYPE) 0 else 1
      t(cons, prod)
    }
  }

  private def fieldInsnIsLongOrDouble(insn: AbstractInsnNode) = {
    val d = insn.asInstanceOf[FieldInsnNode].desc
    d == "J" || d == "D"
  }

  private def computeConsProd[V <: Value](insn: AbstractInsnNode, forClassfile: Boolean, conservative: Boolean, frame: Frame[V] = null): Int = {
    // not used if `forClassfile || conservative`: in these cases, `frame` is allowed to be `null`
    def peekStack(n: Int): V = frame.peekStack(n)

    (insn.getOpcode: @switch) match {
      // The order of opcodes is the same as in Frame.execute.
      case NOP => t(0, 0)

      case ACONST_NULL |
           ICONST_M1 |
           ICONST_0 |
           ICONST_1 |
           ICONST_2 |
           ICONST_3 |
           ICONST_4 |
           ICONST_5 |
           FCONST_0 |
           FCONST_1 |
           FCONST_2 |
           BIPUSH |
           SIPUSH |
           ILOAD |
           FLOAD |
           ALOAD => t(0, 1)

      case LDC =>
        if (forClassfile) insn.asInstanceOf[LdcInsnNode].cst match {
          case _: java.lang.Long | _: java.lang.Double => t(0, 2)
          case _ => t(0, 1)
        } else
          t(0, 1)

      case LCONST_0 |
           LCONST_1 |
           DCONST_0 |
           DCONST_1 |
           LLOAD |
           DLOAD => if (forClassfile) t(0, 2) else t(0, 1)

      case IALOAD |
           FALOAD |
           AALOAD |
           BALOAD |
           CALOAD |
           SALOAD => t(2, 1)

      case LALOAD |
           DALOAD => if (forClassfile) t(2, 2) else t(2, 1)

      case ISTORE |
           FSTORE |
           ASTORE => t(1, 0)

      case LSTORE |
           DSTORE => if (forClassfile) t(2, 0) else t(1, 0)

      case IASTORE |
           FASTORE |
           AASTORE |
           BASTORE |
           CASTORE |
           SASTORE => t(3, 0)

      case LASTORE |
           DASTORE => if (forClassfile) t(4, 0) else t(3, 0)

      case POP => t(1, 0)

      case POP2 =>
        if (forClassfile) t(2, 0)
        else if (conservative) t(1, 0)
        else {
          val isSize2 = peekStack(0).getSize == 2
          if (isSize2) t(1, 0) else t(2, 0)
        }

      case DUP => t(1, 2)

      case DUP_X1 => t(2, 3)

      case DUP_X2 =>
        if (forClassfile || conservative) t(3, 4)
        else {
          val isSize2 = peekStack(1).getSize == 2
          if (isSize2) t(2, 3) else t(3, 4)
        }

      case DUP2 =>
        if (forClassfile || conservative) t(2, 4)
        else {
          val isSize2 = peekStack(0).getSize == 2
          if (isSize2) t(1, 2) else t(2, 4)
        }

      case DUP2_X1 =>
        if (forClassfile || conservative) t(3, 5)
        else {
          val isSize2 = peekStack(0).getSize == 2
          if (isSize2) t(2, 3) else t(3, 5)
        }

      case DUP2_X2 =>
        if (forClassfile || conservative) t(4, 6)
        else {
          val v1isSize2 = peekStack(0).getSize == 2
          if (v1isSize2) {
            val v2isSize2 = peekStack(1).getSize == 2
            if (v2isSize2) t(2, 3) else t(3, 4)
          } else {
            val v3isSize2 = peekStack(2).getSize == 2
            if (v3isSize2) t(3, 5) else t(4, 6)
          }
        }

      case SWAP => t(2, 2)

      case IADD |
           FADD |
           ISUB |
           FSUB |
           IMUL |
           FMUL |
           IDIV |
           FDIV |
           IREM |
           FREM => t(2, 1)

      case LADD |
           DADD |
           LSUB |
           DSUB |
           LMUL |
           DMUL |
           LDIV |
           DDIV |
           LREM |
           DREM => if (forClassfile) t(4, 2) else t(2, 1)

      case INEG |
           FNEG => t(1, 1)

      case LNEG |
           DNEG => if (forClassfile) t(2, 2) else t(1, 1)

      case ISHL |
           ISHR |
           IUSHR |
           IAND |
           IOR |
           IXOR => t(2, 1)

      case LSHL |
           LSHR |
           LUSHR => if (forClassfile) t(3, 2) else t(2, 1)

      case LAND |
           LOR |
           LXOR => if (forClassfile) t(4, 2) else t(2, 1)

      case IINC => t(0, 0)

      case I2F |
           F2I |
           I2B |
           I2C |
           I2S => t(1, 1)

      case I2L |
           I2D |
           F2L |
           F2D => if (forClassfile) t(1, 2) else t(1, 1)

      case L2I |
           L2F |
           D2I |
           D2F => if (forClassfile) t(2, 1) else t(1, 1)

      case L2D |
           D2L => if (forClassfile) t(2, 2) else t(1, 1)

      case FCMPL |
           FCMPG => t(2, 1)

      case LCMP |
           DCMPL |
           DCMPG => if (forClassfile) t(4, 1) else t(2, 1)

      case IFEQ |
           IFNE |
           IFLT |
           IFGE |
           IFGT |
           IFLE => t(1, 0)

      case IF_ICMPEQ |
           IF_ICMPNE |
           IF_ICMPLT |
           IF_ICMPGE |
           IF_ICMPGT |
           IF_ICMPLE |
           IF_ACMPEQ |
           IF_ACMPNE => t(2, 0)

      case GOTO => t(0, 0)

      case JSR => t(0, 1)

      case RET => t(0, 0)

      case TABLESWITCH |
           LOOKUPSWITCH => t(1, 0)

      case IRETURN |
           FRETURN |
           ARETURN => t(1, 0) // Frame.execute consumes one stack value

      case LRETURN |
           DRETURN => if (forClassfile) t(2, 0) else t(1, 0)

      case RETURN => t(0, 0) // Frame.execute does not change the stack

      case GETSTATIC =>
        val prod = if (forClassfile && fieldInsnIsLongOrDouble(insn)) 2 else 1
        t(0, prod)

      case PUTSTATIC =>
        val cons = if (forClassfile && fieldInsnIsLongOrDouble(insn)) 2 else 1
        t(cons, 0)

      case GETFIELD =>
        val prod = if (forClassfile && fieldInsnIsLongOrDouble(insn)) 2 else 1
        t(1, prod)

      case PUTFIELD =>
        val cons = if (forClassfile && fieldInsnIsLongOrDouble(insn)) 3 else 2
        t(cons, 0)

      case INVOKEVIRTUAL |
           INVOKESPECIAL |
           INVOKESTATIC |
           INVOKEINTERFACE => invokeConsProd(insn.asInstanceOf[MethodInsnNode].desc, insn, forClassfile)

      case INVOKEDYNAMIC => invokeConsProd(insn.asInstanceOf[InvokeDynamicInsnNode].desc, insn, forClassfile)

      case NEW => t(0, 1)

      case NEWARRAY |
           ANEWARRAY |
           ARRAYLENGTH => t(1, 1)

      case ATHROW => t(1, 0) // Frame.execute consumes one stack value

      case CHECKCAST |
           INSTANCEOF => t(1, 1) // Frame.execute does push(pop()) for both of them

      case MONITORENTER |
           MONITOREXIT => t(1, 0)

      case MULTIANEWARRAY => t(insn.asInstanceOf[MultiANewArrayInsnNode].dims, 1)

      case IFNULL |
           IFNONNULL => t(1, 0)
    }
  }
}
