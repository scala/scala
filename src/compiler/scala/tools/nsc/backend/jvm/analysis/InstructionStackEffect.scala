package scala.tools.nsc
package backend.jvm
package analysis

import scala.annotation.switch
import scala.tools.asm.Opcodes._
import scala.tools.asm.Type
import scala.tools.asm.tree.{MultiANewArrayInsnNode, InvokeDynamicInsnNode, MethodInsnNode, AbstractInsnNode}
import scala.tools.asm.tree.analysis.{Frame, Value}
import opt.BytecodeUtils._

object InstructionStackEffect {
  /**
   * Returns a pair with the number of stack values consumed and produced by `insn`.
   * This method requires the `frame` to be in the state **before** executing / interpreting
   * the `insn`.
   */
  def apply[V <: Value](insn: AbstractInsnNode, frame: Frame[V]): (Int, Int) = {
    def peekStack(n: Int): V = frame.peekStack(n)

    (insn.getOpcode: @switch) match {
      // The order of opcodes is the same as in Frame.execute.
      case NOP => (0, 0)

      case ACONST_NULL |
           ICONST_M1 |
           ICONST_0 |
           ICONST_1 |
           ICONST_2 |
           ICONST_3 |
           ICONST_4 |
           ICONST_5 |
           LCONST_0 |
           LCONST_1 |
           FCONST_0 |
           FCONST_1 |
           FCONST_2 |
           DCONST_0 |
           DCONST_1 |
           BIPUSH |
           SIPUSH |
           LDC |
           ILOAD |
           LLOAD |
           FLOAD |
           DLOAD |
           ALOAD => (0, 1)

      case IALOAD |
           LALOAD |
           FALOAD |
           DALOAD |
           AALOAD |
           BALOAD |
           CALOAD |
           SALOAD => (2, 1)

      case ISTORE |
           LSTORE |
           FSTORE |
           DSTORE |
           ASTORE => (1, 0)

      case IASTORE |
           LASTORE |
           FASTORE |
           DASTORE |
           AASTORE |
           BASTORE |
           CASTORE |
           SASTORE => (3, 0)

      case POP => (1, 0)

      case POP2 =>
        val isSize2 = peekStack(0).getSize == 2
        if (isSize2) (1, 0) else (2, 0)

      case DUP => (0, 1)

      case DUP_X1 => (2, 3)

      case DUP_X2 =>
        val isSize2 = peekStack(1).getSize == 2
        if (isSize2) (2, 3) else (3, 4)

      case DUP2 =>
        val isSize2 = peekStack(0).getSize == 2
        if (isSize2) (0, 1) else (0, 2)

      case DUP2_X1 =>
        val isSize2 = peekStack(0).getSize == 2
        if (isSize2) (2, 3) else (3, 4)

      case DUP2_X2 =>
        val v1isSize2 = peekStack(0).getSize == 2
        if (v1isSize2) {
          val v2isSize2 = peekStack(1).getSize == 2
          if (v2isSize2) (2, 3) else (3, 4)
        } else {
          val v3isSize2 = peekStack(2).getSize == 2
          if (v3isSize2) (3, 5) else (4, 6)
        }

      case SWAP => (2, 2)

      case IADD |
           LADD |
           FADD |
           DADD |
           ISUB |
           LSUB |
           FSUB |
           DSUB |
           IMUL |
           LMUL |
           FMUL |
           DMUL |
           IDIV |
           LDIV |
           FDIV |
           DDIV |
           IREM |
           LREM |
           FREM |
           DREM => (2, 1)

      case INEG |
           LNEG |
           FNEG |
           DNEG => (1, 1)

      case ISHL |
           LSHL |
           ISHR |
           LSHR |
           IUSHR |
           LUSHR |
           IAND |
           LAND |
           IOR |
           LOR |
           IXOR |
           LXOR => (2, 1)

      case IINC => (0, 0)

      case I2L |
           I2F |
           I2D |
           L2I |
           L2F |
           L2D |
           F2I |
           F2L |
           F2D |
           D2I |
           D2L |
           D2F |
           I2B |
           I2C |
           I2S => (1, 1)

      case LCMP |
           FCMPL |
           FCMPG |
           DCMPL |
           DCMPG => (2, 1)

      case IFEQ |
           IFNE |
           IFLT |
           IFGE |
           IFGT |
           IFLE => (1, 0)

      case IF_ICMPEQ |
           IF_ICMPNE |
           IF_ICMPLT |
           IF_ICMPGE |
           IF_ICMPGT |
           IF_ICMPLE |
           IF_ACMPEQ |
           IF_ACMPNE => (2, 0)

      case GOTO => (0, 0)

      case JSR => (0, 1)

      case RET => (0, 0)

      case TABLESWITCH |
           LOOKUPSWITCH => (1, 0)

      case IRETURN |
           LRETURN |
           FRETURN |
           DRETURN |
           ARETURN => (frame.getStackSize, 0)

      case RETURN => (frame.getStackSize, 0)

      case GETSTATIC => (0, 1)

      case PUTSTATIC => (1, 0)

      case GETFIELD => (1, 1)

      case PUTFIELD => (2, 0)

      case INVOKEVIRTUAL |
           INVOKESPECIAL |
           INVOKESTATIC |
           INVOKEINTERFACE =>
        val desc = insn.asInstanceOf[MethodInsnNode].desc
        val cons = Type.getArgumentTypes(desc).length + (if (insn.getOpcode == INVOKESTATIC) 0 else 1)
        val prod = if (Type.getReturnType(desc) == Type.VOID_TYPE) 0 else 1
        (cons, prod)

      case INVOKEDYNAMIC =>
        val desc = insn.asInstanceOf[InvokeDynamicInsnNode].desc
        val cons = Type.getArgumentTypes(desc).length
        val prod = if (Type.getReturnType(desc) == Type.VOID_TYPE) 0 else 1
        (cons, prod)

      case NEW => (0, 1)

      case NEWARRAY |
           ANEWARRAY |
           ARRAYLENGTH => (1, 1)

      case ATHROW => (frame.getStackSize, 0)

      case CHECKCAST => (0, 0)

      case INSTANCEOF => (1, 1)

      case MONITORENTER |
           MONITOREXIT => (1, 0)

      case MULTIANEWARRAY => (insn.asInstanceOf[MultiANewArrayInsnNode].dims, 1)

      case IFNULL |
           IFNONNULL => (1, 0)
    }
  }

}
