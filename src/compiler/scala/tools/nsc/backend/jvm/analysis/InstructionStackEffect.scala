package scala.tools.nsc
package backend.jvm
package analysis

import scala.annotation.switch
import scala.tools.asm.Opcodes._
import scala.tools.asm.Type
import scala.tools.asm.tree.{MultiANewArrayInsnNode, InvokeDynamicInsnNode, MethodInsnNode, AbstractInsnNode}
import scala.tools.asm.tree.analysis.{Frame, Value}
import opt.BytecodeUtils._
import collection.immutable

object InstructionStackEffect {
  private var cache: immutable.IntMap[(Int, Int)] = immutable.IntMap.empty
  private def t(x: Int, y: Int): (Int, Int) = {
    // x can go up to 255 (number of parameters of a method, dimensions in multianewarray) we cache
    // x up to 10, which covers most cases and limits the cache. y doesn't go above 6 (see cases).
    if (x > 10 || y > 6) (x, y)
    else {
      val key = (x << 8) + y // this would work for any x < 256
      if (cache contains key) {
        cache(key)
      } else {
        val r = (x, y)
        cache += key -> r
        r
      }
    }
  }

  /**
   * Returns a pair with the number of stack values consumed and produced by `insn`.
   * This method requires the `frame` to be in the state **before** executing / interpreting
   * the `insn`.
   */
  def apply[V <: Value](insn: AbstractInsnNode, frame: Frame[V]): (Int, Int) = {
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
           ALOAD => t(0, 1)

      case IALOAD |
           LALOAD |
           FALOAD |
           DALOAD |
           AALOAD |
           BALOAD |
           CALOAD |
           SALOAD => t(2, 1)

      case ISTORE |
           LSTORE |
           FSTORE |
           DSTORE |
           ASTORE => t(1, 0)

      case IASTORE |
           LASTORE |
           FASTORE |
           DASTORE |
           AASTORE |
           BASTORE |
           CASTORE |
           SASTORE => t(3, 0)

      case POP => t(1, 0)

      case POP2 =>
        val isSize2 = peekStack(0).getSize == 2
        if (isSize2) t(1, 0) else t(2, 0)

      case DUP => t(1, 2)

      case DUP_X1 => t(2, 3)

      case DUP_X2 =>
        val isSize2 = peekStack(1).getSize == 2
        if (isSize2) t(2, 3) else t(3, 4)

      case DUP2 =>
        val isSize2 = peekStack(0).getSize == 2
        if (isSize2) t(1, 2) else t(2, 4)

      case DUP2_X1 =>
        val isSize2 = peekStack(0).getSize == 2
        if (isSize2) t(2, 3) else t(3, 4)

      case DUP2_X2 =>
        val v1isSize2 = peekStack(0).getSize == 2
        if (v1isSize2) {
          val v2isSize2 = peekStack(1).getSize == 2
          if (v2isSize2) t(2, 3) else t(3, 4)
        } else {
          val v3isSize2 = peekStack(2).getSize == 2
          if (v3isSize2) t(3, 5) else t(4, 6)
        }

      case SWAP => t(2, 2)

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
           DREM => t(2, 1)

      case INEG |
           LNEG |
           FNEG |
           DNEG => t(1, 1)

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
           LXOR => t(2, 1)

      case IINC => t(0, 0)

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
           I2S => t(1, 1)

      case LCMP |
           FCMPL |
           FCMPG |
           DCMPL |
           DCMPG => t(2, 1)

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
           LRETURN |
           FRETURN |
           DRETURN |
           ARETURN => t(1, 0) // Frame.execute consumes one stack value

      case RETURN => t(0, 0) // Frame.execute does not change the stack

      case GETSTATIC => t(0, 1)

      case PUTSTATIC => t(1, 0)

      case GETFIELD => t(1, 1)

      case PUTFIELD => t(2, 0)

      case INVOKEVIRTUAL |
           INVOKESPECIAL |
           INVOKESTATIC |
           INVOKEINTERFACE =>
        val desc = insn.asInstanceOf[MethodInsnNode].desc
        val cons = Type.getArgumentTypes(desc).length + (if (insn.getOpcode == INVOKESTATIC) 0 else 1)
        val prod = if (Type.getReturnType(desc) == Type.VOID_TYPE) 0 else 1
        t(cons, prod)

      case INVOKEDYNAMIC =>
        val desc = insn.asInstanceOf[InvokeDynamicInsnNode].desc
        val cons = Type.getArgumentTypes(desc).length
        val prod = if (Type.getReturnType(desc) == Type.VOID_TYPE) 0 else 1
        t(cons, prod)

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
