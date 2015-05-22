package scala.tools.nsc.backend.jvm.opt

import scala.annotation.switch
import scala.tools.asm.{Handle, Type, Opcodes}
import scala.tools.asm.tree._

object InstructionResultSize {
  import Opcodes._
  def apply(instruction: AbstractInsnNode): Int = (instruction.getOpcode: @switch) match {
    // The order of opcodes is (almost) the same as in Opcodes.java
    case ACONST_NULL => 1

    case ICONST_M1 |
         ICONST_0 |
         ICONST_1 |
         ICONST_2 |
         ICONST_3 |
         ICONST_4 |
         ICONST_5 => 1

    case LCONST_0 |
         LCONST_1 => 2

    case FCONST_0 |
         FCONST_1 |
         FCONST_2 => 1

    case DCONST_0 |
         DCONST_1 => 2

    case BIPUSH |
         SIPUSH => 1

    case LDC =>
      instruction.asInstanceOf[LdcInsnNode].cst match {
        case _: java.lang.Integer |
             _: java.lang.Float |
             _: String |
             _: Type |
             _: Handle => 1

        case _: java.lang.Long |
             _: java.lang.Double => 2
      }

    case ILOAD |
         FLOAD |
         ALOAD => 1

    case LLOAD |
         DLOAD => 2

    case IALOAD |
         FALOAD |
         AALOAD |
         BALOAD |
         CALOAD |
         SALOAD => 1

    case LALOAD |
         DALOAD => 2

    case ISTORE |
         LSTORE |
         FSTORE |
         DSTORE |
         ASTORE => 0

    case IASTORE |
         LASTORE |
         FASTORE |
         DASTORE |
         AASTORE |
         BASTORE |
         CASTORE |
         SASTORE => 0

    case POP |
         POP2 => 0

    case DUP |
         DUP_X1 |
         DUP_X2 |
         DUP2 |
         DUP2_X1 |
         DUP2_X2 |
         SWAP => throw new IllegalArgumentException("Can't compute the size of DUP/SWAP without knowing what's on stack top")

    case IADD |
         FADD => 1

    case LADD |
         DADD => 2

    case ISUB |
         FSUB => 1

    case LSUB |
         DSUB => 2

    case IMUL |
         FMUL => 1

    case LMUL |
         DMUL => 2

    case IDIV |
         FDIV => 1

    case LDIV |
         DDIV => 2

    case IREM |
         FREM => 1

    case LREM |
         DREM => 2

    case INEG |
         FNEG => 1

    case LNEG |
         DNEG => 2

    case ISHL |
         ISHR => 1

    case LSHL |
         LSHR => 2

    case IUSHR => 1

    case LUSHR => 2

    case IAND |
         IOR |
         IXOR => 1

    case LAND |
         LOR |
         LXOR => 2

    case IINC => 1

    case I2F |
         L2I |
         L2F |
         F2I |
         D2I |
         D2F |
         I2B |
         I2C |
         I2S => 1

    case I2L |
         I2D |
         L2D |
         F2L |
         F2D |
         D2L => 2

    case LCMP |
         FCMPL |
         FCMPG |
         DCMPL |
         DCMPG => 1

    case IFEQ |
         IFNE |
         IFLT |
         IFGE |
         IFGT |
         IFLE => 0

    case IF_ICMPEQ |
         IF_ICMPNE |
         IF_ICMPLT |
         IF_ICMPGE |
         IF_ICMPGT |
         IF_ICMPLE |
         IF_ACMPEQ |
         IF_ACMPNE => 0

    case GOTO => 0

    case JSR => throw new IllegalArgumentException("Subroutines are not supported.")

    case RET => 0

    case TABLESWITCH |
         LOOKUPSWITCH => 0

    case IRETURN |
         FRETURN |
         ARETURN => 1

    case LRETURN |
         DRETURN => 2

    case RETURN => 0

    case GETSTATIC => Type.getType(instruction.asInstanceOf[FieldInsnNode].desc).getSize

    case PUTSTATIC => 0

    case GETFIELD => Type.getType(instruction.asInstanceOf[FieldInsnNode].desc).getSize

    case PUTFIELD => 0

    case INVOKEVIRTUAL |
         INVOKESPECIAL |
         INVOKESTATIC |
         INVOKEINTERFACE =>
      val desc = instruction.asInstanceOf[MethodInsnNode].desc
      Type.getReturnType(desc).getSize

    case INVOKEDYNAMIC =>
      val desc = instruction.asInstanceOf[InvokeDynamicInsnNode].desc
      Type.getReturnType(desc).getSize

    case NEW => 1

    case NEWARRAY |
         ANEWARRAY |
         ARRAYLENGTH => 1

    case ATHROW => 0

    case CHECKCAST |
         INSTANCEOF => 1

    case MONITORENTER |
         MONITOREXIT => 0

    case MULTIANEWARRAY => 1

    case IFNULL |
         IFNONNULL => 0
  }
}
