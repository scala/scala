package scala.tools.partest

import scala.collection.JavaConverters._
import scala.tools.asm
import asm.tree.{ClassNode, MethodNode, InsnList}

/** Makes using ASM from ByteCodeTests more convenient.
 *
 * Wraps ASM instructions in case classes so that equals and toString work
 * for the purpose of bytecode diffing and pretty printing.
 */
trait ASMConverters {
  // wrap ASM's instructions so we get case class-style `equals` and `toString`
  object instructions {
    def fromMethod(meth: MethodNode): List[Instruction] = {
      val insns = meth.instructions
      val asmToScala = new AsmToScala{ def labelIndex(l: asm.tree.AbstractInsnNode) = insns.indexOf(l) }

      asmToScala.mapOver(insns.iterator.asScala.toList).asInstanceOf[List[Instruction]]
    }

    sealed abstract class Instruction { def opcode: String }
    case class Field         (opcode: String, desc: String, name: String, owner: String)             extends Instruction
    case class Incr          (opcode: String, incr: Int, `var`: Int)                                 extends Instruction
    case class Op            (opcode: String)                                                        extends Instruction
    case class IntOp         (opcode: String, operand: Int)                                          extends Instruction
    case class Jump          (opcode: String, label: Label)                                          extends Instruction
    case class Ldc           (opcode: String, cst: Any)                                              extends Instruction
    case class LookupSwitch  (opcode: String, dflt: Label, keys: List[Integer], labels: List[Label]) extends Instruction
    case class TableSwitch   (opcode: String, dflt: Label, max: Int, min: Int, labels: List[Label])  extends Instruction
    case class Method        (opcode: String, desc: String, name: String, owner: String)             extends Instruction
    case class NewArray      (opcode: String, desc: String, dims: Int)                               extends Instruction
    case class TypeOp        (opcode: String, desc: String)                                          extends Instruction
    case class VarOp         (opcode: String, `var`: Int)                                            extends Instruction
    case class Label         (offset: Int)                                                           extends Instruction { def opcode: String = "" }
    case class FrameEntry    (local: List[Any], stack: List[Any])                                    extends Instruction { def opcode: String = "" }
    case class LineNumber    (line: Int, start: Label)                                               extends Instruction { def opcode: String = "" }
  }

  abstract class AsmToScala {
    import instructions._

    def labelIndex(l: asm.tree.AbstractInsnNode): Int

    def mapOver(is: List[Any]): List[Any] = is map {
      case i: asm.tree.AbstractInsnNode => apply(i)
      case x => x
    }

   def op(i: asm.tree.AbstractInsnNode) = if (asm.util.Printer.OPCODES.isDefinedAt(i.getOpcode)) asm.util.Printer.OPCODES(i.getOpcode) else "?"
   def lst[T](xs: java.util.List[T]): List[T] = if (xs == null) Nil else xs.asScala.toList
   def apply(l: asm.tree.LabelNode): Label = this(l: asm.tree.AbstractInsnNode).asInstanceOf[Label]
   def apply(x: asm.tree.AbstractInsnNode): Instruction = x match {
      case i: asm.tree.FieldInsnNode          => Field        (op(i), i.desc: String, i.name: String, i.owner: String)
      case i: asm.tree.IincInsnNode           => Incr         (op(i), i.incr: Int, i.`var`: Int)
      case i: asm.tree.InsnNode               => Op           (op(i))
      case i: asm.tree.IntInsnNode            => IntOp        (op(i), i.operand: Int)
      case i: asm.tree.JumpInsnNode           => Jump         (op(i), this(i.label))
      case i: asm.tree.LdcInsnNode            => Ldc          (op(i), i.cst: Any)
      case i: asm.tree.LookupSwitchInsnNode   => LookupSwitch (op(i), this(i.dflt), lst(i.keys), mapOver(lst(i.labels)).asInstanceOf[List[Label]])
      case i: asm.tree.TableSwitchInsnNode    => TableSwitch  (op(i), this(i.dflt), i.max: Int, i.min: Int, mapOver(lst(i.labels)).asInstanceOf[List[Label]])
      case i: asm.tree.MethodInsnNode         => Method       (op(i), i.desc: String, i.name: String, i.owner: String)
      case i: asm.tree.MultiANewArrayInsnNode => NewArray     (op(i), i.desc: String, i.dims: Int)
      case i: asm.tree.TypeInsnNode           => TypeOp       (op(i), i.desc: String)
      case i: asm.tree.VarInsnNode            => VarOp        (op(i), i.`var`: Int)
      case i: asm.tree.LabelNode              => Label        (labelIndex(x))
      case i: asm.tree.FrameNode              => FrameEntry   (mapOver(lst(i.local)), mapOver(lst(i.stack)))
      case i: asm.tree.LineNumberNode         => LineNumber   (i.line: Int, this(i.start): Label)
    }
  }
}