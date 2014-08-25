package scala.tools.partest

import scala.collection.JavaConverters._
import scala.tools.asm
import asm.{tree => t}

/** Makes using ASM from ByteCodeTests more convenient.
 *
 * Wraps ASM instructions in case classes so that equals and toString work
 * for the purpose of bytecode diffing and pretty printing.
 */
object ASMConverters {

  /**
   * Transform the instructions of an ASM Method into a list of [[Instruction]]s.
   */
  def instructionsFromMethod(meth: t.MethodNode): List[Instruction] = {
    val insns = meth.instructions
    val asmToScala = new AsmToScala {
      def labelIndex(l: t.LabelNode) = insns.indexOf(l)
    }
    asmToScala.convert(insns.iterator.asScala.toList)
  }

  implicit class CompareInstructionLists(val self: List[Instruction]) {
    def === (other: List[Instruction]) = equivalentBytecode(self, other)
  }

  sealed abstract class Instruction extends Product {
    def opcode: Int

    // toString such that the first field, "opcode: Int", is printed textually.
    final override def toString() = {
      import scala.tools.asm.util.Printer.OPCODES
      def opString(op: Int) = if (OPCODES.isDefinedAt(op)) OPCODES(op) else "?"
      val printOpcode = opcode != -1

      productPrefix + (
        if (printOpcode) Iterator(opString(opcode)) ++ productIterator.drop(1)
        else productIterator
      ).mkString("(", ", ", ")")
    }
  }

  case class Field       (opcode: Int, owner: String, name: String, desc: String)               extends Instruction
  case class Incr        (opcode: Int, `var`: Int, incr: Int)                                   extends Instruction
  case class Op          (opcode: Int)                                                          extends Instruction
  case class IntOp       (opcode: Int, operand: Int)                                            extends Instruction
  case class Jump        (opcode: Int, label: Label)                                            extends Instruction
  case class Ldc         (opcode: Int, cst: Any)                                                extends Instruction
  case class LookupSwitch(opcode: Int, dflt: Label, keys: List[Int], labels: List[Label])       extends Instruction
  case class TableSwitch (opcode: Int, min: Int, max: Int, dflt: Label, labels: List[Label])    extends Instruction
  case class Method      (opcode: Int, owner: String, name: String, desc: String, itf: Boolean) extends Instruction
  case class NewArray    (opcode: Int, desc: String, dims: Int)                                 extends Instruction
  case class TypeOp      (opcode: Int, desc: String)                                            extends Instruction
  case class VarOp       (opcode: Int, `var`: Int)                                              extends Instruction
  case class Label       (offset: Int)                                                          extends Instruction { def opcode: Int = -1 }
  case class FrameEntry  (`type`: Int, local: List[Any], stack: List[Any])                      extends Instruction { def opcode: Int = -1 }
  case class LineNumber  (line: Int, start: Label)                                              extends Instruction { def opcode: Int = -1 }

  abstract class AsmToScala {

    def labelIndex(l: t.LabelNode): Int

    def op(i: t.AbstractInsnNode): Int = i.getOpcode

    def convert(instructions: List[t.AbstractInsnNode]): List[Instruction] = instructions map apply

    private def lst[T](xs: java.util.List[T]): List[T] = if (xs == null) Nil else xs.asScala.toList

    // Heterogenous List[Any] is used in FrameNode: type information about locals / stack values
    // are stored in a List[Any] (Integer, String or LabelNode), see Javadoc of MethodNode#visitFrame.
    // Opcodes (eg Opcodes.INTEGER) and Reference types (eg "java/lang/Object") are returned unchanged,
    // LabelNodes are mapped to their LabelEntry.
    private def mapOverFrameTypes(is: List[Any]): List[Any] = is map {
      case i: t.LabelNode => applyLabel(i)
      case x => x
    }

    // avoids some casts
    private def applyLabel(l: t.LabelNode) = this(l: t.AbstractInsnNode).asInstanceOf[Label]

    private def apply(x: t.AbstractInsnNode): Instruction = x match {
      case i: t.FieldInsnNode          => Field        (op(i), i.owner, i.name, i.desc)
      case i: t.IincInsnNode           => Incr         (op(i), i.`var`, i.incr)
      case i: t.InsnNode               => Op           (op(i))
      case i: t.IntInsnNode            => IntOp        (op(i), i.operand)
      case i: t.JumpInsnNode           => Jump         (op(i), applyLabel(i.label))
      case i: t.LdcInsnNode            => Ldc          (op(i), i.cst: Any)
      case i: t.LookupSwitchInsnNode   => LookupSwitch (op(i), applyLabel(i.dflt), lst(i.keys) map (x => x: Int), lst(i.labels) map applyLabel)
      case i: t.TableSwitchInsnNode    => TableSwitch  (op(i), i.min, i.max, applyLabel(i.dflt), lst(i.labels) map applyLabel)
      case i: t.MethodInsnNode         => Method       (op(i), i.desc, i.name, i.owner, i.itf)
      case i: t.MultiANewArrayInsnNode => NewArray     (op(i), i.desc, i.dims)
      case i: t.TypeInsnNode           => TypeOp       (op(i), i.desc)
      case i: t.VarInsnNode            => VarOp        (op(i), i.`var`)
      case i: t.LabelNode              => Label        (labelIndex(i))
      case i: t.FrameNode              => FrameEntry   (i.`type`, mapOverFrameTypes(lst(i.local)), mapOverFrameTypes(lst(i.stack)))
      case i: t.LineNumberNode         => LineNumber   (i.line, applyLabel(i.start))
    }
  }

  import collection.mutable.{Map => MMap}

  /**
   * Bytecode is equal modula local variable numbering and label numbering.
   */
  def equivalentBytecode(as: List[Instruction], bs: List[Instruction], varMap: MMap[Int, Int] = MMap(), labelMap: MMap[Int, Int] = MMap()): Boolean = {
    def same(v1: Int, v2: Int, m: MMap[Int, Int]) = {
      if (m contains v1) m(v1) == v2
      else if (m.valuesIterator contains v2) false // v2 is already associated with some different value v1
      else { m(v1) = v2; true }
    }
    def sameVar(v1: Int, v2: Int) = same(v1, v2, varMap)
    def sameLabel(l1: Label, l2: Label) = same(l1.offset, l2.offset, labelMap)
    def sameLabels(ls1: List[Label], ls2: List[Label]) = ls1.length == ls2.length && (ls1, ls2).zipped.forall(sameLabel)

    def sameFrameTypes(ts1: List[Any], ts2: List[Any]) = ts1.length == ts2.length && (ts1, ts2).zipped.forall {
      case (t1: Label, t2: Label) => sameLabel(t1, t2)
      case (x, y) => x == y
    }

    if (as.isEmpty) bs.isEmpty
    else if (bs.isEmpty) false
    else ((as.head, bs.head) match {
      case (VarOp(op1, v1), VarOp(op2, v2))           => op1 == op2 && sameVar(v1, v2)
      case (Incr(op1, v1, inc1), Incr(op2, v2, inc2)) => op1 == op2 && sameVar(v1, v2) && inc1 == inc2

      case (l1 @ Label(_), l2 @ Label(_))                                                 => sameLabel(l1, l2)
      case (Jump(op1, l1), Jump(op2, l2))                                                 => op1 == op2 && sameLabel(l1, l2)
      case (LookupSwitch(op1, l1, keys1, ls1), LookupSwitch(op2, l2, keys2, ls2))         => op1 == op2 && sameLabel(l1, l2) && keys1 == keys2 && sameLabels(ls1, ls2)
      case (TableSwitch(op1, min1, max1, l1, ls1), TableSwitch(op2, min2, max2, l2, ls2)) => op1 == op2 && min1 == min2 && max1 == max2 && sameLabel(l1, l2) && sameLabels(ls1, ls2)
      case (LineNumber(line1, l1), LineNumber(line2, l2))                                 => line1 == line2 && sameLabel(l1, l2)
      case (FrameEntry(tp1, loc1, stk1), FrameEntry(tp2, loc2, stk2))                     => tp1 == tp2 && sameFrameTypes(loc1, loc2) && sameFrameTypes(stk1, stk2)

      // this needs to go after the above. For example, Label(1) may not equal Label(1), if before
      // the left 1 was associated with another right index.
      case (a, b) if a == b => true

      case _ => false
    }) && equivalentBytecode(as.tail, bs.tail, varMap, labelMap)
  }

  /**
   * Convert back a list of [[Instruction]]s to ASM land. The code is emitted into the parameter
   * `method`.
   */
  def applyToMethod(method: t.MethodNode, instructions: List[Instruction]): Unit = {
    val asmLabel = createLabelNodes(instructions)
    instructions.foreach(visitMethod(method, _, asmLabel))
  }

  private def createLabelNodes(instructions: List[Instruction]): Map[Label, asm.Label] = {
    val labels = instructions collect {
      case l: Label => l
    }
    assert(labels.distinct == labels, s"Duplicate labels in: $labels")
    labels.map(l => (l, new asm.Label())).toMap
  }

  private def frameTypesToAsm(l: List[Any], asmLabel: Map[Label, asm.Label]): List[Object] = l map {
    case l: Label => asmLabel(l)
    case x => x.asInstanceOf[Object]
  }

  private def visitMethod(method: t.MethodNode, instruction: Instruction, asmLabel: Map[Label, asm.Label]): Unit = instruction match {
    case Field(op, owner, name, desc)            => method.visitFieldInsn(op, owner, name, desc)
    case Incr(op, vr, incr)                      => method.visitIincInsn(vr, incr)
    case Op(op)                                  => method.visitInsn(op)
    case IntOp(op, operand)                      => method.visitIntInsn(op, operand)
    case Jump(op, label)                         => method.visitJumpInsn(op, asmLabel(label))
    case Ldc(op, cst)                            => method.visitLdcInsn(cst)
    case LookupSwitch(op, dflt, keys, labels)    => method.visitLookupSwitchInsn(asmLabel(dflt), keys.toArray, (labels map asmLabel).toArray)
    case TableSwitch(op, min, max, dflt, labels) => method.visitTableSwitchInsn(min, max, asmLabel(dflt), (labels map asmLabel).toArray: _*)
    case Method(op, owner, name, desc, itf)      => method.visitMethodInsn(op, owner, name, desc, itf)
    case NewArray(op, desc, dims)                => method.visitMultiANewArrayInsn(desc, dims)
    case TypeOp(op, desc)                        => method.visitTypeInsn(op, desc)
    case VarOp(op, vr)                           => method.visitVarInsn(op, vr)
    case l: Label                                => method.visitLabel(asmLabel(l))
    case FrameEntry(tp, local, stack)            => method.visitFrame(tp, local.length, frameTypesToAsm(local, asmLabel).toArray, stack.length, frameTypesToAsm(stack, asmLabel).toArray)
    case LineNumber(line, start)                 => method.visitLineNumber(line, asmLabel(start))
  }
}
