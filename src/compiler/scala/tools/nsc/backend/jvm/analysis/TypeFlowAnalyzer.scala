package scala.tools.nsc
package backend.jvm
package analysis

import scala.tools.asm.{Opcodes, Type}
import scala.tools.asm.tree.{AbstractInsnNode, InsnNode, MethodNode}
import scala.tools.asm.tree.analysis.{Analyzer, BasicInterpreter, BasicValue}
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.analysis.TypeFlowInterpreter.AaloadValue
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

abstract class TypeFlowInterpreter extends BasicInterpreter(scala.tools.asm.Opcodes.ASM7_EXPERIMENTAL) {
  override def newValue(tp: Type): BasicValue = {
    if (tp == null) super.newValue(tp)
    else if (isRef(tp)) new BasicValue(tp)
    else super.newValue(tp)
  }

  def isRef(tp: Type): Boolean = tp != null && (tp.getSort match {
    case Type.OBJECT | Type.ARRAY => true
    case _ => false
  })

  override def binaryOperation(insn: AbstractInsnNode, value1: BasicValue, value2: BasicValue): BasicValue = insn.getOpcode match {
    case Opcodes.AALOAD => new AaloadValue(insn.asInstanceOf[InsnNode]) // see [[AaloadValue]]
    case _ => super.binaryOperation(insn, value1, value2)
  }

  def refLub(a: BasicValue, b: BasicValue): BasicValue

  override def merge(a: BasicValue, b: BasicValue): BasicValue = {
    if (a.isInstanceOf[AaloadValue] || b.isInstanceOf[AaloadValue]) BasicValue.REFERENCE_VALUE
    if (a == b) a
    else if (isRef(a.getType) && isRef(b.getType)) refLub(a, b)
    else BasicValue.UNINITIALIZED_VALUE
  }
}

object TypeFlowInterpreter {
  // In the interpreter, visiting an AALOAD, we don't know the type of the array
  // just by looking at the instruction. By using an AaloadValue for the value produced
  // by the AALOAD, we can go back to the AALOAD instruction and get the type of its input
  // (once the analysis is done). See preciseAaloadTypeDesc.
  // Note that the merge / refLub function discards AaloadValue instances, so if a value
  // may have other producers than the AALOAD, we just see Object.
  class AaloadValue(val aaload: InsnNode) extends BasicValue(Type.getObjectType("java/lang/Object"))
}

/**
 * A [[TypeFlowInterpreter]] which collapses LUBs of non-equal reference types to Object.
 * This could be made more precise by looking up ClassBTypes for the two reference types and using
 * the `jvmWiseLUB` method.
 */
class NonLubbingTypeFlowInterpreter extends TypeFlowInterpreter {
  def refLub(a: BasicValue, b: BasicValue): BasicValue = BasicValue.REFERENCE_VALUE // java/lang/Object
}

class NonLubbingTypeFlowAnalyzer(methodNode: MethodNode, classInternalName: InternalName) extends AsmAnalyzer(methodNode, classInternalName, new Analyzer(new NonLubbingTypeFlowInterpreter)) {
  // see [[AaloadValue]]
  def preciseAaloadTypeDesc(value: BasicValue): String = value match {
    case aaloadValue: AaloadValue =>
      val f = frameAt(aaloadValue.aaload)
      val arrDesc = f.getValue(f.stackTop - 1).getType.getDescriptor
      // TODO make it safe in case we don't get an array type
      arrDesc.substring(1) // drop `[`

    case _ => value.getType.getDescriptor
  }
}
