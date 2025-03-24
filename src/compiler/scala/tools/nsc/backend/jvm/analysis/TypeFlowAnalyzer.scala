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

import scala.annotation.tailrec
import scala.tools.asm.{Opcodes, Type}
import scala.tools.asm.tree.{AbstractInsnNode, InsnNode, MethodNode}
import scala.tools.asm.tree.analysis.{Analyzer, BasicInterpreter, BasicValue}
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.analysis.TypeFlowInterpreter._
import scala.tools.nsc.backend.jvm.analysis.BackendUtils.LambdaMetaFactoryCall
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

abstract class TypeFlowInterpreter extends BasicInterpreter(scala.tools.asm.Opcodes.ASM7) {
  override def newParameterValue(isInstanceMethod: Boolean, local: Int, tpe: Type): BasicValue =
    new ParamValue(local, tpe)

  override def newValue(tp: Type): BasicValue = {
    if (tp == null) UninitializedValue
    else if (isRef(tp)) new SpecialAwareBasicValue(tp)
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

  override def naryOperation(insn: AbstractInsnNode, values: java.util.List[_ <: BasicValue]): BasicValue = {
    val v = super.naryOperation(insn, values)
    insn.getOpcode match {
      case Opcodes.INVOKEDYNAMIC => insn match {
        case LambdaMetaFactoryCall(_, _, _, _, _) => new LMFValue(v.getType)
        case _ => v
      }
      case _ => v
    }
  }

  def refLub(a: BasicValue, b: BasicValue): BasicValue

  @tailrec
  override final def merge(a: BasicValue, b: BasicValue): BasicValue = {
    if (a == b) a
    else if (a.isInstanceOf[SpecialValue] || b.isInstanceOf[SpecialValue]) merge(new SpecialAwareBasicValue(a.getType), new SpecialAwareBasicValue(b.getType))
    else if (isRef(a.getType) && isRef(b.getType)) refLub(a, b)
    else UninitializedValue
  }
}

object TypeFlowInterpreter {
  // Marker trait for BasicValue subclasses that add a special meaning on top of the value's `getType`.
  trait SpecialValue

  private val obj = Type.getObjectType("java/lang/Object")

  // A BasicValue with equality that knows about special versions
  class SpecialAwareBasicValue(tpe: Type) extends BasicValue(tpe) {
    override def equals(other: Any): Boolean = {
      this match {
        case tav: AaloadValue => other match {
          case oav: AaloadValue => tav.aaload == oav.aaload
          case _ => false
        }
        case _: LMFValue => other.isInstanceOf[LMFValue] && super.equals(other)
        case pv: ParamValue => other.isInstanceOf[ParamValue] && pv.local == other.asInstanceOf[ParamValue].local && super.equals(other)
        case _ => !other.isInstanceOf[SpecialValue] && super.equals(other) // A non-special value cannot equal a special value
      }
    }

    override def hashCode: Int = this match {
      case av: AaloadValue => av.aaload.hashCode
      case pv: ParamValue => pv.local + super.hashCode
      case _ => super.hashCode
    }
  }

  val ObjectValue = new SpecialAwareBasicValue(BasicValue.REFERENCE_VALUE.getType)
  val UninitializedValue = new SpecialAwareBasicValue(null)

  // In the interpreter, visiting an AALOAD, we don't know the type of the array
  // just by looking at the instruction. By using an AaloadValue for the value produced
  // by the AALOAD, we can go back to the AALOAD instruction and get the type of its input
  // (once the analysis is done). See preciseAaloadTypeDesc.
  // Note that the merge / refLub function discards AaloadValue instances (unless the merged values
  // denote the same AALOAD instruction), so if a value may have other producers than the AALOAD,
  // we just get Object.
  class AaloadValue(val aaload: InsnNode) extends SpecialAwareBasicValue(obj) with SpecialValue

  // Note: merging two LMFValue with the same underlying type gives a LMFValue, but if the
  // underlying types differ, the merge is just a BasicValue
  class LMFValue(tpe: Type) extends SpecialAwareBasicValue(tpe) with SpecialValue

  // Note: merging two ParamValue with the same underlying type gives a ParamValue, but if the
  // underlying types differ, the merge is just a BasicValue
  class ParamValue(val local: Int, tpe: Type) extends SpecialAwareBasicValue(tpe) with SpecialValue
}

/**
 * A [[TypeFlowInterpreter]] which collapses LUBs of non-equal reference types to Object.
 * This could be made more precise by looking up ClassBTypes for the two reference types and using
 * the `jvmWiseLUB` method.
 */
class NonLubbingTypeFlowInterpreter extends TypeFlowInterpreter {
  def refLub(a: BasicValue, b: BasicValue): BasicValue = ObjectValue
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
