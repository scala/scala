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

import java.util

import scala.annotation.switch
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis._
import scala.tools.asm.{MethodVisitor, Type}
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

/**
 * This class provides additional queries over ASM's built-in `SourceValue` analysis.
 *
 * The analysis computes for each value in a frame a set of source instructions, which are the
 * potential producers. Most instructions produce either nothing or a stack value. For example,
 * a `LOAD` instruction is the producer of the value pushed onto the stack. The exception are
 * `STORE` instructions, which produce a new value for a local variable slot, so they are used
 * as producers for the value they stored.
 *
 * Note that pseudo-instructions are used as initial producers for parameters and local variables.
 * See the documentation on class InitialProducer.
 *
 * This class implements the following queries over the data computed by the SourceValue analysis:
 *
 *   - producersForValueAt(insn, slot)
 *   - consumersOfValueAt(insn, slot)
 *
 *   - producersForInputsOf(insn)
 *   - consumersOfOutputsFrom(insn)
 *
 *   - initialProducersForValueAt(insn, slot)
 *   - ultimateConsumersOfValueAt(insn, slot)
 *
 *   - initialProducersForInputsOf(insn)
 *   - ultimateConsumersOfOutputsFrom(insn)
 *
 * The following operations are considered as copying operations:
 *   - xLOAD, xSTORE
 *   - DUP, DUP2, DUP_X1, DUP_X2, DUP2_X1, DUP2_X2
 *   - SWAP
 *   - CHECKCAST
 *
 * If ever needed, we could introduce a mode where primitive conversions (l2i) are considered as
 * copying operations.
 *
 * Note on performance: thee data flow analysis (SourceValue / SourceInterpreter, provided by ASM)
 * is roughly 2-3x slower than a simple analysis (like BasicValue). The reason is that the merge
 * function (merging producer sets) is more complex than merging simple basic values.
 * See also the doc comment in the package object `analysis`.
 */
class ProdConsAnalyzer(methodNode: MethodNode, classInternalName: InternalName) extends AsmAnalyzer(methodNode, classInternalName, new Analyzer(new InitialProducerSourceInterpreter)) {
  /**
   * Returns the potential producer instructions of a (local or stack) value in the frame of `insn`.
   * This method simply returns the producer information computed by the SourceValue analysis.
   */
  def producersForValueAt(insn: AbstractInsnNode, slot: Int): Set[AbstractInsnNode] = {
    frameAt(insn).getValue(slot).insns.asScala.toSet
  }

  /**
   * Returns the potential consumer instructions of a (local or stack) value in the frame of `insn`.
   * This is the counterpart of `producersForValueAt`.
   */
  def consumersOfValueAt(insn: AbstractInsnNode, slot: Int): Set[AbstractInsnNode] = {
    producersForValueAt(insn, slot).flatMap[AbstractInsnNode](prod => {
      val outputNumber = outputValueSlots(prod).indexOf(slot)
      _consumersOfOutputsFrom.get(prod).map(v => {
        v(outputNumber)
      }).getOrElse(Set.empty)
    })
  }

  /**
   * Returns the potential producer instructions of any of the values consumed by `insn`.
   */
  def producersForInputsOf(insn: AbstractInsnNode): Set[AbstractInsnNode] = {
    inputValues(insn).iterator.flatMap(v => v.insns.asScala).toSet
  }

  def consumersOfOutputsFrom(insn: AbstractInsnNode): Set[AbstractInsnNode] = insn match {
    case _: UninitializedLocalProducer                      => Set.empty
    case ParameterProducer(local)                           => consumersOfValueAt(methodNode.instructions.getFirst, local)
    case ExceptionProducer(handlerLabel, handlerStackTop)   => consumersOfValueAt(handlerLabel, handlerStackTop)
    case _ =>
      _consumersOfOutputsFrom.get(insn).map(v => Set.from[AbstractInsnNode](v.indices.iterator.flatMap(v.apply))).getOrElse(Set.empty)
  }

  /**
   * Returns the potential initial producer instructions of a value in the frame of `insn`.
   *
   * Unlike `producersForValueAt`, producers are tracked through copying instructions such as STORE
   * and LOAD. If the producer of the value is a LOAD, then the producers of the stored value(s) are
   * returned instead.
   */
  def initialProducersForValueAt(insn: AbstractInsnNode, slot: Int): Set[AbstractInsnNode] = {
    def initialProducers(insn: AbstractInsnNode, producedSlot: Int): Set[AbstractInsnNode] = {
      if (isCopyOperation(insn)) {
        val key = (insn, producedSlot)
        _initialProducersCache.getOrElseUpdate(key, {
          // prevent infinite recursion if an instruction is its own producer or consumer
          // see cyclicProdCons in ProdConsAnalyzerTest
          _initialProducersCache(key) = Set.empty
          val (sourceValue, sourceValueSlot) = copyOperationSourceValue(insn, producedSlot)
          sourceValue.insns.iterator.asScala.flatMap(initialProducers(_, sourceValueSlot)).toSet
        })
      } else {
        Set(insn)
      }
    }
    producersForValueAt(insn, slot).flatMap(initialProducers(_, slot))
  }

  /**
   * Returns the potential ultimate consumers of a value in the frame of `insn`. Consumers are
   * tracked through copying operations such as SOTRE and LOAD.
   */
  def ultimateConsumersOfValueAt(insn: AbstractInsnNode, slot: Int): Set[AbstractInsnNode] = {
    def ultimateConsumers(insn: AbstractInsnNode, consumedSlot: Int): Set[AbstractInsnNode] = {
      if (isCopyOperation(insn)) {
        val key = (insn, consumedSlot)
        _ultimateConsumersCache.getOrElseUpdate(key, {
          // prevent infinite recursion if an instruction is its own producer or consumer
          // see cyclicProdCons in ProdConsAnalyzerTest
          _ultimateConsumersCache(key) = Set.empty
          for {
            producedSlot     <- copyOperationProducedValueSlots(insn, consumedSlot)
            consumer         <- consumersOfValueAt(insn.getNext, producedSlot)
            ultimateConsumer <- ultimateConsumers(consumer, producedSlot)
          } yield ultimateConsumer
        })
      } else {
        Set(insn)
      }
    }
    consumersOfValueAt(insn, slot).flatMap(ultimateConsumers(_, slot))
  }

  def initialProducersForInputsOf(insn: AbstractInsnNode): Set[AbstractInsnNode] = {
    inputValueSlots(insn).flatMap(slot => initialProducersForValueAt(insn, slot)).toSet
  }

  def ultimateConsumersOfOutputsFrom(insn: AbstractInsnNode): Set[AbstractInsnNode] = insn match {
    case _: UninitializedLocalProducer => Set.empty
    case _ =>
      lazy val next = insn match {
        case _: ParameterProducer               => methodNode.instructions.getFirst
        case ExceptionProducer(handlerLabel, _) => handlerLabel
        case _                                  => insn.getNext
      }
      outputValueSlots(insn).flatMap(slot => ultimateConsumersOfValueAt(next, slot)).toSet
  }

  private def isCopyOperation(insn: AbstractInsnNode): Boolean = {
    isLoadOrStore(insn) || {
      (insn.getOpcode: @switch) match {
        case DUP | DUP_X1 | DUP_X2 | DUP2 | DUP2_X1 | DUP2_X2 | SWAP | CHECKCAST => true
        case _ => false
      }
    }
  }

  /**
   * Returns the value and its frame slot that `copyOp` copies into `producedSlot`.
   *
   * Example:
   *   - copyOp = DUP_X1, assume it produces slots 2,3,4
   *   - producedSlot = 3
   *   - the result is the value at slot 2 in the frame of `copyOp`
   */
  private def copyOperationSourceValue(copyOp: AbstractInsnNode, producedSlot: Int): (SourceValue, Int) = {
    val frame = frameAt(copyOp)

    // Index of the produced value. Example: DUP_X1 produces 3 values, so producedIndex is 0, 1 or 2,
    // where 0 corresponds to the lowest value on the stack.
    def producedIndex(numConsumed: Int) = {
      val numUsedSlotsBeforeCopy = frame.stackTop + 1
      producedSlot - (numUsedSlotsBeforeCopy - numConsumed)
    }

    def stackValue(n: Int) = (frame.peekStack(n), frame.stackTop - n)

    def dupX1Case = (producedIndex(2): @switch) match {
      case 0 | 2 => stackValue(0)
      case 1     => stackValue(1)
    }

    // Form 1 of dup_x2
    def dupX2Case = (producedIndex(3): @switch) match {
      case 0 | 3 => stackValue(0)
      case 1     => stackValue(2)
      case 2     => stackValue(1)
    }

    // Form 1 of dup2_x1
    def dup2X1Case = (producedIndex(3): @switch) match {
      case 0 | 3 => stackValue(1)
      case 1 | 4 => stackValue(0)
      case 2     => stackValue(2)
    }

    if (isLoad(copyOp)) {
      val slot = copyOp.asInstanceOf[VarInsnNode].`var`
      (frame.getLocal(slot), slot)
    } else if (isStore(copyOp)) {
      stackValue(0)
    } else (copyOp.getOpcode: @switch) match {
      case DUP =>
        stackValue(0) // the current stack top is the source of both produced values

      case DUP_X1 =>
        dupX1Case

      case DUP_X2 =>
        if (frame.peekStack(1).getSize == 2) dupX1Case
        else dupX2Case

      case DUP2 =>
        if (frame.peekStack(0).getSize == 2) stackValue(0)
        else {
          (producedIndex(2): @switch) match {
            case 0 | 2 => stackValue(1)
            case 1 | 3 => stackValue(0)
          }
        }

      case DUP2_X1 =>
        if (frame.peekStack(0).getSize == 2) dupX1Case
        else dup2X1Case

      case DUP2_X2 =>
        val v1isSize2 = frame.peekStack(0).getSize == 2
        if (v1isSize2) {
          val v2isSize2 = frame.peekStack(1).getSize == 2
          if (v2isSize2) dupX1Case // Form 4
          else dupX2Case // Form 2
        } else {
          val v3isSize2 = frame.peekStack(2).getSize == 2
          if (v3isSize2) dup2X1Case // Form 3
          else {
            // Form 1
            (producedIndex(4): @switch) match {
              case 0 | 4 => stackValue(1)
              case 1 | 5 => stackValue(0)
              case 2     => stackValue(3)
              case 3     => stackValue(2)
            }
          }
        }

      case SWAP =>
        if (producedIndex(2) == 0) stackValue(0)
        else stackValue(1)

      case CHECKCAST =>
        stackValue(0)
    }
  }

  /**
   * Returns the value slots into which `copyOp` copies the value at `consumedSlot`.
   *
   * Example:
   *   - copyOp = DUP_X1, assume it consumes slots 2,3 and produces 2,3,4
   *   - if consumedSlot == 2, the result is Set(3)
   *   - if consumedSlot == 3, the result is Set(2, 4)
   */
  private def copyOperationProducedValueSlots(copyOp: AbstractInsnNode, consumedSlot: Int): Set[Int] = {
    if (isStore(copyOp)) Set(copyOp.asInstanceOf[VarInsnNode].`var`)
    else {
      val nextFrame = frameAt(copyOp.getNext)
      val top = nextFrame.stackTop

      // Index of the consumed value. Example: DUP_X1 consumes two values, so consumedIndex is
      // 0 or 1, where 0 corresponds to the lower value on the stack.
      def consumedIndex(numProduced: Int) = {
        val numUsedSlotsAfterCopy = top + 1
        consumedSlot - (numUsedSlotsAfterCopy - numProduced)
      }

      def dupX1Case = (consumedIndex(3): @switch) match {
        case 0 => Set(top - 1)
        case 1 => Set(top - 2, top)
      }

      def dupX2Case = (consumedIndex(4): @switch) match {
        case 0 => Set(top - 2)
        case 1 => Set(top - 1)
        case 2 => Set(top - 3, top)
      }

      def dup2X1Case = (consumedIndex(5): @switch) match {
        case 0 => Set(top - 2)
        case 1 => Set(top - 4, top - 1)
        case 2 => Set(top - 3, top)
      }

      if (isLoad(copyOp)) Set(top)
      else (copyOp.getOpcode: @switch) match {
        case DUP =>
          Set(top - 1, top)

        case DUP_X1 =>
          dupX1Case

        case DUP_X2 =>
          if (nextFrame.peekStack(1).getSize == 2) dupX1Case
          else dupX2Case

        case DUP2 =>
          if (nextFrame.peekStack(0).getSize == 2) Set(top - 1, top)
          else (consumedIndex(4): @switch) match {
            case 0 => Set(top - 3, top - 1)
            case 1 => Set(top - 2, top)
          }

        case DUP2_X1 =>
          if (nextFrame.peekStack(0).getSize == 2) dupX1Case
          else dup2X1Case

        case DUP2_X2 =>
          val v1isSize2 = nextFrame.peekStack(0).getSize == 2
          if (v1isSize2) {
            val v2isSize2 = nextFrame.peekStack(1).getSize == 2
            if (v2isSize2) dupX1Case // Form 4
            else dupX2Case // Form 2
          } else {
            val v3isSize2 = nextFrame.peekStack(2).getSize == 2
            if (v3isSize2) dup2X1Case // Form 3
            else {
              // Form 1
              (consumedIndex(6): @switch) match {
                case 0 => Set(top - 3)
                case 1 => Set(top - 2)
                case 2 => Set(top - 5, top - 1)
                case 3 => Set(top - 4, top)
              }
            }
          }

        case SWAP =>
          if (consumedIndex(2) == 0) Set(top)
          else Set(top - 1)

        case CHECKCAST =>
          Set(top)
      }
    }
  }

  /** Returns the frame values consumed by executing `insn`. */
  private def inputValues(insn: AbstractInsnNode): Seq[SourceValue] = {
    lazy val frame = frameAt(insn)
    inputValueSlots(insn) map frame.getValue
  }

  /** Returns the frame slots holding the values consumed by executing `insn`. */
  private def inputValueSlots(insn: AbstractInsnNode): Seq[Int] = {
    if (insn.getOpcode == -1) return Seq.empty
    if (isLoad(insn)) {
      Seq(insn.asInstanceOf[VarInsnNode].`var`)
    } else if (insn.getOpcode == IINC) {
      Seq(insn.asInstanceOf[IincInsnNode].`var`)
    } else {
      val frame = frameAt(insn)
      val prodCons = InstructionStackEffect.forAsmAnalysis(insn, frame)
      val stackSize = frame.getLocals + frame.getStackSize
      (stackSize - InstructionStackEffect.cons(prodCons)) until stackSize
    }
  }

  /** Returns the frame slots holding the values produced by executing `insn`. */
  private def outputValueSlots(insn: AbstractInsnNode): Seq[Int] = insn match {
    case ParameterProducer(local)          => Seq(local)
    case UninitializedLocalProducer(local) => Seq(local)
    case ExceptionProducer(_, stackTop)    => Seq(stackTop)
    case _ =>
      if (insn.getOpcode == -1) return Seq.empty
      if (isStore(insn)) {
        Seq(insn.asInstanceOf[VarInsnNode].`var`)
      } else if (insn.getOpcode == IINC) {
        Seq(insn.asInstanceOf[IincInsnNode].`var`)
      } else {
        val frame = frameAt(insn)
        val prodCons = InstructionStackEffect.forAsmAnalysis(insn, frame)
        val nextFrame = frameAt(insn.getNext)
        val stackSize = nextFrame.getLocals + nextFrame.getStackSize
        (stackSize - InstructionStackEffect.prod(prodCons)) until stackSize
      }
  }

  /** For each instruction, a set of potential consumers of the produced values. */
  private lazy val _consumersOfOutputsFrom: Map[AbstractInsnNode, Vector[Set[AbstractInsnNode]]] = {
    var res = Map.empty[AbstractInsnNode, Vector[Set[AbstractInsnNode]]]
    for {
      insn <- methodNode.instructions.iterator.asScala
      frame = frameAt(insn)
      i <- inputValueSlots(insn)
      producer <- frame.getValue(i).insns.asScala
    } {
      val producedSlots = outputValueSlots(producer)
      val currentConsumers = res.getOrElse(producer, Vector.fill(producedSlots.size)(Set.empty[AbstractInsnNode]))
      val outputIndex = producedSlots.indexOf(i)
      res = res.updated(producer, currentConsumers.updated(outputIndex, currentConsumers(outputIndex) + insn))
    }
    res
  }

  private val _initialProducersCache:  mutable.HashMap[(AbstractInsnNode, Int), Set[AbstractInsnNode]] = mutable.HashMap.empty
  private val _ultimateConsumersCache: mutable.HashMap[(AbstractInsnNode, Int), Set[AbstractInsnNode]] = mutable.HashMap.empty
}

/**
 * A class for pseudo-instructions representing the initial producers of local values that have
 * no producer instruction in the method:
 *   - parameters, including `this`
 *   - uninitialized local variables
 *   - exception values in handlers
 *
 * The ASM built-in SourceValue analysis yields an empty producers set for such values. This leads
 * to ambiguities. Example (in Java one can re-assign parameter):
 *
 *   int foo(int a) {
 *     if (a == 0) a = 1;
 *     return a;
 *   }
 *
 * In the first frame of the method, the SourceValue for parameter `a` gives an empty set of
 * producer instructions.
 *
 * In the frame of the `IRETURN` instruction, the SourceValue for parameter `a` lists a single
 * producer instruction: the `ISTORE 1`. This makes it look as if there was a single producer for
 * `a`, where in fact it might still hold the parameter's initial value.
 */
abstract class InitialProducer extends AbstractInsnNode(-1) {
  override def getType: Int = throw new UnsupportedOperationException
  override def clone(labels: util.Map[LabelNode, LabelNode]): AbstractInsnNode = throw new UnsupportedOperationException
  override def accept(cv: MethodVisitor): Unit = throw new UnsupportedOperationException
}

case class ParameterProducer(local: Int)                                                extends InitialProducer
case class UninitializedLocalProducer(local: Int)                                       extends InitialProducer
case class ExceptionProducer[V <: Value](handlerLabel: LabelNode, handlerStackTop: Int) extends InitialProducer

class InitialProducerSourceInterpreter extends SourceInterpreter(scala.tools.asm.Opcodes.ASM7) {
  override def newParameterValue(isInstanceMethod: Boolean, local: Int, tp: Type): SourceValue = {
    new SourceValue(tp.getSize, ParameterProducer(local))
  }

  override def newEmptyValue(local: Int): SourceValue = {
    new SourceValue(1, UninitializedLocalProducer(local))
  }

  override def newExceptionValue(tryCatchBlockNode: TryCatchBlockNode, handlerFrame: Frame[SourceValue], exceptionType: Type): SourceValue = {
    // -1 to go from the number of locals to the (0-based) index of the last local
    // +1 because this value is about to be pushed onto `handlerFrame`
    val handlerStackTop = handlerFrame.getLocals - 1 + 1
    new SourceValue(1, ExceptionProducer(tryCatchBlockNode.handler, handlerStackTop))
  }
}
