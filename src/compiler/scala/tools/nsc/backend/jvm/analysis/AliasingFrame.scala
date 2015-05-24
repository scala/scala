package scala.tools.nsc
package backend.jvm
package analysis

import scala.annotation.switch
import scala.collection.{mutable, immutable}
import scala.tools.asm.Opcodes
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis.{Analyzer, Value, Frame, Interpreter}
import opt.BytecodeUtils._

object AliasingFrame {
  private var _idCounter: Long = 0l
  private def nextId = { _idCounter += 1; _idCounter }
}

class AliasingFrame[V <: Value](nLocals: Int, nStack: Int) extends Frame[V](nLocals, nStack) {
  import Opcodes._

  // Auxiliary constructor required for implementing `AliasingAnalyzer.newFrame`
  def this(src: Frame[_ <: V]) {
    this(src.getLocals, src.getMaxStackSize)
    init(src)
  }

  /**
   * For each slot (entry in the `values` array of the frame), an id that uniquely represents
   * the object stored in it. If two values have the same id, they are aliases of the same
   * object.
   */
  private val aliasIds: Array[Long] = Array.fill(nLocals + nStack)(AliasingFrame.nextId)

  /**
   * The object alias id of for a value index.
   */
  def aliasId(entry: Int) = aliasIds(entry)

  /**
   * Returns the indices of the values array which are aliases of the object `id`.
   */
  def valuesWithAliasId(id: Long): Set[Int] = immutable.BitSet.empty ++ aliasIds.indices.iterator.filter(i => aliasId(i) == id)

  /**
   * The set of aliased values for a given entry in the `values` array.
   */
  def aliasesOf(entry: Int): Set[Int] = valuesWithAliasId(aliasIds(entry))

  /**
   * Define a new alias. For example, given
   *   var a = this       // this, a have the same aliasId
   * then an assignment
   *   b = a
   * will set the same the aliasId for `b`.
   */
  private def newAlias(assignee: Int, source: Int): Unit = {
    aliasIds(assignee) = aliasIds(source)
  }

  /**
   * An assignment
   *   a = someUnknownValue()
   * sets a fresh alias id for `a`.
   * A stack value is also removed from its alias set when being consumed.
   */
  private def removeAlias(assignee: Int): Unit = {
    aliasIds(assignee) = AliasingFrame.nextId
  }

  override def execute(insn: AbstractInsnNode, interpreter: Interpreter[V]): Unit = {
    // Make the extendsion methods easier to use (otherwise we have to repeat `this`.stackTop)
    def stackTop: Int = this.stackTop
    def peekStack(n: Int): V = this.peekStack(n)

    // the val pattern `val (p, c) = f` still allocates a tuple (https://github.com/scala-opt/scala/issues/28)
    val prodCons = InstructionStackEffect(insn, this) // needs to be called before super.execute, see its doc
    val consumed = prodCons._1
    val produced = prodCons._2

    super.execute(insn, interpreter)

    (insn.getOpcode: @switch) match {
      case ALOAD =>
        newAlias(assignee = stackTop, source = insn.asInstanceOf[VarInsnNode].`var`)

      case DUP =>
        val top = stackTop
        newAlias(assignee = top, source = top - 1)

      case DUP_X1 =>
        val top = stackTop
        newAlias(assignee = top,     source = top - 1)
        newAlias(assignee = top - 1, source = top - 2)
        newAlias(assignee = top - 2, source = top)

      case DUP_X2 =>
        // Check if the second element on the stack is size 2
        // https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html#jvms-6.5.dup_x2
        val isSize2 = peekStack(1).getSize == 2
        val top = stackTop
        newAlias(assignee = top,     source = top - 1)
        newAlias(assignee = top - 1, source = top - 2)
        if (isSize2) {
          // Size 2 values on the stack only take one slot in the `values` array
          newAlias(assignee = top - 2, source = top)
        } else {
          newAlias(assignee = top - 2, source = top - 3)
          newAlias(assignee = top - 3, source = top)
        }

      case DUP2 =>
        val isSize2 = peekStack(0).getSize == 2
        val top = stackTop
        if (isSize2) {
          newAlias(assignee = top, source = top - 1)
        } else {
          newAlias(assignee = top - 1, source = top - 3)
          newAlias(assignee = top,     source = top - 2)
        }

      case DUP2_X1 =>
        val isSize2 = peekStack(0).getSize == 2
        val top = stackTop
        if (isSize2) {
          newAlias(assignee = top,     source = top - 1)
          newAlias(assignee = top - 1, source = top - 2)
          newAlias(assignee = top - 2, source = top)
        } else {
          newAlias(assignee = top,     source = top - 2)
          newAlias(assignee = top - 1, source = top - 3)
          newAlias(assignee = top - 2, source = top - 4)
          newAlias(assignee = top - 4, source = top)
          newAlias(assignee = top - 5, source = top - 1)
        }

      case DUP2_X2 =>
        val top = stackTop
        // https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html#jvms-6.5.dup2_x2
        val v1isSize2 = peekStack(0).getSize == 2
        if (v1isSize2) {
          newAlias(assignee = top,     source = top - 1)
          newAlias(assignee = top - 1, source = top - 2)
          val v2isSize2 = peekStack(1).getSize == 2
          if (v2isSize2) {
            // Form 4
            newAlias(assignee = top - 2, source = top)
          } else {
            // Form 2
            newAlias(assignee = top - 2, source = top - 3)
            newAlias(assignee = top - 3, source = top)
          }
        } else {
          newAlias(assignee = top,     source = top - 2)
          newAlias(assignee = top - 1, source = top - 3)
          newAlias(assignee = top - 2, source = top - 4)
          val v3isSize2 = peekStack(2).getSize == 2
          if (v3isSize2) {
            // Form 3
            newAlias(assignee = top - 3, source = top)
            newAlias(assignee = top - 4, source = top - 1)
          } else {
            // Form 1
            newAlias(assignee = top - 3, source = top - 5)
            newAlias(assignee = top - 4, source = top)
            newAlias(assignee = top - 5, source = top - 1)
          }
        }

      case SWAP =>
        val top = stackTop
        val idTop = aliasIds(top)
        aliasIds(top)     = aliasIds(top - 1)
        aliasIds(top - 1) = idTop

      case opcode =>
        if (opcode == ASTORE) {
          // Not a separate case because we need to remove the consumed stack value from alias sets after.
          val stackTopBefore = stackTop - produced + consumed
          val local = insn.asInstanceOf[VarInsnNode].`var`
          newAlias(assignee = local, source = stackTopBefore)
          // if the value written is size 2, it overwrites the subsequent slot, which is then no
          // longer an alias of anything. see the corresponding case in `Frame.execute`.
          if (getLocal(local).getSize == 2)
            removeAlias(local + 1)

          // if the value at the preceding index is size 2, it is no longer valid, so we remove its
          // aliasing. see corresponding case in `Frame.execute`
          if (local > 0) {
            val precedingValue = getLocal(local - 1)
            if (precedingValue != null && precedingValue.getSize == 2)
              removeAlias(local - 1)
          }
        }

        // Remove consumed stack values from aliasing sets.
        // Example: iadd
        //  - before: local1, local2, stack1, consumed1, consumed2
        //  - after:  local1, local2, stack1, produced1             // stackTop = 3
        val firstConsumed = stackTop - produced + 1                 // firstConsumed = 3
        for (i <- 0 until consumed)
          removeAlias(firstConsumed + i)                            // remove aliases for 3 and 4

        // We don't need to set the aliases ids for the produced values: the aliasIds array already
        // contains fresh ids for non-used stack values (ensured by removeAlias).
    }
  }

  /**
   * Merge the AliasingFrame `other` into this AliasingFrame.
   *
   * Aliases that are common in both frames are kept. Example:
   *
   * var x, y = null
   * if (...) {
   *   x = a
   *   y = a     // (x, y, a) are aliases
   * } else {
   *   x = a
   *   y = b     // (x, a) and (y, b)
   * }
   * [...]       // (x, a)
   */
  override def merge(other: Frame[_ <: V], interpreter: Interpreter[V]): Boolean = {
    val valuesChanged = super.merge(other, interpreter)
    var aliasesChanged = false
    val aliasingOther = other.asInstanceOf[AliasingFrame[_]]
    for (i <- aliasIds.indices) {
      val thisAliases = aliasesOf(i)
      val thisNotOther = thisAliases diff (thisAliases intersect aliasingOther.aliasesOf(i))
      if (thisNotOther.nonEmpty) {
        aliasesChanged = true
        thisNotOther foreach removeAlias
      }
    }
    valuesChanged || aliasesChanged
  }

  override def init(src: Frame[_ <: V]): Frame[V] = {
    super.init(src)
    compat.Platform.arraycopy(src.asInstanceOf[AliasingFrame[_]].aliasIds, 0, aliasIds, 0, aliasIds.length)
    this
  }
}

/**
 * An analyzer that uses AliasingFrames instead of bare Frames. This can be used when an analysis
 * needs to track aliases, but doesn't require a more specific Frame subclass.
 */
class AliasingAnalyzer[V <: Value](interpreter: Interpreter[V]) extends Analyzer[V](interpreter) {
  override def newFrame(nLocals: Int, nStack: Int): AliasingFrame[V] = new AliasingFrame(nLocals, nStack)
  override def newFrame(src: Frame[_ <: V]): AliasingFrame[V] = new AliasingFrame(src)
}
