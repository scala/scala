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

import scala.annotation.switch
import scala.collection.AbstractIterator
import scala.collection.mutable
import scala.tools.asm.Opcodes
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.analysis.AliasSet.SmallBitSet
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

/**
 * A subclass of Frame that tracks aliasing of values stored in local variables and on the stack.
 *
 * Note: an analysis tracking aliases is roughly 5x slower than a usual analysis (assuming a simple
 * value domain with a fast merge function). For example, nullness analysis is roughly 5x slower
 * than a BasicValue analysis.
 *
 * See the doc of package object `analysis` for some notes on the performance of alias analysis.
 */
class AliasingFrame[V <: Value](nLocals: Int, nStack: Int) extends Frame[V](nLocals, nStack) {
  import Opcodes._

  // Auxiliary constructor required for implementing `AliasingAnalyzer.newFrame`
  def this(src: Frame[_ <: V]) = {
    this(src.getLocals, src.getMaxStackSize)
    init(src)
  }

  override def toString: String = super.toString + " - " + aliases.toList.filter(s => s != null && s.size > 1).map(_.toString).distinct.mkString(",")

  /**
   * For every value the set of values that are aliases of it.
   *
   * Invariants:
   *  - If `aliases(i) == null` then i has no aliases. This is equivalent to having
   *    `aliases(i) == SingletonSet(i)`.
   *  - If `aliases(i) != null` then `aliases(i) contains i`.
   *  - If `aliases(i) contains j` then `aliases(i) eq aliases(j)`, i.e., they are references to the
   *    same (mutable) AliasSet.
   */
  val aliases: Array[AliasSet] = new Array[AliasSet](getLocals + getMaxStackSize)

  /**
   * The set of aliased values for a given entry in the `values` array.
   */
  def aliasesOf(entry: Int): AliasSet = {
    if (aliases(entry) != null) aliases(entry)
    else {
      val init = new AliasSet(new AliasSet.SmallBitSet(entry, -1, -1, -1), 1)
      aliases(entry) = init
      init
    }
  }

  /**
   * Define a new alias. For example, an assignment
   *   b = a
   * adds b to the set of aliases of a.
   */
  private def newAlias(assignee: Int, source: Int): Unit = {
    removeAlias(assignee)
    val sourceAliases = aliasesOf(source)
    sourceAliases += assignee
    aliases(assignee) = sourceAliases
  }

  /**
   * Remove an alias. For example, an assignment
   *   a = someUnknownValue()
   * removes a from its former alias set.
   * As another example, stack values are removed from their alias sets when being consumed.
   */
  private def removeAlias(assignee: Int): Unit = {
    if (aliases(assignee) != null) {
      aliases(assignee) -= assignee
      aliases(assignee) = null
    }
  }

  /**
   * Define the alias set for a given value.
   */
  private def setAliasSet(assignee: Int, set: AliasSet): Unit = {
    if (aliases(assignee) != null) {
      aliases(assignee) -= assignee
    }
    aliases(assignee) = set
  }

  override def execute(insn: AbstractInsnNode, interpreter: Interpreter[V]): Unit = {
    // Make the extension methods easier to use (otherwise we have to repeat `this`.stackTop)
    def stackTop: Int = this.stackTop
    def peekStack(n: Int): V = this.peekStack(n)

    val prodCons = InstructionStackEffect.forAsmAnalysis(insn, this) // needs to be called before super.execute, see its doc
    val consumed = InstructionStackEffect.cons(prodCons)
    val produced = InstructionStackEffect.prod(prodCons)

    super.execute(insn, interpreter)

    (insn.getOpcode: @switch) match {
      case ILOAD | LLOAD | FLOAD | DLOAD | ALOAD =>
        newAlias(assignee = stackTop, source = insn.asInstanceOf[VarInsnNode].`var`)

      case IINC =>
        removeAlias(insn.asInstanceOf[IincInsnNode].`var`)
      
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
        // could be written more elegantly with higher-order combinators, but thinking of performance
        val top = stackTop

        def moveNextToTop(): Unit = {
          val nextAliases = aliases(top - 1)
          aliases(top) = nextAliases
          nextAliases -= (top - 1)
          nextAliases += top
        }

        if (aliases(top) != null) {
          val topAliases = aliases(top)
          if (aliases(top - 1) != null) moveNextToTop()
          else aliases(top) = null
          // move top to next
          aliases(top - 1) = topAliases
          topAliases -= top
          topAliases += (top - 1)
        } else {
          if (aliases(top - 1) != null) {
            moveNextToTop()
            aliases(top - 1) = null
          }
        }

      case opcode =>
        (opcode: @switch) match {
          case ISTORE | LSTORE | FSTORE | DSTORE | ASTORE =>
            // not a separate case: we re-use the code below that removes the consumed stack value from alias sets
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

          case _ =>
        }

        // Remove consumed stack values from aliasing sets.
        // Example: iadd
        //  - before: local1, local2, stack1, consumed1, consumed2
        //  - after:  local1, local2, stack1, produced1             // stackTop = 3
        val firstConsumed = stackTop - produced + 1                 // firstConsumed = 3
        for (i <- 0 until consumed)
          removeAlias(firstConsumed + i)                            // remove aliases for 3 and 4
    }
  }

  /**
   * When entering an exception handler, all values are dropped from the stack (and the exception
   * value is pushed). The ASM analyzer invokes `firstHandlerInstructionFrame.clearStack()`. To
   * ensure consistent aliasing sets, we need to remove the dropped values from aliasing sets.
   */
  override def clearStack(): Unit = {
    var i = getLocals
    val end = i + getStackSize
    while (i < end) {
      removeAlias(i)
      i += 1
    }
    super.clearStack()
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
   * [...]       // (x, a) -- merge of ((x, y, a)) and ((x, a), (y, b))
   */
  override def merge(other: Frame[_ <: V], interpreter: Interpreter[V]): Boolean = {
    // merge is the main performance hot spot of a data flow analysis.

    // in nullness analysis, super.merge (which actually merges the nullness values) takes 20% of
    // the overall analysis time.
    val valuesChanged = super.merge(other, interpreter)

    // in nullness analysis, merging the alias sets takes ~55% of the analysis time. therefore, this
    // code has been heavily optimized. most of the time is spent in the `hasNext` method of the
    // andNotIterator, see its comment.

    var aliasesChanged = false
    val aliasingOther = other.asInstanceOf[AliasingFrame[_]]

    val numValues = getLocals + getStackSize
    // assume (a, b) are aliases both in this frame, and the other frame. when merging the alias set
    // for a, we already see that a and b will be aliases in the final result. so we can skip over
    // merging the alias set for b. in this case, while merging the sets for a, knownOk(b) will be
    // set to `true`.
    val knownOk = new Array[Boolean](numValues)
    var i = 0
    while (i < numValues) {
      if (!knownOk(i)) {
        val thisAliases = this.aliases(i)
        val otherAliases = aliasingOther.aliases(i)
        if (thisAliases != null) {
          if (otherAliases == null) {
            if (thisAliases.size > 1) {
              aliasesChanged = true
              removeAlias(i)
            }
          } else {
            // The iterator yields elements that are in `thisAliases` but not in `otherAliases`.
            // As a side-effect, for every index `i` that is in both alias sets, the iterator sets
            // `knownOk(i) = true`: the alias sets for these values don't need to be merged again.
            val thisNotOtherIt = AliasSet.andNotIterator(thisAliases, otherAliases, knownOk)
            if (thisNotOtherIt.hasNext) {
              aliasesChanged = true
              val newSet = AliasSet.empty
              while (thisNotOtherIt.hasNext) {
                val next = thisNotOtherIt.next()
                newSet += next
                setAliasSet(next, newSet)
              }
            }
          }
        }
      }
      i += 1
    }

    valuesChanged || aliasesChanged
  }

  private def min(s: SmallBitSet) = {
    var r = s.a
    if (             s.b < r) r = s.b
    if (s.c != -1 && s.c < r) r = s.c
    if (s.d != -1 && s.d < r) r = s.d
    r
  }

  override def init(src: Frame[_ <: V]): Frame[V] = {
    super.init(src) // very quick (just an arraycopy)
    System.arraycopy(src.asInstanceOf[AliasingFrame[_]].aliases, 0, aliases, 0, aliases.length) // also quick

    val newSets = mutable.HashMap.empty[AliasSet, AliasSet]

    // the rest of this method (cloning alias sets) is the second performance˙hotspot (next to
    // AliasingFrame.merge). for nullness, it takes ~20% of the analysis time.
    // the difficulty here is that we have to clone the alias sets correctly. if two values a, b are
    // aliases, then aliases(a) eq aliases(b). we need to make sure to use the same clone for the
    // two values.

    var i = 0
    while (i < aliases.length) {
      val set = aliases(i)
      if (set != null) {
        // size cannot be 0 - alias sets are always at least singletons.
        // for sets of size 1-4, don't use the `newSets` map - lookup / update is slow
        if (set.size == 1) {
          aliases(i) = null
        } else if (set.size <= 4) {
          val small = set.set.asInstanceOf[AliasSet.SmallBitSet]
          val firstOfSet = i == min(small)
          if (firstOfSet) {
            val newSet = set.clone()
            aliases(small.a) = newSet
            aliases(small.b) = newSet
            if (small.c != -1) aliases(small.c) = newSet
            if (small.d != -1) aliases(small.d) = newSet
          }
        } else {
          // the actual hot spot is the hash map operations here: this is where almost all of the 20%
          // mentioned above is spent.
          // i also benchmarked an alternative implementation: keep an array of booleans for indexes
          // that already contain the cloned set. iterate through all elements of the cloned set and
          // assign the cloned set. this approach is 50% slower than using a hash map.
          if (newSets contains set) aliases(i) = newSets(set)
          else {
            val newSet = set.clone()
            newSets(set) = newSet
            aliases(i) = newSet
          }
        }
      }
      i += 1
    }
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

// Marker trait for AsmAnalyzers that use AliasingFrame
trait AliasingAsmAnalyzerMarker

class BasicAliasingAnalyzer(methodNode: MethodNode, classInternalName: InternalName)
  extends AsmAnalyzer[BasicValue](methodNode, classInternalName, new AliasingAnalyzer(new BasicInterpreter))
    with AliasingAsmAnalyzerMarker

/**
 * An iterator over Int (required to prevent boxing the result of next).
 */
abstract class IntIterator extends AbstractIterator[Int] {
  def hasNext: Boolean
  def next(): Int
}

/**
 * An efficient mutable bit set.
 *
 * @param set  Either a SmallBitSet or an Array[Long]
 * @param size The size of the set, useful for performance of certain operations
 */
class AliasSet(var set: Object /*SmallBitSet | Array[Long]*/, var size: Int) {
  import AliasSet._

  override def toString: String = iterator.toSet.mkString("<", ",", ">")

  /**
   * An iterator for the elements of this bit set. Note that only one iterator can be used at a
   * time. Also make sure not to change the underlying AliasSet during iteration.
   */
  def iterator: IntIterator = andNotIterator(this, empty, null)

  def +=(value: Int): Unit = (set: @unchecked) match {
    case s: SmallBitSet => (size: @switch) match {
      case 0 =>                                                     s.a = value; size = 1
      case 1 => if (value != s.a)                                 { s.b = value; size = 2 }
      case 2 => if (value != s.a && value != s.b)                 { s.c = value; size = 3 }
      case 3 => if (value != s.a && value != s.b && value != s.c) { s.d = value; size = 4 }
      case 4 =>
        if (value != s.a && value != s.b && value != s.c && value != s.d) {
          this.set = bsEmpty
          this.size = 0
          bsAdd(this, s.a)
          bsAdd(this, s.b)
          bsAdd(this, s.c)
          bsAdd(this, s.d)
          bsAdd(this, value)
        }
    }
    case _: Array[Long] =>
      bsAdd(this, value)
  }

  def -=(value: Int): Unit = (set: @unchecked) match {
    case s: SmallBitSet => (size: @switch) match {
      case 0 =>
      case 1 =>
        if      (value == s.a) { s.a = -1; size = 0 }
      case 2 =>
        if      (value == s.a) { s.a = s.b; s.b = -1; size = 1 }
        else if (value == s.b) {            s.b = -1; size = 1 }
      case 3 =>
        if      (value == s.a) { s.a = s.b; s.b = s.c; s.c = -1; size = 2 }
        else if (value == s.b) {            s.b = s.c; s.c = -1; size = 2 }
        else if (value == s.c) {                       s.c = -1; size = 2 }
      case 4 =>
        if      (value == s.a) { s.a = s.b; s.b = s.c; s.c = s.d; s.d = -1; size = 3 }
        else if (value == s.b) {            s.b = s.c; s.c = s.d; s.d = -1; size = 3 }
        else if (value == s.c) {                       s.c = s.d; s.d = -1; size = 3 }
        else if (value == s.d) {                                  s.d = -1; size = 3 }
    }
    case _: Array[Long] =>
      bsRemove(this, value)
      if (this.size == 4)
        this.set = bsToSmall(this.set.asInstanceOf[Array[Long]])
  }

  override def clone(): AliasSet = {
    val resSet: Object = (set: @unchecked) match {
      case s: SmallBitSet => new SmallBitSet(s.a, s.b, s.c, s.d)
      case bits: Array[Long] => bits.clone()
    }
    new AliasSet(resSet, this.size)
  }
}

object AliasSet {
  def empty = new AliasSet(new SmallBitSet(-1, -1, -1, -1), 0)

  final class SmallBitSet(var a: Int, var b: Int, var c: Int, var d: Int) {
    override def toString = s"($a, $b, $c, $d)"
  }

  def bsEmpty: Array[Long] = new Array[Long](1)

  private def bsEnsureCapacity(set: Array[Long], index: Int): Array[Long] = {
    if (index < set.length) set
    else {
      var newLength = set.length
      while (index >= newLength) newLength *= 2
      val newSet = new Array[Long](newLength)
      Array.copy(set, 0, newSet, 0, set.length)
      newSet
    }
  }

  def bsAdd(set: AliasSet, bit: Int): Unit = {
    val bits = set.set.asInstanceOf[Array[Long]]
    val index = bit >> 6
    val resSet = bsEnsureCapacity(bits, index)
    val before = resSet(index)
    val result = before | (1L << bit)
    if (result != before) {
      resSet(index) = result
      set.set = resSet
      set.size += 1
    }
  }

  def bsRemove(set: AliasSet, bit: Int): Unit = {
    val bits = set.set.asInstanceOf[Array[Long]]
    val index = bit >> 6
    if (index < bits.length) {
      val before = bits(index)
      val result = before & ~(1L << bit)
      if (result != before) {
        bits(index) = result
        set.size -= 1
      }
    }
  }

  def bsContains(set: Array[Long], bit: Int): Boolean = {
    val index = bit >> 6
    bit >= 0 && index < set.length && (set(index) & (1L << bit)) != 0L
  }

//  var sizesHist: Array[Int] = new Array[Int](1000)

  /**
   * Convert a bit array to a SmallBitSet. Requires the bit array to contain exactly four bits.
   */
  def bsToSmall(bits: Array[Long]): SmallBitSet = {
    var a = -1
    var b = -1
    var c = -1
    var i = 0
    val end = bits.length * 64
    while (i < end) {
      if (bsContains(bits, i)) {
        if (a == -1) a = i
        else if (b == -1) b = i
        else if (c == -1) c = i
        else return new SmallBitSet(a, b, c, i)
      }
      i += 1
    }
    null
  }

  /**
   * An iterator that yields the elements that are in one bit set and not in another (&~).
   */
  private class AndNotIt(setA: AliasSet, setB: AliasSet, thisAndOther: Array[Boolean]) extends IntIterator {
    // values in the first bit set
    private var a, b, c, d = -1
    private var xs: Array[Long] = null

    // values in the second bit set
    private var notA, notB, notC, notD = -1
    private var notXs: Array[Long] = null

    // holds the next value of `x`, `y` or `z` that should be returned. assigned in hasNext
    private var abcdNext = -1

    // counts through elements in the `xs` bit set
    private var i = 0
    // true if the current value of `i` should be returned by this iterator
    private var iValid = false

    (setA.set: @unchecked) match {
      case s: SmallBitSet => a = s.a; b = s.b; c = s.c; d = s.d
      case bits: Array[Long] => xs = bits
    }

    (setB.set: @unchecked) match {
      case s: SmallBitSet => notA = s.a; notB = s.b; notC = s.c; notD = s.d
      case bits: Array[Long] => notXs = bits
    }

    // for each value that exists both in this AND (&) the other bit, `thisAndOther` is set to true.
    // hacky side-effect, used for performance of AliasingFrame.merge.
    private def setThisAndOther(x: Int) = if (thisAndOther != null) thisAndOther(x) = true

    private def checkABCD(x: Int, num: Int): Boolean = {
      // assert(x == a && num == 1 || x == b && num == 2 || ...)
      x != -1 && {
        val otherHasA = x == notA || x == notB  || x == notC || x == notD || (notXs != null && bsContains(notXs, x))
        if (otherHasA) setThisAndOther(x)
        else abcdNext = x
        (num: @switch) match {
          case 1 => a = -1
          case 2 => b = -1
          case 3 => c = -1
          case 4 => d = -1
        }
        !otherHasA
      }
    }

    // main performance hot spot
    private def checkXs = {
      (xs != null) && {
        val end = xs.length * 64

        while (i < end && {
          val index = i >> 6
          if (xs(index) == 0L) { // boom. for nullness, this saves 35% of the overall analysis time.
            i = ((index + 1) << 6) - 1 // -1 required because i is incremented in the loop body
            true
          } else {
            val mask = 1L << i
            // if (mask > xs(index)) we could also advance i to the next value, but that didn't pay off in benchmarks
            val thisHasI = (xs(index) & mask) != 0L
            !thisHasI || {
              val otherHasI = i == notA || i == notB || i == notC || i == notD || (notXs != null && index < notXs.length && (notXs(index) & mask) != 0L)
              if (otherHasI) setThisAndOther(i)
              otherHasI
            }
          }
        }) i += 1

        iValid = i < end
        iValid
      }
    }

    // this is the main hot spot of alias analysis. for nullness, 38% of the overall analysis time
    // is spent here. within hasNext, almost the entire time is spent in `checkXs`.
    //
    def hasNext: Boolean = iValid || abcdNext != -1 || checkABCD(a, 1) || checkABCD(b, 2) || checkABCD(c, 3) || checkABCD(d, 4) || checkXs

    def next(): Int = {
      if (hasNext) {
        if (abcdNext != -1) {
          val r = abcdNext; abcdNext = -1; r
        } else {
          val r = i; i += 1; iValid = false; r
        }
      } else Iterator.empty.next()
    }
  }

//  The number of bits in a bit array. Useful for debugging.
//  def bsSize(bits: Array[Long]) = {
//    var r = 0
//    var i = 0
//    while (i < bits.length) {
//      r += java.lang.Long.bitCount(bits(i))
//      i += 1
//    }
//    r
//  }

  /**
   * An iterator returning the elements in a that are not also in b (a &~ b).
   *
   * If `thisAndOther` is non-null, the iterator sets thisAndOther(i) to true for every value that
   * is both in a and b (&).
   */
  def andNotIterator(a: AliasSet, b: AliasSet, thisAndOther: Array[Boolean]): IntIterator = new AndNotIt(a, b, thisAndOther)
}
