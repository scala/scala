/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package backend
package icode

//import scala.tools.nsc.ast._
import scala.collection.mutable.{Map, Set}
import scala.collection.mutable.LinkedHashSet
import scala.tools.nsc.util.{Position,NoPosition}
import scala.tools.nsc.backend.icode.analysis.ProgramPoint

trait BasicBlocks {
  self: ICodes =>
  import opcodes._

  /** This class represents a basic block. Each
   *  basic block contains a list of instructions that are
   *  either executed all, or none. No jumps
   *  to/from the "middle" of the basic block are allowed (modulo exceptions).
   */
  class BasicBlock(val label: Int, val method: IMethod)
        extends AnyRef
        with ProgramPoint[BasicBlock]
        with Seq[Instruction] {

    import BBFlags._

    def code = method.code

    /** Flags of this basic block. */
    private var flags: Int = 0

    /** Does this block have the given flag? */
    def hasFlag(flag: Int): Boolean = (flags & flag) != 0

    /** Set the given flag. */
    private def setFlag(flag: Int): Unit = flags |= flag
    private def resetFlag(flag: Int) {
      flags &= ~flag
    }

    /** Is this block closed? */
    def closed: Boolean = hasFlag(CLOSED)
    def closed_=(b: Boolean) = if (b) setFlag(CLOSED) else resetFlag(CLOSED)

    /** When set, the <code>emit</code> methods will be ignored. */
    def ignore: Boolean = hasFlag(IGNORING)
    def ignore_=(b: Boolean) = if (b) setFlag(IGNORING) else resetFlag(IGNORING)

    /** Is this block the head of a while? */
    def loopHeader = hasFlag(LOOP_HEADER)
    def loopHeader_=(b: Boolean) =
      if (b) setFlag(LOOP_HEADER) else resetFlag(LOOP_HEADER)

    /** Is this block the start block of an exception handler? */
    def exceptionHandlerStart = hasFlag(EX_HEADER)
    def exceptionHandlerStart_=(b: Boolean) =
      if (b) setFlag(EX_HEADER) else resetFlag(EX_HEADER)

    /** Has this basic block been modified since the last call to 'successors'? */
    def touched = hasFlag(DIRTYSUCCS)
    def touched_=(b: Boolean) = if (b) {
      setFlag(DIRTYSUCCS | DIRTYPREDS)
    } else {
      resetFlag(DIRTYSUCCS | DIRTYPREDS)
    }

    // basic blocks start in a dirty state
    setFlag(DIRTYSUCCS | DIRTYPREDS)

    /** Cached predecessors. */
    var preds: List[BasicBlock] = null

    /** Local variables that are in scope at entry of this basic block. Used
     *  for debugging information.
     */
    var varsInScope: Set[Local] = new LinkedHashSet()

    /** ICode instructions, used as temporary storage while emitting code.
     * Once closed is called, only the `instrs' array should be used.
     */
    private var instructionList: List[Instruction] = Nil

    private var _lastInstruction: Instruction = null

    private var instrs: Array[Instruction] = _

    override def toList: List[Instruction] = {
      if (closed)
        instrs.toList
      else instructionList
    }

    /** Return an iterator over the instructions in this basic block. */
    def iterator: Iterator[Instruction] =
      if (closed) instrs.iterator else instructionList.reverse.iterator

    /** return the underlying array of instructions */
    def getArray: Array[Instruction] = {
      assert(closed)
      instrs
    }

    def fromList(is: List[Instruction]) {
      code.touched = true
      instrs = toInstructionArray(is)
      closed = true
    }

    /** Return the index of inst. Uses reference equality.
     *  Returns -1 if not found.
     */
    def indexOf(inst: Instruction): Int = {
      assert(closed)
      var i = 0
      while (i < instrs.length) {
        if (instrs(i) eq inst) return i
        i += 1
      }
      -1
    }

    /** Compute an hashCode for the block */
//    override def hashCode() = label;

    /** Apply a function to all the instructions of the block. */
    override def foreach[U](f: Instruction => U) = {
      if (!closed) {
        dump
        global.abort("Traversing an open block!: " + label)
      }
      instrs foreach f
    }

    /** The number of instructions in this basic block so far. */
    def length: Int =
      if (closed) instrs.length else instructionList.length

    /** Return the index of the instruction which produced the value
     *  consumed by the given instruction.
     */
    def findDef(pos: Int): Option[Int] = {
      assert(closed)
      var i = pos
      var d = 0
      while (i > 0) {
        i -= 1
        val prod = instrs(i).produced
        if (prod > 0 && d == 0)
          return Some(i)
        d += (instrs(i).consumed - instrs(i).produced)
      }
      None
    }


    /** Return the n-th instruction. */
    def apply(n: Int): Instruction =
      if (closed) instrs(n) else instructionList.reverse(n)

    ///////////////////// Substitutions ///////////////////////

    /**
     * Replace the instruction at the given position. Used by labels when
     * they are anchored. It retains the position of the previous instruction.
     */
    def replaceInstruction(pos: Int, instr: Instruction): Boolean = {
      assert(closed, "Instructions can be replaced only after the basic block is closed")
      instr.setPos(instrs(pos).pos)
      instrs(pos) = instr
      code.touched = true
      true
    }

    /**
     * Replace the given instruction with the new one.
     * Returns `true' if it actually changed something.
     * It retains the position of the previous instruction.
     */
    def replaceInstruction(oldInstr: Instruction, newInstr: Instruction): Boolean = {
      assert(closed, "Instructions can be replaced only after the basic block is closed")

      var i = 0
      var changed = false
      while (i < instrs.length && !changed) {
        if (instrs(i) eq oldInstr) {
          newInstr.setPos(oldInstr.pos)
          instrs(i) = newInstr
          changed = true
          code.touched = true
        }
        i += 1
      }
      changed
    }

    /** Replaces <code>iold</code> with <code>is</code>. It does not update
     *  the position field in the newly inserted instructions, so it behaves
     *  differently than the one-instruction versions of this function.
     *
     *  @param iold ..
     *  @param is   ..
     *  @return     ..
     */
    def replaceInstruction(iold: Instruction, is: List[Instruction]): Boolean = {
      assert(closed, "Instructions can be replaced only after the basic block is closed")

      var i = 0
      var changed = false

      while (i < instrs.length && (instrs(i) ne iold))
        i += 1

      if (i < instrs.length) {
        val newInstrs = new Array[Instruction](instrs.length + is.length - 1);
        changed = true
        code.touched = true

        Array.copy(instrs, 0, newInstrs, 0, i)
        var j = i
        for (x <- is) {
          newInstrs(j) = x
          j += 1
        }
        if (i + 1 < instrs.length)
          Array.copy(instrs, i + 1, newInstrs, j, instrs.length - i - 1)
        instrs = newInstrs;
      }

      changed
    }

    /** Insert instructions in 'is' immediately after index 'idx'. */
    def insertAfter(idx: Int, is: List[Instruction]) {
      assert(closed, "Instructions can be replaced only after the basic block is closed")

      var i = idx + 1
      if (i < instrs.length) {
        val newInstrs = new Array[Instruction](instrs.length + is.length);
        Array.copy(instrs, 0, newInstrs, 0, i)
        var j = i
        for (x <- is) {
          newInstrs(j) = x
          j += 1
        }
        if (i + 1 < instrs.length)
          Array.copy(instrs, i + 1, newInstrs, j, instrs.length - i)
        instrs = newInstrs;
      }
      code.touched = true
    }

    /** Removes instructions found at the given positions.
     *
     *  @param positions ...
     */
    def removeInstructionsAt(positions: Int*) {
      assert(closed)
      val removed = positions.toList
      val newInstrs = new Array[Instruction](instrs.length - positions.length)
      var i = 0
      var j = 0
      while (i < instrs.length) {
        if (!removed.contains(i)) {
          newInstrs(j) = instrs(i)
          j += 1
        }
        i += 1
      }
      instrs = newInstrs
      code.touched = true
    }

    /** Remove the last instruction of this basic block. It is
     *  fast for an open block, but slower when the block is closed.
     */
    def removeLastInstruction {
      if (closed)
        removeInstructionsAt(size)
      else {
        instructionList = instructionList.tail
        code.touched = true
      }
    }

    /** Replaces all instructions found in the map.
     *
     *  @param map ...
     */
    def subst(map: Map[Instruction, Instruction]) {
      if (!closed) substOnList(map) else {
        var i = 0
        while (i < instrs.length) {
          map get instrs(i) match {
            case Some(instr) =>
              val changed = replaceInstruction(i, instr)
              code.touched |= changed
            case None => ()
          }
          i += 1
        }
      }
    }

    private def substOnList(map: Map[Instruction, Instruction]) {
      def subst(l: List[Instruction]): List[Instruction] = l match {
        case Nil => Nil
        case x :: xs =>
          map.get(x) match {
            case Some(newInstr) => newInstr :: subst(xs)
            case None => x :: subst(xs)
          }
      }

      instructionList = subst(instructionList)
    }

    ////////////////////// Emit //////////////////////


    /** Add a new instruction at the end of the block,
     *  using the same source position as the last emitted instruction
     */
    def emit(instr: Instruction) {
      if (!instructionList.isEmpty)
        emit(instr, instructionList.head.pos)
      else
        emit(instr, NoPosition)
    }

    /** Emitting does not set touched to true. During code generation this is a hotspot and
     *  setting the flag for each emit is a waste. Caching should happen only after a block
     *  is closed, which sets the DIRTYSUCCS flag.
     */
    def emit(instr: Instruction, pos: Position) {
      if (closed) {
        print()
        Console.println("trying to emit: " + instr)
      }
      assert(!closed || ignore, "BasicBlock closed")

      if (!ignore) {
        instr.setPos(pos)
        instructionList = instr :: instructionList
        _lastInstruction = instr
      }
    }

    def emit(instrs: Seq[Instruction]) {
      instrs foreach (i => emit(i, i.pos))
    }

    /** The semantics of this are a little odd but it's designed to work
     *  seamlessly with the existing code.  It emits each supplied instruction,
     *  then closes the block.  The odd part is that if the instruction has
     *  pos == NoPosition, it calls the 1-arg emit, but otherwise it calls
     *  the 2-arg emit.  This way I could retain existing behavior exactly by
     *  calling setPos on any instruction using the two arg version which
     *  I wanted to include in a call to emitOnly.
     */
    def emitOnly(instrs: Instruction*) {
      instrs foreach (i => if (i.pos == NoPosition) emit(i) else emit(i, i.pos))
      this.close
    }

    /** Close the block */
    def close {
      assert(instructionList.length > 0, "Empty block.")
      closed = true
      setFlag(DIRTYSUCCS)
      instructionList = instructionList.reverse
      instrs = toInstructionArray(instructionList)
    }

    def open {
      assert(closed)
      closed = false
      ignore = false
      touched = true
      instructionList = instructionList.reverse  // prepare for appending to the head
    }

    def clear {
      instructionList = Nil
      instrs = null
      preds  = null
    }

    override def isEmpty: Boolean = instructionList.isEmpty

    /** Enter ignore mode: new 'emit'ted instructions will not be
     *  added to this basic block. It makes the generation of THROW
     *  and RETURNs easier.
     */
    def enterIgnoreMode = ignore = true

    def exitIgnoreMode {
      assert(ignore, "Exit ignore mode when not in ignore mode.")
      ignore = false
    }

    /** Return the last instruction of this basic block. */
    def lastInstruction =
      if (closed)
        instrs(instrs.length - 1)
      else
        instructionList.head

    def firstInstruction =
      if (closed)
        instrs(0)
      else
        instructionList.last

    /** Convert the list to an array */
    private def toInstructionArray(l: List[Instruction]): Array[Instruction] = {
      var array = new Array[Instruction](l.length)
      var i: Int = 0

      l foreach (x => { array(i) = x; i += 1 })
      array
    }

    /** Cached value of successors. Must be recomputed whenver a block in the current method is changed. */
    private var succs: List[BasicBlock] = Nil

    def successors : List[BasicBlock] = {
      if (touched) {
        resetFlag(DIRTYSUCCS)
        succs = if (isEmpty) Nil else {
          var res = lastInstruction match {
            case JUMP(whereto) => List(whereto)
            case CJUMP(success, failure, _, _) => failure :: success :: Nil
            case CZJUMP(success, failure, _, _) => failure :: success :: Nil
            case SWITCH(_, labels) => labels
            case RETURN(_) => Nil
            case THROW() => Nil
            case _ =>
              if (closed) {
                dump
                global.abort("The last instruction is not a control flow instruction: " + lastInstruction)
              }
              else Nil
          }
          method.exh.foreach {
            e: ExceptionHandler =>
              if (e.covers(this)) res = e.startBlock :: res
          }
          val res1 = res ++ exceptionalSucc(this, res)
          res1
        }
      }
//        println("reusing cached successors for " + this + " in method " + method)
      succs
    }

    def directSuccessors: List[BasicBlock] = {
      if (isEmpty) Nil else lastInstruction match {
        case JUMP(whereto) => List(whereto)
        case CJUMP(success, failure, _, _) => failure :: success :: Nil
        case CZJUMP(success, failure, _, _) => failure :: success :: Nil
        case SWITCH(_, labels) => labels
        case RETURN(_) => Nil
        case THROW() => Nil
        case _ =>
          if (closed) {
            dump
            global.abort("The last instruction is not a control flow instruction: " + lastInstruction)
          }
          else Nil
      }
    }

    /** Return a list of successors for 'b' that come from exception handlers
     *  covering b's (non-exceptional) successors. These exception handlers
     *  might not cover 'b' itself. This situation corresponds to an
     *  exception being thrown as the first thing of one of b's successors.
     */
    private def exceptionalSucc(b: BasicBlock, succs: List[BasicBlock]): List[BasicBlock] = {
      def findSucc(s: BasicBlock): List[BasicBlock] = {
        val ss = method.exh flatMap { h =>
          if (h.covers(s) /*&& mayThrow(h.startBlock.firstInstruction)*/) List(h.startBlock) else Nil
        }
        ss ++ (ss flatMap findSucc)
      }

      succs.flatMap(findSucc).distinct
    }

    /** Returns the predecessors of this block.     */
    def predecessors: List[BasicBlock] = {
      if (hasFlag(DIRTYPREDS)) {
        resetFlag(DIRTYPREDS)
        preds = code.blocks.iterator.filter (_.successors.contains(this)).toList
      }
      preds
    }

    override def equals(other: Any): Boolean = other match {
      case that: BasicBlock => (that.label == label) && (that.code == code)
      case _ => false
    }

    override def hashCode = label * 41 + code.hashCode

    // Instead of it, rather use a printer
    def print() { print(java.lang.System.out) }

    def print(out: java.io.PrintStream) {
      out.println("block #"+label+" :")
      foreach(i => out.println("  " + i))
      out.print("Successors: ")
      successors.foreach((x: BasicBlock) => out.print(" "+x.label.toString()))
      out.println()
    }

    def fullString: String = {
      val buf = new StringBuilder()
      buf.append("Block ").append(label.toString())
      buf.append("\nSuccessors: ").append(successors)
      buf.append("\nPredecessors: ").append(predecessors)
      buf.toString()
    }

    override def toString(): String = "" + label

    def flagsString: String =
      ("block " + label + (
         if (hasFlag(LOOP_HEADER)) " <loopheader> "
         else if (hasFlag(IGNORING)) " <ignore> "
         else if (hasFlag(EX_HEADER)) " <exheader> "
         else if (hasFlag(CLOSED)) " <closed> "
         else if (hasFlag(DIRTYSUCCS)) " <dirtysuccs> "
         else if (hasFlag(DIRTYPREDS)) " <dirtypreds> "
         else ""
      ))
  }

}

object BBFlags {
  /** This block is a loop header (was translated from a while). */
  final val LOOP_HEADER = 0x00000001

  /** Ignoring mode: emit instructions are dropped. */
  final val IGNORING    = 0x00000002

  /** This block is the header of an exception handler. */
  final val EX_HEADER   = 0x00000004

  /** This block is closed. No new instructions can be added. */
  final val CLOSED      = 0x00000008

  /** Code has been changed, recompute successors. */
  final val DIRTYSUCCS     = 0x00000010

  /** Code has been changed, recompute predecessors. */
  final val DIRTYPREDS  = 0x00000020
}
