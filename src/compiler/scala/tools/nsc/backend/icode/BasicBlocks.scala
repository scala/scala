/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode

import compat.StringBuilder
import scala.tools.nsc.ast._
import scala.collection.mutable.{Map, Set, HashSet}
import scala.tools.nsc.util.Position
import scala.tools.nsc.backend.icode.analysis.ProgramPoint

trait BasicBlocks requires ICodes {
  import opcodes._

  /** This class represents a basic block. Each
   *  basic block contains a list of instructions that are
   *  either executed all, or none. No jumps
   *  to/from the "middle" of the basic block are allowed.
   */
  class BasicBlock (theLabel: int, val code: Code)
        extends AnyRef
        with ProgramPoint[BasicBlock] {

    /** The label of the block */
    val label = theLabel

    /** When set, the <code>emit</code> methods will be ignored. */
    var ignore: Boolean = false

    var preds: List[BasicBlock] = null

    /** Is this block the head of a while? */
    var loopHeader = false

    /** Local variables that are in scope at entry of this basic block. Used
     *  for debugging information.
     */
    var varsInScope: Set[Local] = HashSet.empty

    /** ICode instructions, used as temporary storage while emitting code.
     * Once closed is called, only the `instrs' array should be used.
     */
    private var instructionList: List[Instruction] = Nil

    private var _lastInstruction: Instruction = null

    private var closed: boolean = false

    private var instrs: Array[Instruction] = _
    private var touched = false

    def toList: List[Instruction] = {
      if (closed && touched)
        instructionList = List.fromArray(instrs)
      instructionList
    }

    def fromList(is: List[Instruction]): Unit = {
      instrs = toInstructionArray(is)
      closed = true
    }

    // public:

    /** Return the index of inst. Uses reference equality.
     *  Returns -1 if not found.
     */
    def indexOf(inst: Instruction): Int = {
      assert(closed)
      var i = 0
      while (i < instrs.length) {
        if (instrs(i) eq inst) return i
        i = i + 1
      }
      -1
    }

    /** Compute an hashCode for the block */
//    override def hashCode() = label;

    /** Apply a function to all the instructions of the block. */
    def traverse(f: Instruction => unit) = {
      if (!closed) {
        dump
        global.abort("Traversing an open block!: " + label)
      }
      instrs foreach f
    }

    def traverseBackwards(f: Instruction => Unit) = {
      var i = instrs.length - 1
      while (i >= 0) {
        f(instrs(i));
        i = i - 1
      }
    }

    /** The number of instructions in this basic block so far. */
    def size: Int =
      if (isClosed)
        instrs.length
      else
        instructionList.length

    /** Return the index of the instruction which produced the value
     *  consumed by the given instruction.
     */
    def findDef(pos: Int): Option[Int] = {
      assert(closed)
      var i = pos
      var d = 0
      while (i > 0) {
        i = i - 1
        val prod = instrs(i).produced
        if (prod > 0 && d == 0)
          return Some(i)
        d = d + (instrs(i).consumed - instrs(i).produced);
      }
      None
    }

    def findDefs(idx: Int, m: Int): List[(BasicBlock, Int)] = {
      assert(closed)
      var res: List[(BasicBlock, Int)] = Nil
      var i = idx
      var n = m
      // "I look for who produced the 'n' elements below the 'd' topmost slots of the stack"
      var d = 0
      while (n > 0) {
        i = i - 1
        val prod = instrs(i).produced
        if (prod > d) {
          res = (this, i) :: res
          n   = n - (prod - d)
        }
        d = d + (instrs(i).consumed - instrs(i).produced);
      }
      res
    }

    def findDefs(bb: BasicBlock, d: Int, n: Int): List[(BasicBlock, Int)] = {
      var i = bb.size
      while (n > 0 && i > 0) {

      }
      Nil
    }

    /** Return the n-th instruction. */
    def apply(n: Int): Instruction =
      if (closed)
        instrs(n)
      else
        instructionList.reverse(n)

    ///////////////////// Substitutions ///////////////////////

    /**
     * Replace the instruction at the given position. Used by labels when
     * they are anchored. It retains the position of the previous instruction.
     */
    def replaceInstruction(pos: Int, instr: Instruction): Boolean = {
      assert(closed, "Instructions can be replaced only after the basic block is closed")

      instr.pos = instrs(pos).pos
      instrs(pos) = instr
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
        if (instrs(i) == oldInstr) {
          newInstr.pos = oldInstr.pos
          instrs(i) = newInstr
          changed = true
        }
        i = i + 1
      }
      changed
    }

    /** Replaces <code>iold</code> with <code>is</code>. It does not update
     *  the position field in the newly inserted instrucitons, so it behaves
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
        i = i + 1;

      if (i < instrs.length) {
        val newInstrs = new Array[Instruction](instrs.length + is.length - 1);
        changed = true
        Array.copy(instrs, 0, newInstrs, 0, i)
        var j = i
        for (val x <- is) {
          newInstrs(j) = x
          j = j + 1
        }
        if (i + 1 < instrs.length)
          Array.copy(instrs, i + 1, newInstrs, j, instrs.length - i - 1)
        instrs = newInstrs;
      }

      changed
    }

    /** Removes instructions found at the given positions.
     *
     *  @param positions ...
     */
    def removeInstructionsAt(positions: Int*): Unit = {
      assert(closed)
      val removed = positions.toList
      val newInstrs = new Array[Instruction](instrs.length - positions.length)
      var i = 0
      var j = 0
      while (i < instrs.length) {
        if (!removed.contains(i)) {
          newInstrs(j) = instrs(i)
          j = j + 1
        }
        i = i + 1
      }
      instrs = newInstrs
    }

    /** Remove the last instruction of this basic block. It is
     *  fast for an open block, but slower when the block is closed.
     */
    def removeLastInstruction: Unit = {
      if (closed)
        removeInstructionsAt(size)
      else {
        instructionList = instructionList.tail
        touched = true
      }
    }

    /** Replaces all instructions found in the map.
     *
     *  @param map ...
     */
    def subst(map: Map[Instruction, Instruction]): Unit =
      if (!closed) substOnList(map) else {
        var i = 0
        while (i < instrs.length) {
          map get instrs(i) match {
            case Some(instr) => touched = replaceInstruction(i, instr)
            case None => ()
          }
          i = i + 1
        }
      }

    private def substOnList(map: Map[Instruction, Instruction]): Unit = {
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
    def emit(instr: Instruction): Unit =
      if (!instructionList.isEmpty)
        emit(instr, instructionList.head.pos)
      else
        emit(instr, Position.NOPOS)

    def emit(instr: Instruction, pos: Int) = {
      if (closed) {
        print()
        Console.println("trying to emit: " + instr)
      }
      assert (!closed || ignore, "BasicBlock closed")

      if (!ignore) {
        touched = true
        instr.pos = pos
        instructionList = instr :: instructionList
        _lastInstruction = instr
      }
    }

    /** Close the block */
    def close = {
       assert(instructionList.length > 0,
              "Empty block.")
      closed = true
      instructionList = instructionList.reverse
      instrs = toInstructionArray(instructionList)
    }

    def open = {
      closed = false
      ignore = false
      instructionList = instructionList.reverse  // prepare for appending to the head
    }

    def clear: Unit = {
      instructionList = Nil
      instrs = null
      preds  = null
    }

    def isEmpty: Boolean = instructionList.isEmpty

    /** Enter ignore mode: new 'emit'ted instructions will not be
     *  added to this basic block. It makes the generation of THROW
     *  and RETURNs easier.
     */
    def enterIgnoreMode = ignore = true

    def exitIgnoreMode = {
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
    def toInstructionArray(l: List[Instruction]): Array[Instruction] = {
      var array = new Array[Instruction](l.length)
      var i: Int = 0

      l foreach (x => { array(i) = x; i = i + 1 })
      array
    }

    def isClosed = closed

    // TODO: Take care of exception handlers!
    def successors : List[BasicBlock] = if (isEmpty) Nil else
      lastInstruction match {
        case JUMP (where) => List(where)
        case CJUMP(success, failure, _, _) => success :: failure :: Nil
        case CZJUMP(success, failure, _, _) => success :: failure :: Nil
        case SWITCH(_,labels) => labels
        case RETURN(_) => Nil
        case THROW() => Nil
        case _ =>
          if (isClosed) {
            dump
            global.abort("The last instruction is not a control flow instruction: " + lastInstruction)
          }
          else Nil
      }

    /** Returns the precessors of this block, in the current 'code' chunk.
     *  This is signifficant only if there are exception handlers, which live
     *  in different code 'chunks' than the rest of the method.
     */
    def predecessors: List[BasicBlock] = {
      preds = code.blocks.elements.filter (.successors.contains(this)).toList
      preds
    }

    override def equals(other: Any): Boolean = other match {
      case that: BasicBlock => that.label == label && that.code == code
      case _ => false
    }

    override def hashCode = label

    // Instead of it, rather use a printer
    def print() : unit = print(java.lang.System.out)

    def print(out: java.io.PrintStream) : unit = {
      out.println("block #"+label+" :")
      toList.foreach(i => out.println("  " + i))
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
  }

}
