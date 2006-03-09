/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import scala.tools.nsc.ast._;
import scala.collection.mutable.Map;
import scala.tools.nsc.util.Position;

trait BasicBlocks requires ICodes {
  import opcodes._;

  /** This class represents a basic block. Each
   *  basic block contains a list of instructions that are
   *  either executed all, or none. No jumps
   *  to/from the "middle" of the basic block are allowed.
   */
  class BasicBlock (theLabel: int, val code: Code) {

    /** The type stack at the begining of the block */
    var initialStack : TypeStack = null;

    /** The label of the block */
    val label = theLabel;

    /** The substitute variables of the block
     *  in the case of a recursive method */
    var substituteVars : List[Symbol] = null;

    /** The stack at the end of the block */
    var endStack : TypeStack = null;

    /** When set, the 'emit' methods will be ignored. */
    var ignore: Boolean = false;

    var preds: List[BasicBlock] = null;

    /** ICode instructions, used as temporary storage while emitting code.
     * Once closed is called, only the `instrs' array should be used.
     */
    private var instructionList: List[Instruction] = Nil;

    private var _lastInstruction: Instruction = null;

    private var closed: boolean = false;

    private var instrs: Array[Instruction] = _;

    // public:

    /** Compute an hashCode for the block */
    override def hashCode() = label;

    /** Apply a function to all the instructions of the block. */
    def traverse(f: Instruction => unit) = {
      if (!closed) {
        dump;
        global.abort("Traversing an open block!: " + label);
      }
      instrs foreach f;
    }

    def traverseBackwards(f: Instruction => Unit) = {
      var i = instrs.length - 1;
      while (i >= 0) {
        f(instrs(i));
        i = i - 1
      }
    }

    /** The number of instructions in this basic block so far. */
    def size: Int =
      if (isClosed)
        instrs.length;
      else
        instructionList.length;

    /** Initialize the stack of the block, must be done before evaluation
     *  the type stack  */
    def initStack(stack : TypeStack) = {
      if (initialStack == null) {
        initialStack = stack;
        endStack = null;
      }
    }

    ///////////////////// Substitutions ///////////////////////

    /**
     * Replace the instruction at the given position. Used by labels when
     * they are anchored.
     */
    def replaceInstruction(pos: Int, instr: Instruction): Boolean = {
      assert(closed, "Instructions can be replaced only after the basic block is closed");

      instrs(pos) = instr;
      true
    }

    /**
     * Replace the given instruction with the new one.
     * Returns `true' if it actually changed something.
     */
    def replaceInstruction(oldInstr: Instruction, newInstr: Instruction): Boolean = {
      assert(closed, "Instructions can be replaced only after the basic block is closed");

      var i = 0;
      var changed = false;
      while (i < instrs.length && !changed) {
        if (instrs(i) == oldInstr) {
          instrs(i) = newInstr;
          changed = true;
        }
        i = i + 1;
      }
      changed
    }

    /** Replace all instructions found in the map. */
    def subst(map: Map[Instruction, Instruction]) =
      if (!closed) substOnList(map) else {
        var i = 0;
        while (i < instrs.length) {
          map get instrs(i) match {
            case Some(instr) => replaceInstruction(i, instr);
            case None => ();
          }
          i = i + 1;
        }
      }

    private def substOnList(map: Map[Instruction, Instruction]): Unit = {
      def subst(l: List[Instruction]): List[Instruction] = l match {
        case Nil => Nil
        case x :: xs =>
          map.get(x) match {
            case Some(newInstr) => newInstr :: subst(xs);
            case None => x :: subst(xs);
          }
      }

      instructionList = subst(instructionList);
    }

    ////////////////////// Emit //////////////////////


    /** Add a new instruction at the end of the block,
     *  using the same source position as the last emitted instruction
     */
    def emit(instr: Instruction): Unit = {
      if (!instructionList.isEmpty)
        emit(instr, instructionList.head.pos);
      else
        emit(instr, Position.NOPOS);
    }

    def emit(instr: Instruction, pos: Int) = {
      assert (!closed || ignore, "BasicBlock closed");

      if (!ignore) {
        instr.pos = pos;
        instructionList = instr :: instructionList;
        _lastInstruction = instr;
      }
    }

    /** Close the block */
    def close = {
       assert(instructionList.length > 0,
              "Empty block.");
      closed = true;
      instructionList = instructionList.reverse;
      instrs = toInstructionArray(instructionList);
    }

    def open = {
      closed = false;
    }

    def instructions: List[Instruction] = instructionList;

    def clear: Unit = {
      instructionList = Nil;
      instrs = null;
    }

    def isEmpty: Boolean = instructionList.isEmpty;

    /** Enter ignore mode: new 'emit'ted instructions will not be
     * added to this basic block. It makes the generation of THROW
     * and RETURNs easier.
     */
    def enterIgnoreMode = ignore = true;

    def exitIgnoreMode = {
      assert(ignore, "Exit ignore mode when not in ignore mode.");
      ignore = false;
    }

    /** Return the last instruction of this basic block. */
    def lastInstruction =
      if (closed)
        instrs(instrs.length - 1)
      else
        instructionList.head;

    def firstInstruction =
      if (closed)
        instrs(0)
      else
        instructionList.last;

    /** Convert the list to an array */
    def toInstructionArray(l: List[Instruction]): Array[Instruction] = {
      var array = new Array[Instruction](l.length);
      var i: Int = 0;

      l foreach (x => { array(i) = x; i = i + 1 });
      array
    }

    def isClosed = closed;

    // TODO: Take care of exception handlers!
    def successors : List[BasicBlock] = if (isEmpty) Nil else
      lastInstruction match {
        case JUMP (where) => List(where);
        case CJUMP(success, failure, _, _) => success :: failure :: Nil;
        case CZJUMP(success, failure, _, _) => success :: failure :: Nil;
        case SWITCH(_,labels) => labels;
        case RETURN(_) => Nil;
        case THROW() => Nil;
        case _ =>
          if (isClosed) {
            dump;
	    global.abort("The last instruction is not a control flow instruction: " + lastInstruction);
          }
          else Nil;
      }

    /** Returns the precessors of this block, in the current 'code' chunk.
     *  This is signifficant only if there are exception handlers, which live
     *  in different code 'chunks' than the rest of the method.
     */
    def predecessors: List[BasicBlock] = {
      if (preds == null)
        preds = code.blocks.elements.filter (p => (p.successors contains this)).toList;
      preds
    }

    override def equals(other: Any): Boolean = (
      other.isInstanceOf[BasicBlock] &&
      other.asInstanceOf[BasicBlock].label == label &&
      other.asInstanceOf[BasicBlock].code  == code
    );

    // Instead of it, rather use a printer
    def print() : unit = print(System.out);

    def print(out: java.io.PrintStream) : unit = {
      out.println("block #"+label+" :");
      instructionList.reverse.foreach(
        (i: Instruction) => out.println("  "+i));
      out.print("Successors: ");
      successors.foreach((x: BasicBlock) => out.print(" "+x.label.toString()));
      out.println();
    }

    def fullString: String = {
      val buf = new StringBuffer();
      buf.append("Block ").append(label.toString());
      buf.append("\nSuccessors: ").append(successors);
      buf.append("\nPredecessors: ").append(predecessors);
      buf.toString()
    }

    override def toString(): String = "" + label;
  }

}
