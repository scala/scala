/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id:

import scalac.symtab.Symbol;
import scalac.symtab.Type;

import scalac.atree._;

package scala.tools.scalac.icode {

/** This class represents a basic block */
class IBasicBlock (theLabel: int) {

  //##################################################
  // Public fields

  /* The type stack at the begining of the block */
  var initialStack : ICTypeStack = null;

  /* The label of the block */
  val label = theLabel;

   /* The substitute variables of the block
   * in the case of a recursive method */
  var substituteVars : List[Symbol] = null;

  /* The stack at the end of the block */
  var endStack : ICTypeStack = null;

  /* The successors of this block */
  var successors : List[IBasicBlock] = Nil;

  /* Is the block closed*/
  var isClosedBlock  : boolean = false;

  //##################################################
  // Private fields

  /* ICode instructions */
  private var instructions : List[ICInstruction] = Nil;

  //##################################################
  // Public methods

  /* Compute an hashCode for the block */
  override def hashCode() = label;

  /* Apply a function to all the instructions of the block*/
  def bbTraverse(f: ICInstruction => unit) = instructions.reverse.foreach(f);

  /* Initialize the stack of the block, must be done before evaluing
  *  the type stack  */
  def initStack(stack : ICTypeStack) = {
    if (initialStack == null) {
      initialStack = stack;
      endStack = stack;
    }
  }

  /* Add a new instruction at the end of the block */
  def emit(instr: ICInstruction) = {
    assert (!isClosedBlock, "IBasicBlock closed.");
    instructions = instr::instructions;
  }

  /* Compute the type stack of the block */
  def typeBlock = {
    assert(initialStack != null, "Stack not initialized");
    bbTraverse((ic : ICInstruction) => endStack = endStack.eval(ic));
  }

  /* Add a successor to the block */
  def addSuccessor(s: IBasicBlock) =
    if (!successors.contains(s))
      successors = s::successors;

  /* Add a list of successors to the block */
  def addSuccessors(ss: List[IBasicBlock]) =
    ss.foreach(addSuccessor);

  /* Close the block */
  def close = isClosedBlock = true;;

  //##################################################
  // Public methods - printing (used for debbuging)
  // Instead of it, rather use a printer
  def print() : unit = print(System.out);

  def print(out: java.io.PrintStream) : unit = {
    out.println("block #"+label+" :");
    instructions.reverse.foreach(
      (i: ICInstruction) => out.println("  "+i));
    out.print("Successors: ");
    successors.foreach((x: IBasicBlock) => out.print(" "+x.label.toString()));
    out.println();
  }

  // ##################################################
  // Private methods

  // none.

}

}

