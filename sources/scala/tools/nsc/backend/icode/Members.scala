/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import scala.collection.mutable.HashMap;
import scala.collection.mutable.HashSet;

trait Members: Global {

  /** This class represents the intermediate code of a method.
   */
  class Code(label: String) {

    /** The set of all blocks */
    val blocks: HashSet[BasicBlock] = new HashSet;

    /** The start block of the method */
    var startBlock: BasicBlock = null;

    /** The stack produced by this method */
    var producedStack: TypeStack = null;

    private var currentLabel: int = 0;
    private var aTreeLabels: HashMap[Symbol, BasicBlock] = new HashMap;

    // Constructor code
    startBlock = newBlock;
    startBlock.initStack(new TypeStack);

    /** Apply a function to all basic blocks, for side-effects. */
    def traverse(f: BasicBlock => Unit) =
      traverseFeedBack((bb: BasicBlock, hm: HashMap[BasicBlock, boolean]) => f(bb));

    /* This method applies the given function to each basic block. */
    def traverseFeedBack(f: (BasicBlock, HashMap[BasicBlock, Boolean])  => Unit) = {
      val visited : HashMap[BasicBlock, Boolean] = new HashMap;
      visited ++= blocks.elements.map(x => Pair(x, false));

      var blockToVisit : List[BasicBlock] = startBlock::Nil;

      while (!blockToVisit.isEmpty) {
        blockToVisit match {
	  case b::xs =>
	    if (!visited(b)) {
	      f(b, visited);
	      blockToVisit = b.successors ::: xs;
	      visited += b -> true;
	    } else
	      blockToVisit = xs;
	}
      }
    }

    /** This methods returns a string representation of the ICode */
    override def toString() : String = "ICode '" + label + "'";

    /** This method print the code */
    def print() : unit = print(System.out);

    def print(out: java.io.PrintStream) : unit = {
      traverse((bb: BasicBlock) => {
        out.println("Block #" + bb.label);
        out.println("Substituable variables : ");
        if (bb.substituteVars != null)
	  bb.substituteVars.foreach(out.print);
        else
	  out.println(" {Empty} ");
        out.println("Instructions:");
        bb.traverse((ici: Instruction) =>
	  out.println("  "+ici.toString()));
        out.print  ("Successors: ");
        bb.successors.foreach((bb: BasicBlock) => out.print(bb.label+", "));
        out.println (""); // ?? Del
        out.println ();
      });
    }

    def logType = {
      log ("// Typing " + toString());
      traverse( (bb: BasicBlock) => {
        log ("Typing block #" + bb.label);
        var typer = new TypeStack;
        bb.traverse((ic: Instruction) => {
	  typer = typer.eval(ic);
	  log(ic.toString()+" -> "+typer.toString());
        });

      });
    }

    /* Compute a unique new label */
    def nextLabel = {
      currentLabel = currentLabel + 1;
      currentLabel;
    }

    /* Create a new block and append it to the list
     */
    def newBlock : BasicBlock = {
      val block = new BasicBlock(nextLabel);
      blocks += block;
      block;
    }
  }

  /** Represent a class in ICode */
  class IClass(val symbol: Symbol) {
    var fields: List[IField] = Nil;
    var methods: List[IMethod] = Nil;
    var cunit: CompilationUnit = _;

    def addField(f: IField): this.type = {
      fields = f :: fields;
      this
    }

    def addMethod(m: IMethod): this.type = {
      methods = m :: methods;
      this
    }

    def setCompilationUnit(unit: CompilationUnit): this.type = {
      this.cunit = unit;
      this
    }
  }

  /** Represent a field in ICode */
  class IField(val symbol: Symbol) {
  }

  /** Represent a method in ICode */
  class IMethod(val symbol: Symbol) {
    var code: Code = null;

    /** local variables and method parameters */
    var locals: List[Symbol] = Nil;

    /** method parameters */
    var params: List[Symbol] = Nil;

    def setCode(code: Code): IMethod = {
      this.code = code;
      this
    }

    def addLocal(sym: Symbol): Unit =
      if (!(locals contains sym))
        locals = sym :: locals;

    def addLocals(ls: List[Symbol]): Unit =
      ls foreach addLocal;

    def addParam(sym: Symbol): Unit =
      if (!(params contains sym))
        params = sym :: params;

    def addParams(as: List[Symbol]): Unit =
      as.foreach( (a: Symbol) => { addParam(a); addLocal(a); } )
  }
}
