/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import scala.collection.mutable.HashMap;
import scala.collection.mutable.{Set, HashSet};
import scala.{Symbol => scala_Symbol};

import scala.tools.nsc.symtab.Flags;

trait Members requires ICodes {
  import global._;

  /**
   * This class represents the intermediate code of a method or
   * other multi-block piece of code, like exception handlers.
   */
  class Code(label: String) {

    /** The set of all blocks */
    val blocks: HashSet[BasicBlock] = new HashSet;

    /** The start block of the method */
    var startBlock: BasicBlock = null;

    /** The stack produced by this method */
    var producedStack: TypeStack = null;

    private var currentLabel: int = 0;

    // Constructor code
    startBlock = newBlock;
    startBlock.initStack(new TypeStack);


    def removeBlock(b: BasicBlock) = {
      if (settings.debug.value) {
        assert(blocks.forall(p => !(p.successors.contains(b))),
               "Removing block that is still referenced in method code " + label);
        if (b == startBlock)
          assert(b.successors.length == 1,
                 "Removing start block with more than one successor.");
      }

      if (b == startBlock)
        startBlock = b.successors.head;
      blocks -= b;
    }

    /**
     * Apply a function to all basic blocks, for side-effects. It starts at
     * the given startBlock and checks that are no predecessors of the given node.
     * Only blocks that are reachable via a path from startBlock are ever visited.
     */
    def traverseFrom(startBlock: BasicBlock, f: BasicBlock => Unit) = {
      val visited: Set[BasicBlock] = new HashSet();

      def traverse0(toVisit: List[BasicBlock]): Unit = toVisit match {
        case Nil => ();
        case b :: bs => if (!visited.contains(b)) {
          f(b);
          visited += b;
          traverse0(bs ::: b.successors);
        } else
          traverse0(bs);
      }
      assert(startBlock.predecessors == Nil,
             "Starting traverse from a block with predecessors: " + this);
      traverse0(startBlock :: Nil)
    }

    def traverse(f: BasicBlock => Unit) = blocks foreach f;

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

    /* Compute a unique new label */
    def nextLabel = {
      currentLabel = currentLabel + 1;
      currentLabel;
    }

    /* Create a new block and append it to the list
     */
    def newBlock: BasicBlock = {
      val block = new BasicBlock(nextLabel, this);
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

    override def toString() = symbol.fullNameString;

    def lookupField(s: Symbol) = fields find ((f) => f.symbol == s);
    def lookupMethod(s: Symbol) = methods find ((m) => m.symbol == s);
    def lookupMethod(s: Name) = methods find ((m) => m.symbol.name == s);
  }

  /** Represent a field in ICode */
  class IField(val symbol: Symbol) {
  }

  /**
   * Represents a method in ICode. Local variables contain
   * both locals and parameters, similar to the way the JVM
   * 'sees' them.
   *
   * Locals and parameters are added in reverse order, as they
   * are kept in cons-lists. The 'builder' is responsible for
   * reversing them and putting them back, when the generation is
   * finished (GenICode does that).
   */
  class IMethod(val symbol: Symbol) {
    var code: Code = null;

    /** The list of exception handlers, ordered from innermost to outermost. */
    var exh: List[ExceptionHandler] = Nil;
    var sourceFile: String = _;
    var returnType: TypeKind = _;

    /** local variables and method parameters */
    var locals: List[Local] = Nil;

    /** method parameters */
    var params: List[Local] = Nil;

    def setCode(code: Code): IMethod = {
      this.code = code;
      this
    }

    def addLocal(l: Local): Unit =
      if (!(locals contains l))
        locals = l :: locals;

    def addLocals(ls: List[Local]): Unit =
      ls foreach addLocal;

    def addParam(p: Local): Unit =
      if (!(params contains p)) {
        params = p :: params;
        locals = p :: locals;
      }

    def addParams(as: List[Local]): Unit =
      as foreach addParam;

    def lookupLocal(n: Name): Option[Local] =
      locals find ((l) => l.sym.name == n);

    def lookupLocal(sym: Symbol): Option[Local] =
      locals find ((l) => l.sym == sym);

    def addHandler(e: ExceptionHandler): Unit =
      exh = e :: exh;

    /** Is this method deferred ('abstract' in Java sense) */
    def isDeferred = (
      symbol.hasFlag(Flags.DEFERRED) ||
      symbol.owner.hasFlag(Flags.INTERFACE)
    );

    override def toString() = symbol.fullNameString;
  }

  /** Represent local variables and parameters */
  class Local(val sym: Symbol, val kind: TypeKind) {
    var index: Int = -1;

    override def equals(other: Any): Boolean = (
      other.isInstanceOf[Local] &&
      other.asInstanceOf[Local].sym == this.sym
    );

    override def toString(): String = sym.toString();
  }

}
