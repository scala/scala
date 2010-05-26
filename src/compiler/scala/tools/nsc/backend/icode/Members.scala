/* NSC -- new scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package icode

import java.io.PrintWriter

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Set, HashSet, ListBuffer}
import scala.{Symbol => scala_Symbol}

import scala.tools.nsc.symtab.Flags

trait Members { self: ICodes =>
  import global._

  /**
   * This class represents the intermediate code of a method or
   * other multi-block piece of code, like exception handlers.
   */
  class Code(label: String, method: IMethod) {

    /** The set of all blocks */
    val blocks: ListBuffer[BasicBlock] = new ListBuffer

    /** The start block of the method */
    var startBlock: BasicBlock = null

    /** The stack produced by this method */
    var producedStack: TypeStack = null

    private var currentLabel: Int = 0
    private var _touched = false

    def touched = _touched
    def touched_=(b: Boolean): Unit = if (b) {
      blocks foreach (_.touched = true)
      _touched = true
    } else
      _touched = false

    // Constructor code
    startBlock = newBlock

    def removeBlock(b: BasicBlock) {
      if (settings.debug.value) {
        assert(blocks.forall(p => !(p.successors.contains(b))),
               "Removing block that is still referenced in method code " + b + "preds: " + b.predecessors);
        if (b == startBlock)
          assert(b.successors.length == 1,
                 "Removing start block with more than one successor.");
      }

      if (b == startBlock)
        startBlock = b.successors.head;
      blocks -= b
      assert(!blocks.contains(b))
      for (handler <- method.exh if handler.covers(b))
        handler.covered -= b

      touched = true
    }

    /** This methods returns a string representation of the ICode */
    override def toString() : String = "ICode '" + label + "'";

    /* Compute a unique new label */
    def nextLabel: Int = {
      currentLabel += 1
      currentLabel
    }

    /* Create a new block and append it to the list
     */
    def newBlock: BasicBlock = {
      touched = true
      val block = new BasicBlock(nextLabel, method);
      blocks += block;
      block
    }
  }

  /** Represent a class in ICode */
  class IClass(val symbol: Symbol) {
    var fields: List[IField] = Nil
    var methods: List[IMethod] = Nil
    var cunit: CompilationUnit = _
    var bootstrapClass: Option[String] = None

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

    override def toString() = symbol.fullName

    def lookupField(s: Symbol) = fields find (_.symbol == s)
    def lookupMethod(s: Symbol) = methods find (_.symbol == s)
    def lookupMethod(s: Name) = methods find (_.symbol.name == s)

    /* determines whether or not this class contains a static ctor. */
    def containsStaticCtor: Boolean = methods.exists(_.isStaticCtor)
    /* returns this methods static ctor if it has one. */
    def lookupStaticCtor: Option[IMethod] = methods.find(_.isStaticCtor)
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
    var code: Code = null
    var native = false

    /** The list of exception handlers, ordered from innermost to outermost. */
    var exh: List[ExceptionHandler] = Nil
    var sourceFile: String = _
    var returnType: TypeKind = _

    var recursive: Boolean = false

    /** local variables and method parameters */
    var locals: List[Local] = Nil

    /** method parameters */
    var params: List[Local] = Nil

    def setCode(code: Code): IMethod = {
      this.code = code;
      this
    }

    def addLocal(l: Local): Local =
      locals find (l.==) match {
        case Some(loc) => loc
        case None =>
          locals = l :: locals;
          l
      }

    def addLocals(ls: List[Local]) {
      ls foreach addLocal
    }

    def addParam(p: Local) {
      if (!(params contains p)) {
        params = p :: params;
        locals = p :: locals;
      }
    }

    def addParams(as: List[Local]) {
      as foreach addParam
    }

    def lookupLocal(n: Name): Option[Local] =
      locals find ((l) => l.sym.name == n);

    def lookupLocal(sym: Symbol): Option[Local] =
      locals find ((l) => l.sym == sym);

    def addHandler(e: ExceptionHandler) {
      exh = e :: exh
    }

    /** Is this method deferred ('abstract' in Java sense) */
    def isDeferred = (
      symbol.hasFlag(Flags.DEFERRED) ||
      symbol.owner.hasFlag(Flags.INTERFACE) ||
      native
    );

    def isStatic: Boolean = symbol.isStaticMember

    /* determines whether or not this method is the class static constructor. */
    def isStaticCtor: Boolean = isStatic && symbol.rawname == nme.CONSTRUCTOR

    override def toString() = symbol.fullName

    import opcodes._
    def checkLocals: Unit = if (code ne null) {
      Console.println("[checking locals of " + this + "]")
      for (bb <- code.blocks; i <- bb) i match {
        case LOAD_LOCAL(l) =>
          if (!this.locals.contains(l))
            Console.println("Local " + l + " is not declared in " + this)
        case STORE_LOCAL(l) =>
          if (!this.locals.contains(l))
            Console.println("Local " + l + " is not declared in " + this)
        case _ => ()
      }
    }

    /** Merge together blocks that have a single successor which has a
     * single predecessor. Exception handlers are taken into account (they
     * might force to break a block of straight line code like that).
     *
     * This method should be most effective after heavy inlining.
     */
    def normalize: Unit = if (this.code ne null) {
      import scala.collection.mutable.{Map, HashMap}
      val nextBlock: Map[BasicBlock, BasicBlock] = HashMap.empty
      for (b <- code.blocks.toList
        if b.successors.length == 1;
        val succ = b.successors.head;
        if succ ne b;
        if succ.predecessors.length == 1;
        if succ.predecessors.head eq b;
        if !(exh.exists { (e: ExceptionHandler) =>
            (e.covers(succ) && !e.covers(b)) || (e.covers(b) && !e.covers(succ)) })) {
          nextBlock(b) = succ
      }

      var bb = code.startBlock
      while (!nextBlock.isEmpty) {
        if (nextBlock.isDefinedAt(bb)) {
          bb.open
          var succ = bb
          do {
            succ = nextBlock(succ);
            bb.removeLastInstruction
            succ.toList foreach { i => bb.emit(i, i.pos) }
            code.removeBlock(succ)
            nextBlock -= bb
            exh foreach { e => e.covered = e.covered - succ }
          } while (nextBlock.isDefinedAt(succ))
          bb.close
        } else
          bb = nextBlock.keysIterator.next
      }
    }

    def dump {
      val printer = new TextPrinter(new PrintWriter(Console.out, true),
                                    new DumpLinearizer)
      printer.printMethod(this)
    }
  }

  /** Represent local variables and parameters */
  class Local(val sym: Symbol, val kind: TypeKind, val arg: Boolean) {
    var index: Int = -1

    /** Starting PC for this local's visbility range. */
    var start: Int = _

    /** Ending PC for this local's visbility range. */
    var end: Int = _

    /** PC-based ranges for this local variable's visibility */
    var ranges: List[(Int, Int)] = Nil

    override def equals(other: Any): Boolean = (
      other.isInstanceOf[Local] &&
      other.asInstanceOf[Local].sym == this.sym
    );

    override def hashCode = sym.hashCode

    override def toString(): String = sym.toString()
  }
}
