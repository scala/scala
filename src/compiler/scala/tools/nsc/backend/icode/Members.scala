/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

import java.io.PrintWriter
import scala.collection.{ mutable, immutable }
import util.{ SourceFile, NoSourceFile }
import symtab.Flags.{ DEFERRED }

trait ReferenceEquality {
  override def hashCode = System.identityHashCode(this)
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
}

trait Members {
  self: ICodes =>

  import global._

  object NoCode extends Code(null, "NoCode") {
    override def blocksList: List[BasicBlock] = Nil
  }

  /**
   * This class represents the intermediate code of a method or
   * other multi-block piece of code, like exception handlers.
   */
  class Code(method: IMethod, name: String) {
    def this(method: IMethod) = this(method, method.symbol.decodedName.toString.intern)
    /** The set of all blocks */
    val blocks = mutable.ListBuffer[BasicBlock]()

    /** The start block of the method */
    var startBlock: BasicBlock = NoBasicBlock

    private var currentLabel: Int = 0
    private var _touched = false

    def blocksList: List[BasicBlock] = blocks.toList
    def instructions                 = blocksList flatMap (_.iterator)
    def blockCount                   = blocks.size
    def instructionCount             = (blocks map (_.length)).sum

    def touched = _touched
    def touched_=(b: Boolean): Unit = {
      if (b)
        blocks foreach (_.touched = true)

      _touched = b
    }

    // Constructor code
    startBlock = newBlock

    def removeBlock(b: BasicBlock) {
      if (settings.debug.value) {
        assert(blocks forall (p => !(p.successors contains b)),
          "Removing block that is still referenced in method code " + b + "preds: " + b.predecessors
        )
        assert(b != startBlock || b.successors.length == 1,
          "Removing start block with more than one successor."
        )
      }

      if (b == startBlock)
        startBlock = b.successors.head

      blocks -= b
      assert(!blocks.contains(b))
      method.exh filter (_ covers b) foreach (_.covered -= b)
      touched = true
    }

    /** This methods returns a string representation of the ICode */
    override def toString = "ICode '" + name + "'";

    /* Compute a unique new label */
    def nextLabel: Int = {
      currentLabel += 1
      currentLabel
    }

    /* Create a new block and append it to the list
     */
    def newBlock(): BasicBlock = {
      touched = true
      val block = new BasicBlock(nextLabel, method);
      blocks += block;
      block
    }
  }

  /** Common interface for IClass/IField/IMethod. */
  trait IMember extends Ordered[IMember] {
    def symbol: Symbol

    def compare(other: IMember) =
      if (symbol eq other.symbol) 0
      else if (symbol isLess other.symbol) -1
      else 1
  }

  /** Represent a class in ICode */
  class IClass(val symbol: Symbol) extends IMember {
    var fields: List[IField] = Nil
    var methods: List[IMethod] = Nil
    var cunit: CompilationUnit = _

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

    def lookupField(s: Symbol)  = fields find (_.symbol == s)
    def lookupMethod(s: Symbol) = methods find (_.symbol == s)
    def lookupMethod(s: Name)   = methods find (_.symbol.name == s)

    /* returns this methods static ctor if it has one. */
    def lookupStaticCtor: Option[IMethod] = methods find (_.symbol.isStaticConstructor)
  }

  /** Represent a field in ICode */
  class IField(val symbol: Symbol) extends IMember { }

  object NoIMethod extends IMethod(NoSymbol) { }

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
  class IMethod(val symbol: Symbol) extends IMember {
    var code: Code = NoCode

    def newBlock() = code.newBlock
    def startBlock = code.startBlock
    def lastBlock  = blocks.last
    def blocks = code.blocksList
    def linearizedBlocks(lin: Linearizer = self.linearizer): List[BasicBlock] = lin linearize this

    def foreachBlock[U](f: BasicBlock  => U): Unit = blocks foreach f
    def foreachInstr[U](f: Instruction => U): Unit = foreachBlock(_.toList foreach f)

    var native = false

    /** The list of exception handlers, ordered from innermost to outermost. */
    var exh: List[ExceptionHandler] = Nil
    var sourceFile: SourceFile = NoSourceFile
    var returnType: TypeKind = _
    var recursive: Boolean = false

    /** local variables and method parameters */
    var locals: List[Local] = Nil

    /** method parameters */
    var params: List[Local] = Nil

    def hasCode = code ne NoCode
    def setCode(code: Code): IMethod = {
      this.code = code;
      this
    }

    def addLocal(l: Local): Local = findOrElse(locals)(_ == l) { locals ::= l ; l }

    def addParam(p: Local): Unit =
      if (params contains p) ()
      else {
        params ::= p
        locals ::= p
      }

    def addLocals(ls: List[Local]) = ls foreach addLocal
    def addParams(as: List[Local]) = as foreach addParam

    def lookupLocal(n: Name): Option[Local]     = locals find (_.sym.name == n)
    def lookupLocal(sym: Symbol): Option[Local] = locals find (_.sym == sym)

    def addHandler(e: ExceptionHandler) = exh ::= e

    /** Is this method deferred ('abstract' in Java sense)?
     */
    def isAbstractMethod = symbol.isDeferred || symbol.owner.isInterface || native

    def isStatic: Boolean = symbol.isStaticMember

    override def toString() = symbol.fullName

    def matchesSignature(other: IMethod) = {
      (symbol.name == other.symbol.name) &&
      (params corresponds other.params)(_.kind == _.kind) &&
      (returnType == other.returnType)
    }

    import opcodes._
    def checkLocals(): Unit = {
      def localsSet = (code.blocks flatMap { bb =>
        bb.iterator collect {
          case LOAD_LOCAL(l)  => l
          case STORE_LOCAL(l) => l
        }
      }).toSet

      if (hasCode) {
        log("[checking locals of " + this + "]")
        locals filterNot localsSet foreach { l =>
          log("Local " + l + " is not declared in " + this)
        }
      }
    }

    /** Merge together blocks that have a single successor which has a
     * single predecessor. Exception handlers are taken into account (they
     * might force to break a block of straight line code like that).
     *
     * This method should be most effective after heavy inlining.
     */
    def normalize(): Unit = if (this.hasCode) {
      val nextBlock: mutable.Map[BasicBlock, BasicBlock] = mutable.HashMap.empty
      for (b <- code.blocks.toList
        if b.successors.length == 1;
        succ = b.successors.head;
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
            val lastInstr = bb.lastInstruction
            /* Ticket SI-5672
             * Besides removing the control-flow instruction at the end of `bb` (usually a JUMP), we have to pop any values it pushes.
             * Examples:
             *   `SWITCH` consisting of just the default case, or
             *   `CJUMP(targetBlock, targetBlock, _, _)` ie where success and failure targets coincide (this one consumes two stack values).
             */
            val oldTKs = lastInstr.consumedTypes
            assert(lastInstr.consumed == oldTKs.size, "Someone forgot to override consumedTypes() in " +  lastInstr)

              bb.removeLastInstruction
              for(tk <- oldTKs.reverse) { bb.emit(DROP(tk), lastInstr.pos) }
              succ.toList foreach { i => bb.emit(i, i.pos) }
              code.removeBlock(succ)
              exh foreach { e => e.covered = e.covered - succ }

            nextBlock -= bb
          } while (nextBlock.isDefinedAt(succ))
          bb.close
        } else
          bb = nextBlock.keysIterator.next
      }
      checkValid(this)
    }

    def dump() {
      Console.println("dumping IMethod(" + symbol + ")")
      newTextPrinter() printMethod this
    }
  }

  /** Represent local variables and parameters */
  class Local(val sym: Symbol, val kind: TypeKind, val arg: Boolean) {
    var index: Int = -1

    /** Starting PC for this local's visibility range. */
    var start: Int = _

    /** Ending PC for this local's visibility range. */
    var end: Int = _

    /** PC-based ranges for this local variable's visibility */
    var ranges: List[(Int, Int)] = Nil

    override def equals(other: Any): Boolean = other match {
      case x: Local => sym == x.sym
      case _        => false
    }
    override def hashCode = sym.hashCode
    override def toString(): String = sym.toString
  }
}
