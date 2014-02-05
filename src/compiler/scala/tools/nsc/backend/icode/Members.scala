/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package backend
package icode

import scala.collection.{ mutable, immutable }
import scala.reflect.internal.util.{ SourceFile, NoSourceFile }

trait ReferenceEquality {
  override def hashCode = System.identityHashCode(this)
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
}

trait Members {
  self: ICodes =>

  import global._

  object NoCode extends Code(null, TermName("NoCode")) {
    override def blocksList: List[BasicBlock] = Nil
  }

  /**
   * This class represents the intermediate code of a method or
   * other multi-block piece of code, like exception handlers.
   */
  class Code(method: IMethod, name: Name) {
    def this(method: IMethod) = this(method, method.symbol.name)
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
      @annotation.tailrec def loop(xs: List[BasicBlock]) {
        xs match {
          case Nil     =>
          case x :: xs => x.touched = true ; loop(xs)
        }
      }
      if (b) loop(blocks.toList)

      _touched = b
    }

    // Constructor code
    startBlock = newBlock()

    def removeBlock(b: BasicBlock) {
      if (settings.debug) {
        // only do this sanity check when debug is turned on because it's moderately expensive
        val referers = blocks filter (_.successors contains b)
        assert(referers.isEmpty, s"Trying to removing block $b (with preds ${b.predecessors.mkString}) but it is still refered to from block(s) ${referers.mkString}")
      }

      if (b == startBlock) {
        assert(b.successors.length == 1,
          s"Removing start block ${b} with ${b.successors.length} successors (${b.successors.mkString})."
        )
        startBlock = b.successors.head
      }

      blocks -= b
      assert(!blocks.contains(b))
      method.exh filter (_ covers b) foreach (_.covered -= b)
      touched = true
    }

    /** This methods returns a string representation of the ICode */
    override def toString = "ICode '" + name.decoded + "'"

    /* Compute a unique new label */
    def nextLabel: Int = {
      currentLabel += 1
      currentLabel
    }

    /* Create a new block and append it to the list
     */
    def newBlock(): BasicBlock = {
      touched = true
      val block = new BasicBlock(nextLabel, method)
      blocks += block
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

    override def equals(other: Any): Boolean =
      other match {
        case other: IMember => (this compare other) == 0
        case _ => false
      }

    override def hashCode = symbol.##
  }

  /** Represent a class in ICode */
  class IClass(val symbol: Symbol) extends IMember {
    var fields: List[IField] = Nil
    var methods: List[IMethod] = Nil
    var cunit: CompilationUnit = _

    def addField(f: IField): this.type = {
      fields = f :: fields
      this
    }

    def addMethod(m: IMethod): this.type = {
      methods = m :: methods
      this
    }

    def setCompilationUnit(unit: CompilationUnit): this.type = {
      this.cunit = unit
      this
    }

    override def toString() = symbol.fullName

    def lookupMethod(s: Symbol) = methods find (_.symbol == s)

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

    def newBlock() = code.newBlock()
    def startBlock = code.startBlock
    def lastBlock  = { assert(blocks.nonEmpty, symbol); blocks.last }
    def blocks = code.blocksList
    def linearizedBlocks(lin: Linearizer = self.linearizer): List[BasicBlock] = lin linearize this

    def foreachBlock[U](f: BasicBlock  => U): Unit = blocks foreach f

    var native = false

    /** The list of exception handlers, ordered from innermost to outermost. */
    var exh: List[ExceptionHandler] = Nil
    var sourceFile: SourceFile = NoSourceFile
    var returnType: TypeKind = _
    var recursive: Boolean = false
    var bytecodeHasEHs = false // set by ICodeReader only, used by Inliner to prevent inlining (SI-6188)
    var bytecodeHasInvokeDynamic = false // set by ICodeReader only, used by Inliner to prevent inlining until we have proper invoke dynamic support

    /** local variables and method parameters */
    var locals: List[Local] = Nil

    /** method parameters */
    var params: List[Local] = Nil

    def hasCode = code ne NoCode
    def setCode(code: Code): IMethod = {
      this.code = code
      this
    }

    final def updateRecursive(called: Symbol): Unit = {
      recursive ||= (called == symbol)
    }

    def addLocal(l: Local): Local = findOrElse(locals)(_ == l) { locals ::= l ; l }

    def addParam(p: Local): Unit =
      if (params contains p) ()
      else {
        params ::= p
        locals ::= p
      }

    def addLocals(ls: List[Local]) = ls foreach addLocal

    def lookupLocal(n: Name): Option[Local]     = locals find (_.sym.name == n)
    def lookupLocal(sym: Symbol): Option[Local] = locals find (_.sym == sym)

    def addHandler(e: ExceptionHandler) = exh ::= e

    /** Is this method deferred ('abstract' in Java sense)?
     */
    def isAbstractMethod = symbol.isDeferred || symbol.owner.isInterface || native

    def isStatic: Boolean = symbol.isStaticMember

    override def toString() = symbol.fullName

    import opcodes._

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
        succ = b.successors.head
        if succ ne b
        if succ.predecessors.length == 1
        if succ.predecessors.head eq b
        if !(exh.exists { (e: ExceptionHandler) =>
            (e.covers(succ) && !e.covers(b)) || (e.covers(b) && !e.covers(succ)) })) {
          nextBlock(b) = succ
      }

      var bb = code.startBlock
      while (!nextBlock.isEmpty) {
        if (nextBlock.isDefinedAt(bb)) {
          bb.open()
          var succ = bb
          do {
            succ = nextBlock(succ)
            val lastInstr = bb.lastInstruction
            /* Ticket SI-5672
             * Besides removing the control-flow instruction at the end of `bb` (usually a JUMP), we have to pop any values it pushes.
             * Examples:
             *   `SWITCH` consisting of just the default case, or
             *   `CJUMP(targetBlock, targetBlock, _, _)` ie where success and failure targets coincide (this one consumes two stack values).
             */
            val oldTKs = lastInstr.consumedTypes
            assert(lastInstr.consumed == oldTKs.size, "Someone forgot to override consumedTypes() in " +  lastInstr)

              bb.removeLastInstruction()
              for(tk <- oldTKs.reverse) { bb.emit(DROP(tk), lastInstr.pos) }
              succ.toList foreach { i => bb.emit(i, i.pos) }
              code.removeBlock(succ)
              exh foreach { e => e.covered = e.covered - succ }

            nextBlock -= bb
          } while (nextBlock.isDefinedAt(succ))
          bb.close()
        } else
          bb = nextBlock.keysIterator.next()
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

    override def equals(other: Any): Boolean = other match {
      case x: Local => sym == x.sym
      case _        => false
    }
    override def hashCode = sym.hashCode
    override def toString(): String = sym.toString
  }
}
