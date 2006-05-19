/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import scala.collection.mutable.HashMap;
import scala.collection.mutable.HashSet;

/**
 * Exception handlers are pieces of code that `handle' exceptions on
 * the covered basic blocks. Since Scala's exception handling uses
 * pattern matching instead of just class names to identify handlers,
 * all our handlers will catch `Throwable' and rely on proper ordering
 * in the generated code to preserve nesting.
 */
trait ExceptionHandlers requires ICodes {
  import global.{Symbol, NoSymbol};

  class ExceptionHandler(val method: IMethod, val label: String, val cls: Symbol) {
    private var _startBlock: BasicBlock = _;
    var finalizer: Finalizer = _;

    def setStartBlock(b: BasicBlock) = _startBlock = b;
    def startBlock = _startBlock;

    def addBlock(b: BasicBlock): ExceptionHandler = {
      covered = b :: covered;
      this
    }

    var covered: List[BasicBlock] = Nil;

    def covers(b: BasicBlock): Boolean = covered.contains(b);

    override def toString() = "exh_" + label + "(" + cls.simpleName + ")";

    def this(other: ExceptionHandler) = {
      this(other.method, other.label, other.cls);
      covered    = other.covered;
      setStartBlock(other.startBlock);
      finalizer  = other.finalizer;
    }

    def dup: ExceptionHandler = new ExceptionHandler(this);
  }

  class Finalizer(method: IMethod, label: String) extends ExceptionHandler(method, label, NoSymbol) {
    override def toString() = "finalizer_" + label;

    override def dup: Finalizer = new Finalizer(method, label);
  }

  object NoFinalizer extends Finalizer(null, "<no finalizer>") {
    override def startBlock: BasicBlock = error("NoFinalizer cannot have a start block.");
    override def setStartBlock(b: BasicBlock): Unit = error("NoFinalizer cannot have a start block.");
  }
}
