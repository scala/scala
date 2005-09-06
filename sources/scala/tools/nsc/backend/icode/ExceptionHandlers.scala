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
trait ExceptionHandlers: ICodes {

  class ExceptionHandler(label: String) {
    val code: Code = new Code("exh" + label);
    var outer: ExceptionHandler = _;
    private var coveredBlocks: List[BasicBlock] = Nil;

    def setOuter(o: ExceptionHandler): ExceptionHandler = {
      outer = o;
      this
    }

    def addBlock(b: BasicBlock): ExceptionHandler = {
      coveredBlocks = b :: coveredBlocks;
      this
    }

    def covered: List[BasicBlock] = coveredBlocks;
  }

  object NoHandler extends ExceptionHandler("<nohandler>");
}
