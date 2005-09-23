/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import scala.tools.nsc.ast._;
import scala.collection.mutable.Queue;

trait Linearizers: ICodes {
  import opcodes._;

  trait Linearizer {
    def linearize(c: Code): List[BasicBlock];
  }

  /**
   * A simple linearizer which predicts all branches to
   * take the 'success' branch and tries to schedule those
   * blocks immediately after the test. This is in sync with
   * how 'while' statements are translated (if the test is
   * 'true', the loop continues).
   */
  class NormalLinearizer extends Linearizer with WorklistAlgorithm {
    type Elem = BasicBlock;
    val worklist: Queue[Elem] = new Queue();

    var blocks: List[BasicBlock] = Nil;

    def linearize(c: Code): List[BasicBlock] = {
      val b = c.startBlock;
      blocks = b :: Nil;

      run( { worklist.enqueue(b); } );
      blocks.reverse;
    }

    def processElement(b: BasicBlock) =
      b.lastInstruction match {
        case JUMP(where) =>
          add(where);
        case CJUMP(success, failure, _, _) =>
          add(success);
          add(failure);
        case CZJUMP(success, failure, _, _) =>
          add(success);
          add(failure);
        case SWITCH(_, labels) =>
          add(labels);
        case RETURN() =>
          ()
      }

    /**
     * Prepend b to the list, if not already scheduled.
     * TODO: use better test than linear search
     */
    def add(b: BasicBlock) =
      if (blocks.contains(b))
        ()
      else {
        blocks = b :: blocks;
        worklist enqueue b;
      }

    def add(bs: List[BasicBlock]): Unit = bs foreach add;
  }
}
