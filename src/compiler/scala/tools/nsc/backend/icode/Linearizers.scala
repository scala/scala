/* NSC -- new scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package icode

import scala.tools.nsc.ast._
import scala.collection.{ mutable, immutable }
import mutable.ListBuffer

trait Linearizers {
  self: ICodes =>

  import global.debuglog
  import opcodes._

  abstract class Linearizer {
    def linearize(c: IMethod): List[BasicBlock]
    def linearizeAt(c: IMethod, start: BasicBlock): List[BasicBlock]
  }

  /**
   * A simple linearizer which predicts all branches to
   * take the 'success' branch and tries to schedule those
   * blocks immediately after the test. This is in sync with
   * how 'while' statements are translated (if the test is
   * 'true', the loop continues).
   */
  class NormalLinearizer extends Linearizer with WorklistAlgorithm {
    type Elem = BasicBlock
    val worklist: WList = new mutable.Stack()
    var blocks: List[BasicBlock] = Nil

    def linearize(m: IMethod): List[BasicBlock] = {
      val b = m.startBlock;
      blocks = Nil;

      run {
        worklist pushAll (m.exh map (_.startBlock));
        worklist.push(b);
      }

      blocks.reverse;
    }

    def linearizeAt(m: IMethod, start: BasicBlock): List[BasicBlock] = {
      blocks = Nil
      worklist.clear()
      linearize(start)
    }

    /** Linearize another subtree and append it to the existing blocks. */
    def linearize(startBlock: BasicBlock): List[BasicBlock] = {
      //blocks = startBlock :: Nil;
      run( { worklist.push(startBlock); } );
      blocks.reverse;
    }

    def processElement(b: BasicBlock) =
      if (b.nonEmpty) {
        add(b);
        b.lastInstruction match {
          case JUMP(whereto) =>
            add(whereto);
          case CJUMP(success, failure, _, _) =>
            add(success);
            add(failure);
          case CZJUMP(success, failure, _, _) =>
            add(success);
            add(failure);
          case SWITCH(_, labels) =>
            add(labels);
          case RETURN(_) => ();
          case THROW(clasz) =>   ();
        }
      }

    def dequeue: Elem = worklist.pop;

    /**
     * Prepend b to the list, if not already scheduled.
     * TODO: use better test than linear search
     */
    def add(b: BasicBlock) {
      if (blocks.contains(b))
        ()
      else {
        blocks = b :: blocks;
        worklist push b;
      }
    }

    def add(bs: List[BasicBlock]): Unit = bs foreach add;
  }

  /**
   * Linearize code using a depth first traversal.
   */
  class DepthFirstLinerizer extends Linearizer {
    var blocks: List[BasicBlock] = Nil;

    def linearize(m: IMethod): List[BasicBlock] = {
      blocks = Nil;

      dfs(m.startBlock);
      m.exh foreach (b => dfs(b.startBlock));

      blocks.reverse
    }

    def linearizeAt(m: IMethod, start: BasicBlock): List[BasicBlock] = {
      blocks = Nil
      dfs(start)
      blocks.reverse
    }

    def dfs(b: BasicBlock): Unit =
      if (b.nonEmpty && add(b))
        b.successors foreach dfs;

    /**
     * Prepend b to the list, if not already scheduled.
     * TODO: use better test than linear search
     * @return Returns true if the block was added.
     */
    def add(b: BasicBlock): Boolean =
      !(blocks contains b) && {
        blocks = b :: blocks;
        true
      }
  }

  /**
   * Linearize code in reverse post order. In fact, it does
   * a post order traversal, prepending visited nodes to the list.
   * This way, it is constructed already in reverse post order.
   */
  class ReversePostOrderLinearizer extends Linearizer {
    var blocks: List[BasicBlock] = Nil
    val visited = new mutable.HashSet[BasicBlock]
    val added = new mutable.BitSet

    def linearize(m: IMethod): List[BasicBlock] = {
      blocks = Nil;
      visited.clear()
      added.clear;

      m.exh foreach (b => rpo(b.startBlock));
      rpo(m.startBlock);

      // if the start block has predecessors, it won't be the first one
      // in the linearization, so we need to enforce it here
      if (m.startBlock.predecessors eq Nil)
        blocks
      else
        m.startBlock :: (blocks.filterNot(_ == m.startBlock))
    }

    def linearizeAt(m: IMethod, start: BasicBlock): List[BasicBlock] = {
      blocks = Nil
      visited.clear()
      added.clear()

      rpo(start)
      blocks
    }

    def rpo(b: BasicBlock): Unit =
      if (b.nonEmpty && !visited(b)) {
        visited += b;
        b.successors foreach rpo
        add(b)
      }

    /**
     * Prepend b to the list, if not already scheduled.
     * @return Returns true if the block was added.
     */
    def add(b: BasicBlock) = {
      debuglog("Linearizer adding block " + b.label)

      if (!added(b.label)) {
        added += b.label
        blocks = b :: blocks;
      }
    }
  }

  /** A 'dump' of the blocks in this method, which does not
   *  require any well-formedness of the basic blocks (like
   *  the last instruction being a jump).
   */
  class DumpLinearizer extends Linearizer {
    def linearize(m: IMethod): List[BasicBlock] = m.blocks
    def linearizeAt(m: IMethod, start: BasicBlock): List[BasicBlock] = sys.error("not implemented")
  }

  /** The MSIL linearizer is used only for methods with at least one exception handler.
   *  It makes sure that all the blocks belonging to a `try`, `catch` or `finally` block
   *  are emitted in an order that allows the lexical nesting of try-catch-finally, just
   *  like in the source code.
   */
  class MSILLinearizer extends Linearizer {
    /** The MSIL linearizer first calls a NormalLInearizer. This is because the ILGenerator checks
     *  the stack size before emitting instructions. For instance, to emit a `store`, there needs
     *  to be some value on the stack. This can blow up in situations like this:
     *       ...
     *       jump 3
     *    4: store_local 0
     *       jump 5
     *    3: load_value
     *       jump 4
     *    5: ...
     *  here, 3 must be scheduled first.
     *
     *  The NormalLinearizer also removes dead blocks (blocks without predecessor). This is important
     *  in the following example:
     *     try { throw new Exception }
     *     catch { case e => throw e }
     *  which adds a dead block containing just a "throw" (which, again, would blow up code generation
     *  because of the stack size; there's no value on the stack when emitting that `throw`)
     */
    val normalLinearizer = new NormalLinearizer()

    def linearize(m: IMethod): List[BasicBlock] = {

      val handlersByCovered = m.exh.groupBy(_.covered)

      // number of basic blocks covered by the entire try-catch expression
      def size(covered: collection.immutable.Set[BasicBlock]) = {
        val hs = handlersByCovered(covered)
        covered.size + (hs :\ 0)((h, s) => h.blocks.length + s)
      }

      val tryBlocks = handlersByCovered.keys.toList sortBy size
      var result    = normalLinearizer.linearize(m)
      val frozen    = mutable.HashSet[BasicBlock](result.head)

      for (tryBlock <- tryBlocks) {
        result = groupBlocks(m, result, handlersByCovered(tryBlock), frozen)
      }
      result
    }

    /** @param handlers a list of handlers covering the same blocks (same try, multiple catches)
     *  @param frozen blocks can't be moved (fist block of a method, blocks directly following a try-catch)
     */
    def groupBlocks(method: IMethod, blocks: List[BasicBlock], handlers: List[ExceptionHandler], frozen: mutable.HashSet[BasicBlock]) = {
      assert(blocks.head == method.startBlock, method)

      // blocks before the try, and blocks for the try
      val beforeAndTry = new ListBuffer[BasicBlock]()
      // blocks for the handlers
      val catches = handlers map (_ => new ListBuffer[BasicBlock]())
      // blocks to be put at the end
      val after = new ListBuffer[BasicBlock]()

      var beforeTry = true
      val head = handlers.head

      for (b <- blocks) {
        if (head covers b) {
          beforeTry = false
          beforeAndTry += b
        } else {
          val handlerIndex = handlers.indexWhere(_.blocks.contains(b))
          if (handlerIndex >= 0) {
            catches(handlerIndex) += b
          } else if (beforeTry) {
            beforeAndTry += b
          } else {
            after += b
          }
        }
      }

      // reorder the blocks in "catches" so that the "firstBlock" is actually first
      (catches, handlers).zipped foreach { (lb, handler) =>
        lb -= handler.startBlock
        handler.startBlock +=: lb
      }

      // The first block emitted after a try-catch must be the one that the try / catch
      // blocks jump to (because in msil, these jumps cannot be emitted manually)
      var firstAfter: Option[BasicBlock] = None

      // Find the (hopefully) unique successor, look at the try and all catch blocks
      var blks = head.covered.toList :: handlers.map(_.blocks)
      while (firstAfter.isEmpty && !blks.isEmpty) {
        val b = blks.head
        blks = blks.tail

        val leaving = leavingBlocks(b)
        // no leaving blocks when the try or catch ends with THROW or RET
        if (!leaving.isEmpty) {
          assert(leaving.size <= 1, leaving)
          firstAfter = Some(leaving.head)
        }
      }
      if (firstAfter.isDefined) {
        val b = firstAfter.get
        if (frozen(b)) {
          assert(after contains b, b +", "+ method)
        } else {
          frozen += b
          if (beforeAndTry contains b) {
            beforeAndTry -= b
          } else {
            assert(after contains b, after)
            after -= b
          }
          b +=: after
        }
      }

      for (lb <- catches) { beforeAndTry ++= lb }
      beforeAndTry ++= after
      beforeAndTry.toList
    }

    /** Returns all direct successors of `blocks` wich are not part
     *  that list, i.e. successors outside the `blocks` list.
     */
    private def leavingBlocks(blocks: List[BasicBlock]) = {
      val res = new mutable.HashSet[BasicBlock]()
      for (b <- blocks; s <- b.directSuccessors; if (!blocks.contains(s)))
        res += s
      res
    }

    def linearizeAt(m: IMethod, start: BasicBlock): List[BasicBlock] = {
      sys.error("not implemented")
    }
  }
}
