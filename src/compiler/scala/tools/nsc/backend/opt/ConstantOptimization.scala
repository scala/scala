/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  James Iry
 */

package scala
package tools.nsc
package backend.opt

import scala.annotation.tailrec

/**
 * ConstantOptimization uses abstract interpretation to approximate for
 * each instruction what constants a variable or stack slot might hold
 * or cannot hold. From this it will eliminate unreachable conditionals
 * where only one branch is reachable, e.g. to eliminate unnecessary
 * null checks.
 *
 * With some more work it could be extended to
 * - cache stable values (final fields, modules) in locals
 * - replace the copy propagation in ClosureElimination
 * - fold constants
 * - eliminate unnecessary stores and loads
 * - propagate knowledge gathered from conditionals for further optimization
 */
abstract class ConstantOptimization extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._

  val phaseName = "constopt"

  /** Create a new phase */
  override def newPhase(p: Phase) = new ConstantOptimizationPhase(p)

  override val enabled: Boolean = settings.YconstOptimization

  /**
   * The constant optimization phase.
   */
  class ConstantOptimizationPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName

    override def apply(c: IClass) {
      if (settings.YconstOptimization) {
        val analyzer = new ConstantOptimizer
        analyzer optimizeClass c
      }
    }
  }

  class ConstantOptimizer {
    def optimizeClass(cls: IClass) {
      log(s"Analyzing ${cls.methods.size} methods in $cls.")
      cls.methods foreach { m =>
        optimizeMethod(m)
      }
    }

    def optimizeMethod(m: IMethod) {
      if (m.hasCode) {
        log(s"Analyzing ${m.symbol}")
        val replacementInstructions = interpretMethod(m)
        for (block <- m.blocks) {
          if (replacementInstructions contains block) {
            val instructions = replacementInstructions(block)
            block.replaceInstruction(block.lastInstruction, instructions)
          }
        }
      }
    }

    /**
     * A single possible (or impossible) datum that can be held in Contents
     */
    private sealed abstract class Datum
    /**
     * A constant datum
     */
    private case class Const(c: Constant) extends Datum {
      def isIntAssignable = c.tag >= BooleanTag && c.tag <= IntTag
      def toInt = c.tag match {
        case BooleanTag => if (c.booleanValue) 1 else 0
        case _ => c.intValue
      }

      /**
       * True if this constant would compare to other as true under primitive eq
       */
      override def equals(other: Any) = other match {
        case oc @ Const(o) => (this eq oc) || (if (this.isIntAssignable && oc.isIntAssignable) this.toInt == oc.toInt else c.value == o.value)
        case _ => false
      }

      /**
       * Hash code consistent with equals
       */
      override def hashCode = if (this.isIntAssignable) this.toInt else c.hashCode

    }
    /**
     * A datum that has been Boxed via a BOX instruction
     */
    private case class Boxed(c: Datum) extends Datum

    /**
     * The knowledge we have about the abstract state of one location in terms
     * of what constants it might or cannot hold. Forms a lower
     * lattice where lower elements in the lattice indicate less knowledge.
     *
     * With the following partial ordering (where '>' indicates more precise knowledge)
     *
     * Possible(xs) > Possible(xs + y)
     * Possible(xs) > Impossible(ys)
     * Impossible(xs + y) > Impossible(xs)
     *
     * and the following merges, which indicate merging knowledge from two paths through
     * the code,
     *
     * // left must be 1 or 2, right must be 2 or 3 then we must have a 1, 2 or 3
     * Possible(xs) merge Possible(ys) => Possible(xs union ys)
     *
     * // Left says can't be 2 or 3, right says can't be 3 or 4
     * // then it's not 3 (it could be 2 from the right or 4 from the left)
     * Impossible(xs) merge Impossible(ys) => Impossible(xs intersect ys)
     *
     * // Left says it can't be 2 or 3, right says it must be 3 or 4, then
     * // it can't be 2 (left rules out 4 and right says 3 is possible)
     * Impossible(xs) merge Possible(ys) => Impossible(xs -- ys)
     *
     * Intuitively, Possible(empty) says that a location can't hold anything,
     * it's uninitialized. However, Possible(empty) never appears in the code.
     *
     * Conversely, Impossible(empty) says nothing is impossible, it could be
     * anything. Impossible(empty) is given a synonym UNKNOWN and is used
     * for, e.g., the result of an arbitrary method call.
     */
    private sealed abstract class Contents {
      /**
       * Join this Contents with another coming from another path. Join enforces
       * the lattice structure. It is symmetrical and never moves upward in the
       * lattice
       */
      final def merge(other: Contents): Contents = if (this eq other) this else (this, other) match {
        case (Possible(possible1), Possible(possible2)) =>
          Possible(possible1 union possible2)
        case (Impossible(impossible1), Impossible(impossible2)) =>
          Impossible(impossible1 intersect impossible2)
        case (Impossible(impossible), Possible(possible)) =>
          Impossible(impossible -- possible)
        case (Possible(possible), Impossible(impossible)) =>
          Impossible(impossible -- possible)
      }
      // TODO we could have more fine-grained knowledge, e.g. know that 0 < x < 3. But for now equality/inequality is a good start.
      def mightEqual(other: Contents): Boolean
      def mightNotEqual(other: Contents): Boolean
    }
    private def SingleImpossible(x: Datum) = new Impossible(Set(x))

    /**
     * The location is known to have one of a set of values.
     */
    private case class Possible(possible: Set[Datum]) extends Contents {
      assert(possible.nonEmpty, "Contradiction: had an empty possible set indicating an uninitialized location")
      def mightEqual(other: Contents): Boolean = (this eq other) || (other match {
        // two Possibles might be equal if they have any possible members in common
        case Possible(possible2) => (possible intersect possible2).nonEmpty
        // a possible can be equal to an impossible if the impossible doesn't rule
        // out all the possibilities
        case Impossible(possible2) => (possible -- possible2).nonEmpty
      })
      def mightNotEqual(other: Contents): Boolean = (other match {
        case Possible(possible2) =>
          // two Possibles must equal if each is known to be of the same, single value
          val mustEqual = possible.size == 1 && possible == possible2
          !mustEqual
        case Impossible(_) => true
      })
    }
    private def SinglePossible(x: Datum) = new Possible(Set(x))

    /**
     * The location is known to not have any of a set of values value (e.g null).
     */
    private case class Impossible(impossible: Set[Datum]) extends Contents {
      def mightEqual(other: Contents): Boolean = (this eq other) || (other match {
        case Possible(_) => other mightEqual this
        case _ => true
      })
      def mightNotEqual(other: Contents): Boolean = (this eq other) || (other match {
        case Possible(_) => other mightNotEqual this
        case _ => true
      })
    }

    /**
     * Our entire knowledge about the contents of all variables and the stack. It forms
     * a lattice primarily driven by the lattice structure of Contents.
     *
     * In addition to the rules of contents, State has the following properties:
     * - The merge of two sets of locals holds the merges of locals found in the intersection
     * of the two sets of locals. Locals not found in a
     * locals map are thus possibly uninitialized and attempting to load them results
     * in an error.
     * - The stack heights of two states must match otherwise it's an error to merge them
     *
     * State is immutable in order to aid in structure sharing of local maps and stacks
     */
    private case class State(locals: Map[Local, Contents], stack: List[Contents]) {
      def mergeLocals(olocals: Map[Local, Contents]): Map[Local, Contents] = if (locals eq olocals) locals else Map((for {
        key <- (locals.keySet intersect olocals.keySet).toSeq
      } yield (key, locals(key) merge olocals(key))): _*)

      def merge(other: State): State = if (this eq other) this else {
        @tailrec def mergeStacks(l: List[Contents], r: List[Contents], out: List[Contents]): List[Contents] = (l, r) match {
          case (Nil, Nil) => out.reverse
          case (l, r) if l eq r => out.reverse ++ l
          case (lhead :: ltail, rhead :: rtail) => mergeStacks(ltail, rtail, (lhead merge rhead) :: out)
          case _ => sys.error("Mismatched stack heights")
        }

        val newLocals = mergeLocals(other.locals)

        val newStack = if (stack eq other.stack) stack else mergeStacks(stack, other.stack, Nil)
        State(newLocals, newStack)
      }

      /**
       * Peek at the top of the stack without modifying it. Error if the stack is empty
       */
      def peek(n: Int): Contents = stack(n)
      /**
       * Push contents onto a stack
       */
      def push(contents: Contents): State = this copy (stack = contents :: stack)
      /**
       * Drop n elements from the stack
       */
      def drop(number: Int): State = this copy (stack = stack drop number)
      /**
       * Store the top of the stack into the specified local. An error if the stack
       * is empty
       */
      def store(variable: Local): State = {
        val contents = stack.head
        val newVariables = locals + ((variable, contents))
        new State(newVariables, stack.tail)
      }
      /**
       * Load the specified local onto the top of the stack. An error if the local is uninitialized.
       */
      def load(variable: Local): State = {
        val contents: Contents = locals.getOrElse(variable, sys.error(s"$variable is not initialized"))
        push(contents)
      }
      /**
       * A copy of this State with an empty stack
       */
      def cleanStack: State = if (stack.isEmpty) this else this copy (stack = Nil)
    }

    // some precomputed constants
    private val NULL = Const(Constant(null: Any))
    private val UNKNOWN = Impossible(Set.empty)
    private val NOT_NULL = SingleImpossible(NULL)
    private val CONST_UNIT = SinglePossible(Const(Constant(())))
    private val CONST_FALSE = SinglePossible(Const(Constant(false)))
    private val CONST_ZERO_BYTE = SinglePossible(Const(Constant(0: Byte)))
    private val CONST_ZERO_SHORT = SinglePossible(Const(Constant(0: Short)))
    private val CONST_ZERO_CHAR = SinglePossible(Const(Constant(0: Char)))
    private val CONST_ZERO_INT = SinglePossible(Const(Constant(0: Int)))
    private val CONST_ZERO_LONG = SinglePossible(Const(Constant(0: Long)))
    private val CONST_ZERO_FLOAT = SinglePossible(Const(Constant(0.0f)))
    private val CONST_ZERO_DOUBLE = SinglePossible(Const(Constant(0.0d)))
    private val CONST_NULL = SinglePossible(NULL)

    /**
     * Given a TypeKind, figure out what '0' for it means in order to interpret CZJUMP
     */
    private def getZeroOf(k: TypeKind): Contents = k match {
      case UNIT => CONST_UNIT
      case BOOL => CONST_FALSE
      case BYTE => CONST_ZERO_BYTE
      case SHORT => CONST_ZERO_SHORT
      case CHAR => CONST_ZERO_CHAR
      case INT => CONST_ZERO_INT
      case LONG => CONST_ZERO_LONG
      case FLOAT => CONST_ZERO_FLOAT
      case DOUBLE => CONST_ZERO_DOUBLE
      case REFERENCE(_) => CONST_NULL
      case ARRAY(_) => CONST_NULL
      case BOXED(_) => CONST_NULL
      case ConcatClass => abort("no zero of ConcatClass")
    }

    // normal locals can't be null, so we use null to mean the magic 'this' local
    private val THIS_LOCAL: Local = null

    /**
     * interpret a single instruction to find its impact on the abstract state
     */
    private def interpretInst(in: State, inst: Instruction): State = {
      // pop the consumed number of values off the `in` state's stack, producing a new state
      def dropConsumed: State = in drop inst.consumed

      inst match {
        case THIS(_) =>
          in load THIS_LOCAL

        case CONSTANT(k) =>
          // treat NaN as UNKNOWN because NaN must never equal NaN
          val const = if (k.isNaN) UNKNOWN
          else SinglePossible(Const(k))
          in push const

        case LOAD_ARRAY_ITEM(_) | LOAD_FIELD(_, _) | CALL_PRIMITIVE(_) =>
          dropConsumed push UNKNOWN

        case LOAD_LOCAL(local) =>
          // TODO if a local is known to hold a constant then we can replace this instruction with a push of that constant
          in load local

        case STORE_LOCAL(local) =>
          in store local

        case STORE_THIS(_) =>
          // if a local is already known to have a constant and we're replacing with the same constant then we can
          // replace this with a drop
          in store THIS_LOCAL

        case CALL_METHOD(_, _) =>
          // TODO we could special case implementations of equals that are known, e.g. String#equals
          // We could turn Possible(string constants).equals(Possible(string constants) into an eq check
          // We could turn nonConstantString.equals(constantString) into constantString.equals(nonConstantString)
          //  and eliminate the null check that likely precedes this call
          val initial = dropConsumed
          (0 until inst.produced).foldLeft(initial) { case (know, _) => know push UNKNOWN }

        case BOX(_) =>
          val value = in peek 0
          // we simulate boxing by, um, boxing the possible/impossible contents
          // so if we have Possible(1,2) originally then we'll end up with
          // a Possible(Boxed(1), Boxed(2))
          // Similarly, if we know the input is not a 0 then we'll know the
          // output is not a Boxed(0)
          val newValue = value match {
            case Possible(values) => Possible(values map Boxed)
            case Impossible(values) => Impossible(values map Boxed)
          }
          dropConsumed push newValue

        case UNBOX(_) =>
          val value = in peek 0
          val newValue = value match {
            // if we have a Possible, then all the possibilities
            // should themselves be Boxes. In that
            // case we can merge them to figure out what the UNBOX will produce
            case Possible(inners) =>
              assert(inners.nonEmpty, "Empty possible set indicating an uninitialized location")
              val sanitized: Set[Contents] = (inners map {
                case Boxed(content) => SinglePossible(content)
                case _ => UNKNOWN
              })
              sanitized reduce (_ merge _)
            // if we have an impossible then the thing that's impossible
            // should be a box. We'll unbox that to see what we get
            case unknown@Impossible(inners) =>
              if (inners.isEmpty) {
                unknown
              } else {
                val sanitized: Set[Contents] = (inners map {
                  case Boxed(content) => SingleImpossible(content)
                  case _ => UNKNOWN
                })
                sanitized reduce (_ merge _)
              }
          }
          dropConsumed push newValue

        case LOAD_MODULE(_) | NEW(_) | LOAD_EXCEPTION(_) =>
          in push NOT_NULL

        case CREATE_ARRAY(_, _) =>
          dropConsumed push NOT_NULL

        case IS_INSTANCE(_) =>
          // TODO IS_INSTANCE is going to be followed by a C(Z)JUMP
          // and if IS_INSTANCE/C(Z)JUMP the branch for "true" can
          // know that whatever was checked was not a null
          // see the TODO on CJUMP for more information about propagating null
          // information
          // TODO if the top of stack is guaranteed null then we can eliminate this IS_INSTANCE check and
          // replace with a constant false, but how often is a knowable null checked for instanceof?
          // TODO we could track type information and statically know to eliminate IS_INSTANCE
          // which might be a nice win under specialization
          dropConsumed push UNKNOWN // it's actually a Possible(true, false) but since the following instruction
        // will be a conditional jump comparing to true or false there
        // nothing to be gained by being more precise

        case CHECK_CAST(_) =>
          // TODO we could track type information and statically know to eliminate CHECK_CAST
          // but that's probably not a huge win
          in

        case DUP(_) =>
          val value = in peek 0
          in push value

        case DROP(_) | MONITOR_ENTER() | MONITOR_EXIT() | STORE_ARRAY_ITEM(_) | STORE_FIELD(_, _) =>
          dropConsumed

        case SCOPE_ENTER(_) | SCOPE_EXIT(_) =>
          in

        case JUMP(_) | CJUMP(_, _, _, _) | CZJUMP(_, _, _, _) | RETURN(_) | THROW(_) | SWITCH(_, _) =>
          dumpClassesAndAbort("Unexpected block ending instruction: " + inst)
      }
    }
    /**
     * interpret the last instruction of a block which will be jump, a conditional branch, a throw, or a return.
     * It will result in a map from target blocks to the input state computed for that block. It
     * also computes a replacement list of instructions
     */
    private def interpretLast(in: State, inst: Instruction): (Map[BasicBlock, State], List[Instruction]) = {
      def canSwitch(in1: Contents, tagSet: List[Int]) = {
        in1 mightEqual Possible(tagSet.toSet map { tag: Int => Const(Constant(tag)) })
      }

      /* common code for interpreting CJUMP and CZJUMP */
      def interpretConditional(kind: TypeKind, val1: Contents, val2: Contents, success: BasicBlock, failure: BasicBlock, cond: TestOp): (Map[BasicBlock, State], List[Instruction]) = {
        // TODO use reaching analysis to update the state in the two branches
        // e.g. if the comparison was checking null equality on local x
        // then the in the success branch we know x is null and
        // on the failure branch we know it is not
        // in fact, with copy propagation we could propagate that knowledge
        // back through a chain of locations
        //
        // TODO if we do all that we need to be careful in the
        // case that success and failure are the same target block
        // because we're using a Map and don't want one possible state to clobber the other
        // alternative maybe we should just replace the conditional with a jump if both targets are the same

        def mightEqual = val1 mightEqual val2
        def mightNotEqual = val1 mightNotEqual val2
        def guaranteedEqual = mightEqual && !mightNotEqual

        def succPossible = cond match {
          case EQ => mightEqual
          case NE => mightNotEqual
          case LT | GT => !guaranteedEqual // if the two are guaranteed to be equal then they can't be LT/GT
          case LE | GE => true
        }

        def failPossible = cond match {
          case EQ => mightNotEqual
          case NE => mightEqual
          case LT | GT => true
          case LE | GE => !guaranteedEqual // if the two are guaranteed to be equal then they must be LE/GE
        }

        val out = in drop inst.consumed

        var result = Map[BasicBlock, State]()
        if (succPossible) {
          result += ((success, out))
        }

        if (failPossible) {
          result += ((failure, out))
        }

        val replacements = if (result.size == 1) List.fill(inst.consumed)(DROP(kind)) :+ JUMP(result.keySet.head)
        else inst :: Nil

        (result, replacements)
      }

      inst match {
        case JUMP(whereto) =>
          (Map((whereto, in)), inst :: Nil)

        case CJUMP(success, failure, cond, kind) =>
          val in1 = in peek 0
          val in2 = in peek 1
          interpretConditional(kind, in1, in2, success, failure, cond)

        case CZJUMP(success, failure, cond, kind) =>
          val in1 = in peek 0
          val in2 = getZeroOf(kind)
          interpretConditional(kind, in1, in2, success, failure, cond)

        case SWITCH(tags, labels) =>
          val in1 = in peek 0
          val reachableNormalLabels = tags zip labels collect { case (tagSet, label) if canSwitch(in1, tagSet) => label }
          val reachableLabels = if (tags.isEmpty) {
            assert(labels.size == 1, s"When SWITCH node has empty array of tags it should have just one (default) label: $labels")
            labels
          } else if (labels.lengthCompare(tags.length) > 0) {
            // if we've got an extra label then it's the default
            val defaultLabel = labels.last
            // see if the default is reachable by seeing if the input might be out of the set
            // of all tags
            val allTags = Possible(tags.flatten.toSet map { tag: Int => Const(Constant(tag)) })
            if (in1 mightNotEqual allTags) {
              reachableNormalLabels :+ defaultLabel
            } else {
              reachableNormalLabels
            }
          } else {
            reachableNormalLabels
          }
          // TODO similar to the comment in interpretConditional, we should update our the State going into each
          // branch based on which tag is being matched. Also, just like interpretConditional, if target blocks
          // are the same we need to merge State rather than clobber

          // alternative, maybe we should simplify the SWITCH to not have same target labels
          val newState = in drop inst.consumed
          val result = Map(reachableLabels map { label => (label, newState) }: _*)
          if (reachableLabels.size == 1) (result, DROP(INT) :: JUMP(reachableLabels.head) :: Nil)
          else (result, inst :: Nil)

        // these instructions don't have target blocks
        // (exceptions are assumed to be reachable from all instructions)
        case RETURN(_) | THROW(_) =>
          (Map.empty, inst :: Nil)

        case _ =>
          dumpClassesAndAbort("Unexpected non-block ending instruction: " + inst)
      }
    }

    /**
     * Analyze a single block to find how it transforms an input state into a states for its successor blocks
     * Also computes a list of instructions to be used to replace its last instruction
     */
    private def interpretBlock(in: State, block: BasicBlock): (Map[BasicBlock, State], Map[BasicBlock, State], List[Instruction]) = {
      debuglog(s"interpreting block $block")
      // number of instructions excluding the last one
      val normalCount = block.size - 1

      var exceptionState = in.cleanStack
      var normalExitState = in
      var idx = 0
      while (idx < normalCount) {
        val inst = block(idx)
        normalExitState = interpretInst(normalExitState, inst)
        if (normalExitState.locals ne exceptionState.locals)
          exceptionState = exceptionState.copy(locals = exceptionState mergeLocals normalExitState.locals)
        idx += 1
      }

      val pairs = block.exceptionSuccessors map { b => (b, exceptionState) }
      val exceptionMap = Map(pairs: _*)

      val (normalExitMap, newInstructions) = interpretLast(normalExitState, block.lastInstruction)

      (normalExitMap, exceptionMap, newInstructions)
    }

    /**
     * Analyze a single method to find replacement instructions
     */
    private def interpretMethod(m: IMethod): Map[BasicBlock, List[Instruction]] = {
      import scala.collection.mutable.{ Set => MSet, Map => MMap }

      debuglog(s"interpreting method $m")
      var iterations = 0

      // initially we know that 'this' is not null and the params are initialized to some unknown value
      val initThis: Iterator[(Local, Contents)] = if (m.isStatic) Iterator.empty else Iterator.single((THIS_LOCAL, NOT_NULL))
      val initOtherLocals: Iterator[(Local, Contents)] = m.params.iterator map { param => (param, UNKNOWN) }
      val initialLocals: Map[Local, Contents] = Map((initThis ++ initOtherLocals).toSeq: _*)
      val initialState = State(initialLocals, Nil)

      // worklist of basic blocks to process, initially the start block
      val worklist = MSet(m.startBlock)
      // worklist of exception basic blocks. They're kept in a separate set so they can be
      // processed after normal flow basic blocks. That's because exception basic blocks
      // are more likely to have multiple predecessors and queueing them for later
      // increases the chances that they'll only need to be interpreted once
      val exceptionlist = MSet[BasicBlock]()
      // our current best guess at what the input state is for each block
      // initially we only know about the start block
      val inputState = MMap[BasicBlock, State]((m.startBlock, initialState))

      // update the inputState map based on new information from interpreting a block
      // When the input state of a block changes, add it back to the work list to be
      // reinterpreted
      def updateInputStates(outputStates: Map[BasicBlock, State], worklist: MSet[BasicBlock]) {
        for ((block, newState) <- outputStates) {
          val oldState = inputState get block
          val updatedState = oldState map (x => x merge newState) getOrElse newState
          if (oldState != Some(updatedState)) {
            worklist add block
            inputState(block) = updatedState
          }
        }
      }

      // the instructions to be used as the last instructions on each block
      val replacements = MMap[BasicBlock, List[Instruction]]()

      while (worklist.nonEmpty || exceptionlist.nonEmpty) {
        if (worklist.isEmpty) {
          // once the worklist is empty, start processing exception blocks
          val block = exceptionlist.head
          exceptionlist remove block
          worklist add block
        } else {
          iterations += 1
          val block = worklist.head
          worklist remove block
          val (normalExitMap, exceptionMap, newInstructions) = interpretBlock(inputState(block), block)

          updateInputStates(normalExitMap, worklist)
          updateInputStates(exceptionMap, exceptionlist)
          replacements(block) = newInstructions
        }
      }

      debuglog(s"method $m with ${m.blocks.size} reached fixpoint in $iterations iterations")
      replacements.toMap
    }
  }
}
