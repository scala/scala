/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package backend.icode.analysis

import scala.collection.{mutable, immutable}
import java.util.concurrent.TimeUnit

/** A data-flow analysis on types, that works on `ICode`.
 *
 *  @author Iulian Dragos
 */
abstract class TypeFlowAnalysis {
  val global: Global
  import global._
  import definitions.{ ObjectClass, NothingClass, AnyRefClass, StringClass, ThrowableClass }

  /** The lattice of ICode types.
   */
  object typeLattice extends SemiLattice {
    type Elem = icodes.TypeKind

    val top    = icodes.REFERENCE(ObjectClass)
    val bottom = icodes.REFERENCE(NothingClass)

    def lub2(exceptional: Boolean)(a: Elem, b: Elem) =
      if (a eq bottom) b
      else if (b eq bottom) a
      else icodes.lub(a, b)
  }

  /** The lattice of type stacks. It is a straight forward extension of
   *  the type lattice (lub is pairwise lub of the list elements).
   */
  object typeStackLattice extends SemiLattice {
    import icodes._
    type Elem = TypeStack

    val top                   = new TypeStack
    val bottom                = new TypeStack
    val exceptionHandlerStack = new TypeStack(List(REFERENCE(AnyRefClass)))

    def lub2(exceptional: Boolean)(s1: TypeStack, s2: TypeStack) = {
      if (s1 eq bottom) s2
      else if (s2 eq bottom) s1
      else if ((s1 eq exceptionHandlerStack) || (s2 eq exceptionHandlerStack)) sys.error("merging with exhan stack")
      else {
//        if (s1.length != s2.length)
//          throw new CheckerException("Incompatible stacks: " + s1 + " and " + s2);
        new TypeStack((s1.types, s2.types).zipped map icodes.lub)
      }
    }
  }

  /** A map which returns the bottom type for unfound elements */
  class VarBinding extends mutable.HashMap[icodes.Local, icodes.TypeKind] {
    override def default(l: icodes.Local) = typeLattice.bottom

    def this(o: VarBinding) = {
      this()
      this ++= o
    }
  }

  /** The type flow lattice contains a binding from local variable
   *  names to types and a type stack.
   */
  object typeFlowLattice extends SemiLattice {
    type Elem = IState[VarBinding, icodes.TypeStack]

    val top    = new Elem(new VarBinding, typeStackLattice.top)
    val bottom = new Elem(new VarBinding, typeStackLattice.bottom)

    def lub2(exceptional: Boolean)(a: Elem, b: Elem) = {
      val IState(env1, _) = a
      val IState(env2, _) = b

      val resultingLocals = new VarBinding
      env1 foreach { case (k, v) =>
        resultingLocals += ((k, typeLattice.lub2(exceptional)(v, env2(k))))
      }
      env2 collect { case (k, v) if resultingLocals(k) eq typeLattice.bottom =>
        resultingLocals += ((k, typeLattice.lub2(exceptional)(v, env1(k))))
      }
      val stack =
        if (exceptional) typeStackLattice.exceptionHandlerStack
        else typeStackLattice.lub2(exceptional)(a.stack, b.stack)

      IState(resultingLocals, stack)
    }
  }

  val timer = new Timer

  class MethodTFA extends DataFlowAnalysis[typeFlowLattice.type] {
    import icodes._
    import icodes.opcodes._

    type P = BasicBlock
    val lattice = typeFlowLattice

    val STRING = icodes.REFERENCE(StringClass)
    var method: IMethod = _

    /** Initialize the in/out maps for the analysis of the given method. */
    def init(m: icodes.IMethod) {
      this.method = m
      //typeFlowLattice.lubs = 0
      init {
        worklist += m.startBlock
        worklist ++= (m.exh map (_.startBlock))
        m foreachBlock { b =>
          in(b)  = typeFlowLattice.bottom
          out(b) = typeFlowLattice.bottom
        }

        // start block has var bindings for each of its parameters
        val entryBindings     = new VarBinding ++= (m.params map (p => ((p, p.kind))))
        in(m.startBlock) = lattice.IState(entryBindings, typeStackLattice.bottom)

        m.exh foreach { e =>
          in(e.startBlock) = lattice.IState(in(e.startBlock).vars, typeStackLattice.exceptionHandlerStack)
        }
      }
    }

    def this(m: icodes.IMethod) {
      this()
      init(m)
    }

    def run() = {
      timer.start()
      // icodes.lubs0 = 0
      forwardAnalysis(blockTransfer)
      timer.stop
      if (settings.debug) {
        linearizer.linearize(method).foreach(b => if (b != method.startBlock)
          assert(visited.contains(b),
            "Block " + b + " in " + this.method + " has input equal to bottom -- not visited? .." + visited))
      }
      // log("" + method.symbol.fullName + " ["  + method.code.blocks.size + " blocks] "
      //     + "\n\t" + iterations + " iterations: " + t + " ms."
      //     + "\n\tlubs: " + typeFlowLattice.lubs + " out of which " + icodes.lubs0 + " typer lubs")
    }

    def blockTransfer(b: BasicBlock, in: lattice.Elem): lattice.Elem = {
      var result = lattice.IState(new VarBinding(in.vars), new TypeStack(in.stack))
      var instrs = b.toList
      while(!instrs.isEmpty) {
        val i  = instrs.head
        result = mutatingInterpret(result, i)
        instrs = instrs.tail
      }
      result
    }

    /** Abstract interpretation for one instruction. */
    def interpret(in: typeFlowLattice.Elem, i: Instruction): typeFlowLattice.Elem = {
      val out = lattice.IState(new VarBinding(in.vars), new TypeStack(in.stack))
      mutatingInterpret(out, i)
    }

    def mutatingInterpret(out: typeFlowLattice.Elem, i: Instruction): typeFlowLattice.Elem = {
      val bindings = out.vars
      val stack = out.stack

      if (settings.debug) {
        // Console.println("[before] Stack: " + stack);
        // Console.println(i);
      }
      i match {

        case THIS(clasz)     => stack push toTypeKind(clasz.tpe)
        case CONSTANT(const) => stack push toTypeKind(const.tpe)

        case LOAD_ARRAY_ITEM(kind) =>
          stack.pop2 match {
            case (idxKind, ARRAY(elem)) =>
              assert(idxKind == INT || idxKind == CHAR || idxKind == SHORT || idxKind == BYTE)
              stack.push(elem)
            case (_, _) =>
              stack.push(kind)
          }

        case LOAD_LOCAL(local) =>
          val t = bindings(local)
          stack push (if (t == typeLattice.bottom) local.kind  else t)

        case LOAD_FIELD(field, isStatic) =>
          if (!isStatic) { stack.pop }
          stack push toTypeKind(field.tpe)

        case LOAD_MODULE(module)    => stack push toTypeKind(module.tpe)
        case STORE_ARRAY_ITEM(kind) => stack.pop3
        case STORE_LOCAL(local)     => val t = stack.pop; bindings += (local -> t)
        case STORE_THIS(_)          => stack.pop

        case STORE_FIELD(field, isStatic) => if (isStatic) stack.pop else stack.pop2

        case CALL_PRIMITIVE(primitive) =>
          primitive match {
            case Negation(kind) => stack.pop; stack.push(kind)

            case Test(_, kind, zero) =>
              stack.pop
              if (!zero) { stack.pop }
              stack push BOOL

            case Comparison(_, _) => stack.pop2; stack push INT

            case Arithmetic(op, kind) =>
              stack.pop
              if (op != NOT) { stack.pop }
              val k = kind match {
                case BYTE | SHORT | CHAR => INT
                case _ => kind
              }
              stack push k

            case Logical(op, kind)    => stack.pop2; stack push kind
            case Shift(op, kind)      => stack.pop2; stack push kind
            case Conversion(src, dst) => stack.pop;  stack push dst
            case ArrayLength(kind)    => stack.pop;  stack push INT
            case StartConcat          => stack.push(ConcatClass)
            case EndConcat            => stack.pop;  stack.push(STRING)
            case StringConcat(el)     => stack.pop2; stack push ConcatClass
          }

        case cm @ CALL_METHOD(_, _) =>
          stack pop cm.consumed
          cm.producedTypes foreach (stack push _)

        case BOX(kind)   => stack.pop; stack.push(BOXED(kind))
        case UNBOX(kind) => stack.pop; stack.push(kind)

        case NEW(kind) => stack.push(kind)

        case CREATE_ARRAY(elem, dims) => stack.pop(dims); stack.push(ARRAY(elem))

        case IS_INSTANCE(tpe) => stack.pop; stack.push(BOOL)
        case CHECK_CAST(tpe)  => stack.pop; stack.push(tpe)

        case _: SWITCH => stack.pop
        case _: JUMP   => ()
        case _: CJUMP  => stack.pop2
        case _: CZJUMP => stack.pop

        case RETURN(kind) => if (kind != UNIT) { stack.pop }
        case THROW(_)     => stack.pop

        case DROP(kind) => stack.pop
        case DUP(kind)  => stack.push(stack.head)

        case MONITOR_ENTER() | MONITOR_EXIT()  => stack.pop

        case SCOPE_ENTER(_)  | SCOPE_EXIT(_) => ()

        case LOAD_EXCEPTION(clasz) =>
          stack.pop(stack.length)
          stack.push(toTypeKind(clasz.tpe))

        case _ =>
          dumpClassesAndAbort("Unknown instruction: " + i)
      }
      out
    } // interpret

	abstract class InferredType {
      /** Return the type kind pointed by this inferred type. */
      def getKind(in: lattice.Elem): icodes.TypeKind = this match {
        case Const(k) =>
          k
        case TypeOfVar(l: icodes.Local) =>
          if (in.vars.isDefinedAt(l)) in.vars(l) else l.kind
        case TypeOfStackPos(n: Int) =>
          assert(in.stack.length >= n)
          in.stack(n)
      }
    }
	/** A type that does not depend on input to the transfer function. */
	case class Const(t: icodes.TypeKind) extends InferredType
	/** The type of a given local variable. */
	case class TypeOfVar(l: icodes.Local) extends InferredType
	/** The type found at a stack position. */
	case class TypeOfStackPos(n: Int) extends InferredType

	abstract class Gen
	case class Bind(l: icodes.Local, t: InferredType) extends Gen
	case class Push(t: InferredType) extends Gen

    /** A flow transfer function of a basic block. */
	class TransferFunction(consumed: Int, gens: List[Gen]) extends (lattice.Elem => lattice.Elem) {
	  def apply(in: lattice.Elem): lattice.Elem = {
        val out = lattice.IState(new VarBinding(in.vars), new TypeStack(in.stack))
        val stack = out.stack

        out.stack.pop(consumed)
        for (g <- gens) g match {
          case Bind(l, t) =>
            out.vars += (l -> t.getKind(in))
          case Push(t) =>
            stack.push(t.getKind(in))
        }
        out
      }
	}
  }

  case class CallsiteInfo(bb: icodes.BasicBlock, receiver: Symbol, stackLength: Int, concreteMethod: Symbol)

  /**

    A full type-flow analysis on a method computes in- and out-flows for each basic block (that's what MethodTFA does).

    For the purposes of Inliner, doing so guarantees that an abstract typestack-slot is available by the time an inlining candidate (a CALL_METHOD instruction) is visited.
    This subclass (MTFAGrowable) of MethodTFA also aims at performing such analysis on CALL_METHOD instructions, with some differences:

      (a) early screening is performed while the type-flow is being computed (in an override of `blockTransfer`) by testing a subset of the conditions that Inliner checks later.
          The reasoning here is: if the early check fails at some iteration, there's no chance a follow-up iteration (with a yet more lub-ed typestack-slot) will succeed.
          Failure is sufficient to remove that particular CALL_METHOD from the typeflow's `remainingCALLs`.
          A forward note: in case inlining occurs at some basic block B, all blocks reachable from B get their CALL_METHOD instructions considered again as candidates
          (because of the more precise types that -- perhaps -- can be computed).

      (b) in case the early check does not fail, no conclusive decision can be made, thus the CALL_METHOD stays `isOnwatchlist`.

    In other words, `remainingCALLs` tracks those callsites that still remain as candidates for inlining, so that Inliner can focus on those.
    `remainingCALLs` also caches info about the typestack just before the callsite, so as to spare computing them again at inlining time.

    Besides caching, a further optimization involves skipping those basic blocks whose in-flow and out-flow isn't needed anyway (as explained next).
    A basic block lacking a callsite in `remainingCALLs`, when visited by the standard algorithm, won't cause any inlining.
    But as we know from the way type-flows are computed, computing the in- and out-flow for a basic block relies in general on those of other basic blocks.
    In detail, we want to focus on that sub-graph of the CFG such that control flow may reach a remaining candidate callsite.
    Those basic blocks not in that subgraph can be skipped altogether. That's why:
       - `forwardAnalysis()` in `MTFAGrowable` now checks for inclusion of a basic block in `relevantBBs`
       - same check is performed before adding a block to the worklist, and as part of choosing successors.
    The bookkeeping supporting on-the-fly pruning of irrelevant blocks requires overriding most methods of the dataflow-analysis.

    The rest of the story takes place in Inliner, which does not visit all of the method's basic blocks but only on those represented in `remainingCALLs`.

    @author Miguel Garcia, http://lampwww.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/

   */
  class MTFAGrowable extends MethodTFA {

    import icodes._

    val remainingCALLs = mutable.Map.empty[opcodes.CALL_METHOD, CallsiteInfo]

    val preCandidates  = mutable.Set.empty[BasicBlock]

    var callerLin: Traversable[BasicBlock] = null

    override def run {

      timer.start()
      forwardAnalysis(blockTransfer)
      timer.stop

      /* Now that `forwardAnalysis(blockTransfer)` has finished, all inlining candidates can be found in `remainingCALLs`,
         whose keys are callsites and whose values are pieces of information about the typestack just before the callsite in question.
         In order to keep `analyzeMethod()` simple, we collect in `preCandidates` those basic blocks containing at least one candidate. */
      preCandidates.clear()
      for(rc <- remainingCALLs) {
        preCandidates += rc._2.bb
      }

      if (settings.debug) {
        for(b <- callerLin; if (b != method.startBlock) && preCandidates(b)) {
          assert(visited.contains(b),
                 "Block " + b + " in " + this.method + " has input equal to bottom -- not visited? .." + visited)
        }
      }

    }

    var shrinkedWatchlist = false

    /*
      This is the method where information cached elsewhere is put to use. References are given those other places that populate those caches.

      The goal is avoiding computing type-flows for blocks we don't need (ie blocks not tracked in `relevantBBs`). The method used to add to `relevantBBs` is `putOnRadar`.

      Moreover, it's often the case that the last CALL_METHOD of interest ("of interest" equates to "being tracked in `isOnWatchlist`) isn't the last instruction on the block.
      There are cases where the typeflows computed past this `lastInstruction` are needed, and cases when they aren't.
      The reasoning behind this decision is described in `populatePerimeter()`. All `blockTransfer()` needs to do (in order to know at which instruction it can stop)
      is querying `isOnPerimeter`.

      Upon visiting a CALL_METHOD that's an inlining candidate, the relevant pieces of information about the pre-instruction typestack are collected for future use.
      That is, unless the candidacy test fails. The reasoning here is: if such early check fails at some iteration, there's no chance a follow-up iteration
      (with a yet more lub-ed typestack-slot) will succeed. In case of failure we can safely remove the CALL_METHOD from both `isOnWatchlist` and `remainingCALLs`.

     */
    override def blockTransfer(b: BasicBlock, in: lattice.Elem): lattice.Elem = {
      var result = lattice.IState(new VarBinding(in.vars), new TypeStack(in.stack))

      val stopAt = if(isOnPerimeter(b)) lastInstruction(b) else null
      var isPastLast = false

      var instrs = b.toList
      while(!isPastLast && !instrs.isEmpty) {
        val i  = instrs.head

        if(isOnWatchlist(i)) {
          val cm = i.asInstanceOf[opcodes.CALL_METHOD]
          val msym = cm.method
          val paramsLength = msym.info.paramTypes.size
          val receiver = result.stack.types.drop(paramsLength).head match {
            case REFERENCE(s) => s
            case _            => NoSymbol // e.g. the scrutinee is BOX(s) or ARRAY
          }
          val concreteMethod = inliner.lookupImplFor(msym, receiver)
          val isCandidate = {
            ( inliner.isClosureClass(receiver) || concreteMethod.isEffectivelyFinalOrNotOverridden || receiver.isEffectivelyFinalOrNotOverridden ) &&
            !blackballed(concreteMethod)
          }
          if(isCandidate) {
            remainingCALLs(cm) = CallsiteInfo(b, receiver, result.stack.length, concreteMethod)
          } else {
            remainingCALLs.remove(cm)
            isOnWatchlist.remove(cm)
            shrinkedWatchlist = true
          }
        }

        isPastLast = (i eq stopAt)

        if(!isPastLast) {
          result = mutatingInterpret(result, i)
          instrs = instrs.tail
        }
      }

      result
    } // end of method blockTransfer

    val isOnWatchlist = mutable.Set.empty[Instruction]

    val warnIfInlineFails = mutable.Set.empty[opcodes.CALL_METHOD] // cache for a given IMethod (ie cleared on Inliner.analyzeMethod).

    /* Each time CallerCalleeInfo.isSafeToInline determines a concrete callee is unsafe to inline in the current caller,
       the fact is recorded in this TFA instance for the purpose of avoiding devoting processing to that callsite next time.
       The condition of "being unsafe to inline in the current caller" sticks across inlinings and TFA re-inits
       because it depends on the instructions of the callee, which stay unchanged during the course of `analyzeInc(caller)`
       (with the caveat of the side-effecting `makePublic` in `helperIsSafeToInline`).*/
    val knownUnsafe = mutable.Set.empty[Symbol]
    val knownSafe   = mutable.Set.empty[Symbol]
    val knownNever  = mutable.Set.empty[Symbol] // `knownNever` needs be cleared only at the very end of the inlining phase (unlike `knownUnsafe` and `knownSafe`)
    final def blackballed(msym: Symbol): Boolean = { knownUnsafe(msym) || knownNever(msym) }

    val relevantBBs   = mutable.Set.empty[BasicBlock]

    /*
     * Rationale to prevent some methods from ever being inlined:
     *
     *   (1) inlining getters and setters results in exposing a private field,
     *       which may itself prevent inlining of the caller (at best) or
     *       lead to situations like SI-5442 ("IllegalAccessError when mixing optimized and unoptimized bytecode")
     *
     *   (2) only invocations having a receiver object are considered (ie no static-methods are ever inlined).
     *       This is taken care of by checking `isDynamic` (ie virtual method dispatch) and `Static(true)` (ie calls to private members)
     */
    private def isPreCandidate(cm: opcodes.CALL_METHOD): Boolean = {
      val msym  = cm.method
      val style = cm.style

      !blackballed(msym)  &&
      !msym.isConstructor &&
      (!msym.isAccessor || inliner.isClosureClass(msym.owner)) &&
      (style.isDynamic  || (style.hasInstance && style.isStatic))
    }

    override def init(m: icodes.IMethod) {
      super.init(m)
      remainingCALLs.clear()
      knownUnsafe.clear()
      knownSafe.clear()
      // initially populate the watchlist with all callsites standing a chance of being inlined
      isOnWatchlist.clear()
      relevantBBs.clear()
      warnIfInlineFails.clear()
        /* TODO Do we want to perform inlining in non-finally exception handlers?
         * Seems counterproductive (the larger the method the less likely it will be JITed.
         * It's not that putting on radar only `linearizer linearizeAt (m, m.startBlock)` makes for much shorter inlining times (a minor speedup nonetheless)
         * but the effect on method size could be explored.  */
      putOnRadar(m.linearizedBlocks(linearizer))
      populatePerimeter()
      // usually but not always true (counterexample in SI-6015) `(relevantBBs.isEmpty || relevantBBs.contains(m.startBlock))`
    }

    def conclusives(b: BasicBlock): List[opcodes.CALL_METHOD] = {
      knownBeforehand(b) filter { cm => inliner.isMonadicMethod(cm.method) || inliner.hasInline(cm.method) }
    }

    def knownBeforehand(b: BasicBlock): List[opcodes.CALL_METHOD] = {
      b.toList collect { case c : opcodes.CALL_METHOD => c } filter { cm => isPreCandidate(cm) && isReceiverKnown(cm) }
    }

    private def isReceiverKnown(cm: opcodes.CALL_METHOD): Boolean = {
      cm.method.isEffectivelyFinalOrNotOverridden && cm.method.owner.isEffectivelyFinalOrNotOverridden
    }

    private def putOnRadar(blocks: Traversable[BasicBlock]) {
      for(bb <- blocks) {
        val calls = bb.toList collect { case cm : opcodes.CALL_METHOD => cm }
        for(c <- calls; if(inliner.hasInline(c.method))) {
           warnIfInlineFails += c
        }
        val preCands = calls filter isPreCandidate
        isOnWatchlist ++= preCands
      }
      relevantBBs ++= blocks
    }

    /* those BBs in the argument are also included in the result */
    private def transitivePreds(starters: Traversable[BasicBlock]): Set[BasicBlock] = {
      val result = mutable.Set.empty[BasicBlock]
      var toVisit: List[BasicBlock] = starters.toList.distinct
      while(toVisit.nonEmpty) {
        val h   = toVisit.head
        toVisit = toVisit.tail
        result += h
        for(p <- h.predecessors; if !result(p) && !toVisit.contains(p)) { toVisit = p :: toVisit }
      }
      result.toSet
    }

    /* A basic block B is "on the perimeter" of the current control-flow subgraph if none of its successors belongs to that subgraph.
     * In that case, for the purposes of inlining, we're interested in the typestack right before the last inline candidate in B, not in those afterwards.
     * In particular we can do without computing the outflow at B. */
    private def populatePerimeter() {
      isOnPerimeter.clear()
      var done = true
      do {
        val (frontier, toPrune) = (relevantBBs filter hasNoRelevantSuccs) partition isWatching
        isOnPerimeter ++= frontier
        relevantBBs   --= toPrune
        done = toPrune.isEmpty
      } while(!done)

      lastInstruction.clear()
      for (b <- isOnPerimeter; lastIns = b.toList.reverse find isOnWatchlist) {
        lastInstruction += (b -> lastIns.get.asInstanceOf[opcodes.CALL_METHOD])
      }

      // assertion: "no relevant block can have a predecessor that is on perimeter"
      assert((for (b <- relevantBBs; if transitivePreds(b.predecessors) exists isOnPerimeter) yield b).isEmpty)
    }

    private val isOnPerimeter   = mutable.Set.empty[BasicBlock]
    private val lastInstruction = mutable.Map.empty[BasicBlock, opcodes.CALL_METHOD]

    def hasNoRelevantSuccs(x: BasicBlock): Boolean = { !(x.successors exists relevantBBs) }

    def isWatching(x: BasicBlock): Boolean = (x.toList exists isOnWatchlist)




    /**

      This method is invoked after one or more inlinings have been performed in basic blocks whose in-flow is non-bottom (this makes a difference later).
      What we know about those inlinings is given by:

        - `staleOut`: These are the blocks where a callsite was inlined.
                      For each callsite, all instructions in that block before the callsite were left in the block, and the rest moved to an `afterBlock`.
                      The out-flow of these basic blocks is thus in general stale, that's why we'll add them to the TFA worklist.

        - `inlined` : These blocks were spliced into the method's CFG as part of inlining. Being new blocks, they haven't been visited yet by the typeflow analysis.

        - `staleIn` : These blocks are what `doInline()` calls `afterBlock`s, ie the new home for instructions that previously appeared
                      after a callsite in a `staleOut` block.

      Based on the above information, we have to bring up-to-date the caches that `forwardAnalysis` and `blockTransfer` use to skip blocks and instructions.
      Those caches are `relevantBBs` and `isOnPerimeter` (for blocks) and `isOnWatchlist` and `lastInstruction` (for CALL_METHODs).
      Please notice that all `inlined` and `staleIn` blocks are reachable from `staleOut` blocks.

      The update takes place in two steps:

        (1) `staleOut foreach { so => putOnRadar(linearizer linearizeAt (m, so)) }`
            This results in initial populations for `relevantBBs` and `isOnWatchlist`.
            Because of the way `isPreCandidate` reuses previous decision-outcomes that are still valid,
            this already prunes some candidates standing no chance of being inlined.

        (2) `populatePerimeter()`
            Based on the CFG-subgraph determined in (1) as reflected in `relevantBBs`,
            this method detects some blocks whose typeflows aren't needed past a certain CALL_METHOD
            (not needed because none of its successors is relevant for the purposes of inlining, see `hasNoRelevantSuccs`).
            The blocks thus chosen are said to be "on the perimeter" of the CFG-subgraph.
            For each of them, its `lastInstruction` (after which no more typeflows are needed) is found.

     */
    def reinit(m: icodes.IMethod, staleOut: List[BasicBlock], inlined: scala.collection.Set[BasicBlock], staleIn: scala.collection.Set[BasicBlock]) {
      if (this.method == null || this.method.symbol != m.symbol) {
        init(m)
        return
      } else if(staleOut.isEmpty && inlined.isEmpty && staleIn.isEmpty) {
        // this promotes invoking reinit if in doubt, no performance degradation will ensue!
        return
      }

      worklist.clear() // calling reinit(f: => Unit) would also clear visited, thus forgetting about blocks visited before reinit.

      // asserts conveying an idea what CFG shapes arrive here:
      //   staleIn foreach (p => assert( !in.isDefinedAt(p), p))
      //   staleIn foreach (p => assert(!out.isDefinedAt(p), p))
      //   inlined foreach (p => assert( !in.isDefinedAt(p), p))
      //   inlined foreach (p => assert(!out.isDefinedAt(p), p))
      //   inlined foreach (p => assert(!p.successors.isEmpty || p.lastInstruction.isInstanceOf[icodes.opcodes.THROW], p))
      //   staleOut foreach (p => assert(  in.isDefinedAt(p), p))

      // remainingCALLs.clear()
      isOnWatchlist.clear()
      relevantBBs.clear()

      // never rewrite in(m.startBlock)
      staleOut foreach { b =>
        enqueue(b)
        out(b)    = typeFlowLattice.bottom
      }
      // nothing else is added to the worklist, bb's reachable via succs will be tfa'ed
      blankOut(inlined)
      blankOut(staleIn)
      // no need to add startBlocks from m.exh

      staleOut foreach { so => putOnRadar(linearizer linearizeAt (m, so)) }
      populatePerimeter()

    } // end of method reinit

    /* this is not a general purpose method to add to the worklist,
     * because the assert is expected to hold only when called from MTFAGrowable.reinit() */
    private def enqueue(b: BasicBlock) {
      assert(in(b) ne typeFlowLattice.bottom)
      if(!worklist.contains(b)) { worklist += b }
    }

    private def blankOut(blocks: scala.collection.Set[BasicBlock]) {
      blocks foreach { b =>
        in(b)  = typeFlowLattice.bottom
        out(b) = typeFlowLattice.bottom
      }
    }

    /*
      This is basically the plain-old forward-analysis part of a dataflow algorithm,
      adapted to skip non-relevant blocks (as determined by `reinit()` via `populatePerimeter()`).

      The adaptations are:

        - only relevant blocks dequeued from the worklist move on to have the transfer function applied

        - `visited` now means the transfer function was applied to the block,
          but please notice that this does not imply anymore its out-flow to be different from bottom,
          because a block on the perimeter will have per-instruction typeflows computed only up to its `lastInstruction`.
          In case you need to know whether a visted block `v` has been "fully visited", evaluate `out(v) ne typeflowLattice.bottom`

        - given that the transfer function may remove callsite-candidates from the watchlist (thus, they are not candidates anymore)
          there's an opportunity to detect whether a previously relevant block has been left without candidates.
          That's what `shrinkedWatchlist` detects. Provided the block was on the perimeter, we know we can skip it from now now,
          and we can also constrain the CFG-subgraph by finding a new perimeter (thus the invocation to `populatePerimeter()`).
     */
    override def forwardAnalysis(f: (P, lattice.Elem) => lattice.Elem): Unit = {
      while (!worklist.isEmpty && relevantBBs.nonEmpty) {
        if (stat) iterations += 1
        val point = worklist.iterator.next(); worklist -= point
        if(relevantBBs(point)) {
          shrinkedWatchlist = false
          val output = f(point, in(point))
          visited += point
          if(isOnPerimeter(point)) {
            if(shrinkedWatchlist && !isWatching(point)) {
              relevantBBs -= point
              populatePerimeter()
            }
          } else {
            val propagate = ((lattice.bottom == out(point)) || output != out(point))
            if (propagate) {
              out(point) = output
              val succs = point.successors filter relevantBBs
              succs foreach { p =>
                assert((p.predecessors filter isOnPerimeter).isEmpty)
                val existing = in(p)
                // TODO move the following assertion to typeFlowLattice.lub2 for wider applicability (ie MethodTFA in addition to MTFAGrowable).
                assert(existing == lattice.bottom ||
                       p.exceptionHandlerStart    ||
                       (output.stack.length == existing.stack.length),
                       "Trying to merge non-bottom type-stacks with different stack heights. For a possible cause see SI-6157.")
                val updated = lattice.lub(List(output, existing), p.exceptionHandlerStart)
                if(updated != in(p)) {
                  in(p) = updated
                  enqueue(p)
                }
              }
            }
          }
        }
      }
    }

  }

  class Timer {
    var millis = 0L

    private var lastStart = 0L

    def start() {
      lastStart = System.nanoTime()
    }

    /** Stop the timer and return the number of milliseconds since the last
     * call to start. The 'millis' field is increased by the elapsed time.
     */
    def stop: Long = {
      val elapsed = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - lastStart)
      millis += elapsed
      elapsed
    }
  }
}
