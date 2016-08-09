/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.{tailrec, switch}

import scala.tools.asm.Type
import scala.tools.asm.tree.analysis.Frame
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree._
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.analysis._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

/**
 * Optimizations within a single method. Certain optimizations enable others, for example removing
 * unreachable code can render a `try` block empty and enable removeEmptyExceptionHandlers. The
 * latter in turn enables more unreachable code to be eliminated (the `catch` block), so there is
 * a cyclic dependency. Optimizations that depend on each other are therefore executed in a loop
 * until reaching a fixpoint.
 *
 * The optimizations marked UPSTREAM enable optimizations that were already executed, so they cause
 * another iteration in the fixpoint loop.
 *
 * nullness optimizations: rewrite null-checking branches to GOTO if nullness is known
 *   + enables downstream
 *     - unreachable code (null / non-null branch becomes unreachable)
 *     - box-unbox elimination (may render an escaping consumer of a box unreachable)
 *     - stale stores (aload x is replaced by aconst_null if it's known null)
 *     - simplify jumps (replaces conditional jumps by goto, so may enable goto chains)
 *
 * unreachable code / DCE (removes instructions of basic blocks to which there is no branch)
 *   + enables downstream:
 *     - stale stores (loads may be eliminated, removing consumers of a store)
 *     - empty handlers (try blocks may become empty)
 *     - simplify jumps (goto l; [dead code]; l: ..) => remove goto
 *     - stale local variable descriptors
 *     - (not box-unbox, which is implemented using prod-cons, so it doesn't consider dead code)
 *
 *   note that eliminating empty handlers and stale local variable descriptors is required for
 *   correctness, see the comment in the body of `methodOptimizations`.
 *
 * box-unbox elimination (eliminates box-unbox pairs withing the same method)
 *   + enables UPSTREAM:
 *     - nullness optimizations (a box extraction operation (unknown nullness) may be rewritten to
 *       a read of a non-null local. example in doc comment of box-unbox implementation)
 *     - further box-unbox elimination (e.g. an Integer stored in a Tuple; eliminating the tuple may
 *       enable eliminating the Integer)
 *   + enables downstream:
 *     - copy propagation (new locals are introduced, may be aliases of existing)
 *     - stale stores (multi-value boxes where not all values are used)
 *     - redundant casts (`("a", "b")._1`: the generic `_1` method returns `Object`, a cast
 *       to String is added. The cast is redundant after eliminating the tuple.)
 *     - empty local variable descriptors (local variables that were holding the box may become unused)
 *
 * copy propagation (replaces LOAD n to the LOAD m for the smallest m that is an alias of n)
 *   + enables downstrem:
 *     - stale stores (a stored value may not be loaded anymore)
 *     - store-load pairs (a load n may now be right after a store n)
 *   + NOTE: copy propagation is only executed once, in the first fixpoint loop iteration. none of
 *     the other optimizations enables further copy prop. we still run it as part of the loop
 *     because it requires unreachable code to be eliminated.
 *
 * stale stores (replace STORE by POP)
 *   + enables downstream:
 *     - push-pop (the new pop may be the single consumer for an instruction)
 *
 * redundant casts: eliminates casts that are statically known to succeed (uses type propagation)
 *   + enables UPSTREAM:
 *     - box-unbox elimination (a removed checkcast may be a box consumer)
 *   + enables downstream:
 *     - push-pop for closure allocation elimination (every indyLambda is followed by a checkcast, see SI-9540)
 *
 * push-pop (when a POP is the only consumer of a value, remove the POP and its producer)
 *   + enables UPSTREAM:
 *     - stale stores (if a LOAD is removed, a corresponding STORE may become stale)
 *     - box-unbox elimination (push-pop may eliminate a closure allocation, rendering a captured
 *       box non-escaping)
 *   + enables downstream:
 *     - store-load pairs (a variable may become non-live)
 *     - stale handlers (push-pop removes code)
 *     - simplify jumps (push-pop removes code)
 *
 * store-load pairs (remove `STORE x; LOAD x` if x is otherwise not used in the method)
 *   + enables downstream:
 *     - empty handlers (code is removes, a try block may become empty
 *     - simplify jumps (code is removed, a goto may become redundant for example)
 *     - stale local variable descriptors
 *
 * empty handlers (removes exception handlers whose try block is empty)
 *   + enables UPSTREAM:
 *     - unreachable code (catch block becomes unreachable)
 *     - box-unbox (a box may be escape in an operation in a dead handler)
 *   + enables downstream:
 *     - simplify jumps
 *
 * simplify jumps (various, like `GOTO l; l: ...`, see doc comments of individual optimizations)
 *   + enables UPSTREAM
 *     - unreachable code (`GOTO a; a: GOTO b; b: ...`, the first jump is changed to `GOTO b`, the second becomes unreachable)
 *     - store-load pairs (a `GOTO l; l: ...` is removed between store and load)
 *     - push-pop (`IFNULL l; l: ...` is replaced by `POP`)
 *
 *
 * The following cleanup optimizations don't enable any upstream optimizations, so they can be
 * executed once at the end, when the above optimizations reach a fixpoint.
 *
 *
 * empty local variable descriptors (removes unused variables from the local variable table)
 *   + enables downstream:
 *     - stale labels (labels that the entry points to, if not otherwise referenced)
 *
 * empty line numbers (eliminates line number nodes that describe no executable instructions)
 *   + enables downstream:
 *     - stale labels (label of the line number node, if not otherwise referenced)
 *
 * stale labels (eliminate labels that are not referenced, merge sequences of label definitions)
 *
 *
 * Note on a method's maxLocals / maxStack: the backend only uses those values for running
 * Analyzers. The values can be conservative approximations: if an optimization removes code and
 * the maximal stack size is now smaller, the larger maxStack value will still work fine for
 * running an Analyzer (just that frames allocate more space than required). The correct max
 * values written to the bytecode are re-computed during classfile serialization.
 * To keep things simpler, we don't update the max values in every optimization:
 *   - we do it in `removeUnreachableCodeImpl`, because it's quite straightforward
 *   - maxLocals is updated in `compactLocalVariables`, which runs at the end of method optimizations
 *
 *
 * Note on updating the call graph: whenever an optimization eliminates a callsite or a closure
 * instantiation, we eliminate the corresponding entry from the call graph.
 */
class LocalOpt[BT <: BTypes](val btypes: BT) {
  import LocalOptImpls._
  import btypes._
  import coreBTypes._
  import backendUtils._

  val boxUnbox = new BoxUnbox(btypes)
  import boxUnbox._

  val copyProp = new CopyProp(btypes)
  import copyProp._

  /**
   * Remove unreachable code from a method.
   *
   * This implementation only removes instructions that are unreachable for an ASM analyzer /
   * interpreter. This ensures that future analyses will not produce `null` frames. The inliner
   * depends on this property.
   *
   * @return A set containing the eliminated instructions
   */
  def minimalRemoveUnreachableCode(method: MethodNode, ownerClassName: InternalName): Boolean = {
    // In principle, for the inliner, a single removeUnreachableCodeImpl would be enough. But that
    // would potentially leave behind stale handlers (empty try block) which is not legal in the
    // classfile. So we run both removeUnreachableCodeImpl and removeEmptyExceptionHandlers.
    if (method.instructions.size == 0) return false     // fast path for abstract methods
    if (unreachableCodeEliminated(method)) return false // we know there is no unreachable code
    if (!AsmAnalyzer.sizeOKForBasicValue(method)) return false // the method is too large for running an analyzer

    // For correctness, after removing unreachable code, we have to eliminate empty exception
    // handlers, see scaladoc of def methodOptimizations. Removing an live handler may render more
    // code unreachable and therefore requires running another round.
    def removalRound(): Boolean = {
      val (insnsRemoved, liveLabels) = removeUnreachableCodeImpl(method, ownerClassName)
      if (insnsRemoved) {
        val liveHandlerRemoved = removeEmptyExceptionHandlers(method).exists(h => liveLabels(h.start))
        if (liveHandlerRemoved) removalRound()
      }
      insnsRemoved
    }

    val changed = removalRound()
    if (changed) removeUnusedLocalVariableNodes(method)()
    unreachableCodeEliminated += method
    changed
  }

  /**
   * Remove unreachable instructions from all (non-abstract) methods and apply various other
   * cleanups to the bytecode.
   *
   * @param clazz The class whose methods are optimized
   * @return      `true` if unreachable code was eliminated in some method, `false` otherwise.
   */
  def methodOptimizations(clazz: ClassNode): Boolean = {
    !compilerSettings.optNone && clazz.methods.asScala.foldLeft(false) {
      case (changed, method) => methodOptimizations(method, clazz.name) || changed
    }
  }

  /**
   * Run method-level optimizations, see comment on class [[LocalOpt]].
   *
   * Returns `true` if the bytecode of `method` was changed.
   */
  def methodOptimizations(method: MethodNode, ownerClassName: InternalName): Boolean = {
    if (method.instructions.size == 0) return false // fast path for abstract methods

    // unreachable-code also removes unused local variable nodes and empty exception handlers.
    // This is required for correctness, for example:
    //
    //   def f = { return 0; try { 1 } catch { case _ => 2 } }
    //
    // The result after removeUnreachableCodeImpl:
    //
    //   TRYCATCHBLOCK L0 L1 L2 java/lang/Exception
    //   L4
    //     ICONST_0
    //     IRETURN
    //   L0
    //   L1
    //   L2
    //
    // If we don't eliminate the handler, the ClassWriter emits:
    //
    //   TRYCATCHBLOCK L0 L0 L0 java/lang/Exception
    //   L1
    //     ICONST_0
    //     IRETURN
    //   L0
    //
    // This triggers "ClassFormatError: Illegal exception table range in class file C". Similar
    // for local variables in dead blocks. Maybe that's a bug in the ASM framework.

    var currentTrace: String = null
    val methodPrefix = {val p = compilerSettings.YoptTrace.value; if (p == "_") "" else p }
    val doTrace = compilerSettings.YoptTrace.isSetByUser && s"$ownerClassName.${method.name}".startsWith(methodPrefix)
    def traceIfChanged(optName: String): Unit = if (doTrace) {
      val after = AsmUtils.textify(method)
      if (currentTrace != after) {
        println(s"after $optName")
        println(after)
      }
      currentTrace = after
    }

    /**
     * Runs the optimizations that depend on each other in a loop until reaching a fixpoint. See
     * comment in class [[LocalOpt]].
     *
     * Returns a pair of booleans (codeChanged, requireEliminateUnusedLocals).
     */
    def removalRound(
        requestNullness: Boolean,
        requestDCE: Boolean,
        requestBoxUnbox: Boolean,
        requestStaleStores: Boolean,
        requestPushPop: Boolean,
        requestStoreLoad: Boolean,
        firstIteration: Boolean,
        maxRecursion: Int = 10): (Boolean, Boolean) = {
      if (maxRecursion == 0) return (false, false)

      traceIfChanged("beforeMethodOpt")

      // NULLNESS OPTIMIZATIONS
      val runNullness = compilerSettings.optNullnessTracking && requestNullness
      val nullnessOptChanged = runNullness && nullnessOptimizations(method, ownerClassName)
      traceIfChanged("nullness")

      // UNREACHABLE CODE
      // Both AliasingAnalyzer (used in copyProp) and ProdConsAnalyzer (used in eliminateStaleStores,
      // boxUnboxElimination) require not having unreachable instructions (null frames).
      val runDCE = (compilerSettings.optUnreachableCode && (requestDCE || nullnessOptChanged)) ||
        compilerSettings.optBoxUnbox ||
        compilerSettings.optCopyPropagation
      val (codeRemoved, liveLabels) = if (runDCE) removeUnreachableCodeImpl(method, ownerClassName) else (false, Set.empty[LabelNode])
      traceIfChanged("dce")

      // BOX-UNBOX
      val runBoxUnbox = compilerSettings.optBoxUnbox && (requestBoxUnbox || nullnessOptChanged)
      val boxUnboxChanged = runBoxUnbox && boxUnboxElimination(method, ownerClassName)
      traceIfChanged("boxUnbox")

      // COPY PROPAGATION
      val runCopyProp = compilerSettings.optCopyPropagation && (firstIteration || boxUnboxChanged)
      val copyPropChanged = runCopyProp && copyPropagation(method, ownerClassName)
      traceIfChanged("copyProp")

      // STALE STORES
      val runStaleStores = compilerSettings.optCopyPropagation && (requestStaleStores || nullnessOptChanged || codeRemoved || boxUnboxChanged || copyPropChanged)
      val storesRemoved = runStaleStores && eliminateStaleStores(method, ownerClassName)
      traceIfChanged("staleStores")

      // REDUNDANT CASTS
      val runRedundantCasts = compilerSettings.optRedundantCasts && (firstIteration || boxUnboxChanged)
      val castRemoved = runRedundantCasts && eliminateRedundantCasts(method, ownerClassName)
      traceIfChanged("redundantCasts")

      // PUSH-POP
      val runPushPop = compilerSettings.optCopyPropagation && (requestPushPop || firstIteration || storesRemoved || castRemoved)
      val pushPopRemoved = runPushPop && eliminatePushPop(method, ownerClassName)
      traceIfChanged("pushPop")

      // STORE-LOAD PAIRS
      val runStoreLoad = compilerSettings.optCopyPropagation && (requestStoreLoad || boxUnboxChanged || copyPropChanged || pushPopRemoved)
      val storeLoadRemoved = runStoreLoad && eliminateStoreLoad(method)
      traceIfChanged("storeLoadPairs")

      // STALE HANDLERS
      val removedHandlers = if (runDCE) removeEmptyExceptionHandlers(method) else Set.empty[TryCatchBlockNode]
      val handlersRemoved = removedHandlers.nonEmpty
      val liveHandlerRemoved = removedHandlers.exists(h => liveLabels(h.start))
      traceIfChanged("staleHandlers")

      // SIMPLIFY JUMPS
      // almost all of the above optimizations enable simplifying more jumps, so we just run it in every iteration
      val runSimplifyJumps = compilerSettings.optSimplifyJumps
      val jumpsChanged = runSimplifyJumps && simplifyJumps(method)
      traceIfChanged("simplifyJumps")

      // See doc comment in the beginning of this file (optimizations marked UPSTREAM)
      val runNullnessAgain = boxUnboxChanged
      val runDCEAgain = liveHandlerRemoved || jumpsChanged
      val runBoxUnboxAgain = boxUnboxChanged || castRemoved || pushPopRemoved || liveHandlerRemoved
      val runStaleStoresAgain = pushPopRemoved
      val runPushPopAgain = jumpsChanged
      val runStoreLoadAgain = jumpsChanged
      val runAgain = runNullnessAgain || runDCEAgain || runBoxUnboxAgain || pushPopRemoved || runStaleStoresAgain || runPushPopAgain || runStoreLoadAgain

      val downstreamRequireEliminateUnusedLocals = runAgain && removalRound(
        requestNullness = runNullnessAgain,
        requestDCE = runDCEAgain,
        requestBoxUnbox = runBoxUnboxAgain,
        requestStaleStores = runStaleStoresAgain,
        requestPushPop = runPushPopAgain,
        requestStoreLoad = runStoreLoadAgain,
        firstIteration = false,
        maxRecursion = maxRecursion - 1)._2

      val requireEliminateUnusedLocals = downstreamRequireEliminateUnusedLocals ||
        nullnessOptChanged || // nullness opt may eliminate stores / loads, rendering a local unused
        codeRemoved ||        // see comment in method `methodOptimizations`
        boxUnboxChanged ||    // box-unbox renders locals (holding boxes) unused
        storesRemoved  ||
        storeLoadRemoved ||
        handlersRemoved

      val codeChanged = nullnessOptChanged || codeRemoved || boxUnboxChanged || castRemoved || copyPropChanged || storesRemoved || pushPopRemoved || storeLoadRemoved || handlersRemoved || jumpsChanged
      (codeChanged, requireEliminateUnusedLocals)
    }

    val (nullnessDceBoxesCastsCopypropPushpopOrJumpsChanged, requireEliminateUnusedLocals) = if (AsmAnalyzer.sizeOKForBasicValue(method)) {
      // we run DCE even if the method is already in the `unreachableCodeEliminated` map: the DCE
      // here is more thorough than `minimalRemoveUnreachableCode` that run before inlining.
      val r = removalRound(
        requestNullness = true,
        requestDCE = true,
        requestBoxUnbox = true,
        requestStaleStores = true,
        requestPushPop = true,
        requestStoreLoad = true,
        firstIteration = true)
      if (compilerSettings.optUnreachableCode) unreachableCodeEliminated += method
      r
    } else (false, false)

    // (*) Removing stale local variable descriptors is required for correctness, see comment in `methodOptimizations`
    val localsRemoved =
      if (compilerSettings.optCompactLocals) compactLocalVariables(method) // also removes unused
      else if (requireEliminateUnusedLocals) removeUnusedLocalVariableNodes(method)() // (*)
      else false
    traceIfChanged("localVariables")

    val lineNumbersRemoved = if (compilerSettings.optUnreachableCode) removeEmptyLineNumbers(method) else false
    traceIfChanged("lineNumbers")

    val labelsRemoved = if (compilerSettings.optUnreachableCode) removeEmptyLabelNodes(method) else false
    traceIfChanged("labels")

    // assert that local variable annotations are empty (we don't emit them) - otherwise we'd have
    // to eliminate those covering an empty range, similar to removeUnusedLocalVariableNodes.
    def nullOrEmpty[T](l: java.util.List[T]) = l == null || l.isEmpty
    assert(nullOrEmpty(method.visibleLocalVariableAnnotations), method.visibleLocalVariableAnnotations)
    assert(nullOrEmpty(method.invisibleLocalVariableAnnotations), method.invisibleLocalVariableAnnotations)

    nullnessDceBoxesCastsCopypropPushpopOrJumpsChanged || localsRemoved || lineNumbersRemoved || labelsRemoved
  }

  /**
   * Apply various optimizations based on nullness analysis information.
   *   - IFNULL / IFNONNULL are rewritten to GOTO if nullness is known
   *   - IF_ACMPEQ / IF_ACMPNE are rewritten to GOTO if the both references are known null, or if
   *     one is known null and the other known not-null
   *   - ALOAD is replaced by ACONST_NULL if the local is known to hold null
   *   - ASTORE of null is removed if the local is known to hold null
   *   - INSTANCEOF of null is replaced by `ICONST_0`
   *   - scala.runtime.BoxesRunTime.unboxToX(null) is rewritten to a zero-value load
   */
  def nullnessOptimizations(method: MethodNode, ownerClassName: InternalName): Boolean = {
    AsmAnalyzer.sizeOKForNullness(method) && {
      lazy val nullnessAnalyzer = new AsmAnalyzer(method, ownerClassName, new NullnessAnalyzer(btypes, method))

      // When running nullness optimizations the method may still have unreachable code. Analyzer
      // frames of unreachable instructions are `null`.
      def frameAt(insn: AbstractInsnNode): Option[Frame[NullnessValue]] = Option(nullnessAnalyzer.frameAt(insn))

      def nullness(insn: AbstractInsnNode, slot: Int): Option[NullnessValue] = {
        frameAt(insn).map(_.getValue(slot))
      }

      def isNull(insn: AbstractInsnNode, slot: Int) = nullness(insn, slot).contains(NullValue)

      // cannot change instructions while iterating, it gets the analysis out of synch (indexed by instructions)
      val toReplace = mutable.Map.empty[AbstractInsnNode, List[AbstractInsnNode]]

      val it = method.instructions.iterator()
      while (it.hasNext) it.next() match {
        case vi: VarInsnNode if isNull(vi, vi.`var`) =>
          if (vi.getOpcode == ALOAD)
            toReplace(vi) = List(new InsnNode(ACONST_NULL))
          else if (vi.getOpcode == ASTORE)
            for (frame <- frameAt(vi) if frame.peekStack(0) == NullValue)
              toReplace(vi) = List(getPop(1))

        case ji: JumpInsnNode =>
          val isIfNull = ji.getOpcode == IFNULL
          val isIfNonNull = ji.getOpcode == IFNONNULL
          if (isIfNull || isIfNonNull) for (frame <- frameAt(ji)) {
            val nullness = frame.peekStack(0)
            val taken = nullness == NullValue && isIfNull || nullness == NotNullValue && isIfNonNull
            val avoided = nullness == NotNullValue && isIfNull || nullness == NullValue && isIfNonNull
            if (taken || avoided) {
              val jump = if (taken) List(new JumpInsnNode(GOTO, ji.label)) else Nil
              toReplace(ji) = getPop(1) :: jump
            }
          } else {
            val isIfEq = ji.getOpcode == IF_ACMPEQ
            val isIfNe = ji.getOpcode == IF_ACMPNE
            if (isIfEq || isIfNe) for (frame <- frameAt(ji)) {
              val aNullness = frame.peekStack(1)
              val bNullness = frame.peekStack(0)
              val eq = aNullness == NullValue && bNullness == NullValue
              val ne = aNullness == NullValue && bNullness == NotNullValue || aNullness == NotNullValue && bNullness == NullValue
              val taken = isIfEq && eq || isIfNe && ne
              val avoided = isIfEq && ne || isIfNe && eq
              if (taken || avoided) {
                val jump = if (taken) List(new JumpInsnNode(GOTO, ji.label)) else Nil
                toReplace(ji) = getPop(1) :: getPop(1) :: jump
              }
            }
          }

        case ti: TypeInsnNode =>
          if (ti.getOpcode == INSTANCEOF) for (frame <- frameAt(ti) if frame.peekStack(0) == NullValue) {
            toReplace(ti) = List(getPop(1), new InsnNode(ICONST_0))
          }

        case mi: MethodInsnNode =>
          if (isScalaUnbox(mi)) for (frame <- frameAt(mi) if frame.peekStack(0) == NullValue) {
            toReplace(mi) = List(
              getPop(1),
              loadZeroForTypeSort(Type.getReturnType(mi.desc).getSort))
          }

        case _ =>
      }

      def removeFromCallGraph(insn: AbstractInsnNode): Unit = insn match {
        case mi: MethodInsnNode => callGraph.removeCallsite(mi, method)
        case _ =>
      }

      for ((oldOp, newOps) <- toReplace) {
        for (newOp <- newOps) method.instructions.insertBefore(oldOp, newOp)
        method.instructions.remove(oldOp)
        removeFromCallGraph(oldOp)
      }

      toReplace.nonEmpty
    }
  }

  /**
   * Removes unreachable basic blocks.
   *
   * @return A set containing eliminated instructions, and a set containing all live label nodes.
   */
  def removeUnreachableCodeImpl(method: MethodNode, ownerClassName: InternalName): (Boolean, Set[LabelNode]) = {
    val a = new AsmAnalyzer(method, ownerClassName)
    val frames = a.analyzer.getFrames

    var i = 0
    var liveLabels = Set.empty[LabelNode]
    var changed = false
    var maxLocals = parametersSize(method)
    var maxStack = 0
    val itr = method.instructions.iterator()
    while (itr.hasNext) {
      val insn = itr.next()
      val isLive = frames(i) != null
      if (isLive) maxStack = math.max(maxStack, frames(i).getStackSize)

      insn match {
        case l: LabelNode =>
          // label nodes are not removed: they might be referenced for example in a LocalVariableNode
          if (isLive) liveLabels += l

        case v: VarInsnNode if isLive =>
          val longSize = if (isSize2LoadOrStore(v.getOpcode)) 1 else 0
          maxLocals = math.max(maxLocals, v.`var` + longSize + 1) // + 1 because local numbers are 0-based

        case i: IincInsnNode if isLive =>
          maxLocals = math.max(maxLocals, i.`var` + 1)

        case _ =>
          if (!isLive || insn.getOpcode == NOP) {
            // Instruction iterators allow removing during iteration.
            // Removing is O(1): instructions are doubly linked list elements.
            itr.remove()
            changed = true
            insn match {
              case invocation: MethodInsnNode => callGraph.removeCallsite(invocation, method)
              case indy: InvokeDynamicInsnNode => callGraph.removeClosureInstantiation(indy, method)
              case _ =>
            }
          }
      }
      i += 1
    }
    method.maxLocals = maxLocals
    method.maxStack  = maxStack
    (changed, liveLabels)
  }

  /**
   * Eliminate `CHECKCAST` instructions that are statically known to succeed. This is safe if the
   * tested object is null: `null.asInstanceOf` always succeeds.
   *
   * The type of the tested object is determined using a NonLubbingTypeFlowAnalyzer. Note that this
   * analysis collapses LUBs of non-equal references types to Object for simplicity. Example:
   * given `B <: A <: Object`, the cast in `(if (..) new B else new A).asInstanceOf[A]` would not
   * be eliminated.
   *
   * Note: we cannot replace `INSTANCEOF` tests by only looking at the types, `null.isInstanceOf`
   * always returns false, so we'd also need nullness information.
   */
  def eliminateRedundantCasts(method: MethodNode, owner: InternalName): Boolean = {
    AsmAnalyzer.sizeOKForBasicValue(method) && {
      def isSubType(aRefDesc: String, bClass: InternalName): Boolean = aRefDesc == bClass || bClass == ObjectRef.internalName || {
        (bTypeForDescriptorOrInternalNameFromClassfile(aRefDesc) conformsTo classBTypeFromParsedClassfile(bClass)).getOrElse(false)
      }

      lazy val typeAnalyzer = new NonLubbingTypeFlowAnalyzer(method, owner)

      // cannot remove instructions while iterating, it gets the analysis out of synch (indexed by instructions)
      val toRemove = mutable.Set.empty[TypeInsnNode]

      val it = method.instructions.iterator()
      while (it.hasNext) it.next() match {
        case ti: TypeInsnNode if ti.getOpcode == CHECKCAST =>
          val frame = typeAnalyzer.frameAt(ti)
          val valueTp = frame.getValue(frame.stackTop)
          if (valueTp.isReference && isSubType(valueTp.getType.getDescriptor, ti.desc)) {
            toRemove += ti
          }

        case _ =>
      }

      toRemove foreach method.instructions.remove
      toRemove.nonEmpty
    }
  }
}

object LocalOptImpls {
  /**
   * Remove exception handlers that cover empty code blocks. A block is considered empty if it
   * consist only of labels, frames, line numbers, nops and gotos.
   *
   * There are no executable instructions that we can assume don't throw (eg ILOAD). The JVM spec
   * basically says that a VirtualMachineError may be thrown at any time:
   *   http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.3
   *
   * Note that no instructions are eliminated.
   *
   * @return the set of removed handlers
   */
  def removeEmptyExceptionHandlers(method: MethodNode): Set[TryCatchBlockNode] = {
    /** True if there exists code between start and end. */
    def containsExecutableCode(start: AbstractInsnNode, end: LabelNode): Boolean = {
      start != end && ((start.getOpcode: @switch) match {
        // FrameNode, LabelNode and LineNumberNode have opcode == -1.
        case -1 | GOTO => containsExecutableCode(start.getNext, end)
        case _ => true
      })
    }

    var removedHandlers = Set.empty[TryCatchBlockNode]
    val handlersIter = method.tryCatchBlocks.iterator()
    while (handlersIter.hasNext) {
      val handler = handlersIter.next()
      if (!containsExecutableCode(handler.start, handler.end)) {
        removedHandlers += handler
        handlersIter.remove()
      }
    }
    removedHandlers
  }

  /**
   * Remove all non-parameter entries from the local variable table which denote variables that are
   * not actually read or written.
   *
   * Note that each entry in the local variable table has a start, end and index. Two entries with
   * the same index, but distinct start / end ranges are different variables, they may have not the
   * same type or name.
   */
  def removeUnusedLocalVariableNodes(method: MethodNode)(firstLocalIndex: Int = parametersSize(method), renumber: Int => Int = identity): Boolean = {
    @tailrec def variableIsUsed(start: AbstractInsnNode, end: LabelNode, varIndex: Int): Boolean = {
      start != end && (start match {
        case v: VarInsnNode if v.`var` == varIndex => true
        case i: IincInsnNode if i.`var` == varIndex => true
        case _ => variableIsUsed(start.getNext, end, varIndex)
      })
    }

    val initialNumVars = method.localVariables.size
    val localsIter = method.localVariables.iterator()
    while (localsIter.hasNext) {
      val local = localsIter.next()
      val index = local.index
      // parameters and `this` (the lowest indices, starting at 0) are never removed or renumbered
      if (index >= firstLocalIndex) {
        if (!variableIsUsed(local.start, local.end, index)) localsIter.remove()
        else if (renumber(index) != index) local.index = renumber(index)
      }
    }
    method.localVariables.size != initialNumVars
  }

  /**
   * Compact the local variable slots used in the method's implementation. This prevents having
   * unused slots for example after eliminating unreachable code.
   *
   * This transformation reduces the size of the frame for invoking the method. For example, if the
   * method has an ISTORE instruction to the local variable 3, the maxLocals of the method is at
   * least 4, even if some local variable slots below 3 are not used by any instruction.
   *
   * This could be improved by doing proper register allocation.
   */
  def compactLocalVariables(method: MethodNode): Boolean = {
    // This array is built up to map local variable indices from old to new.
    val renumber = collection.mutable.ArrayBuffer.empty[Int]

    // Add the index of the local variable used by `varIns` to the `renumber` array.
    def addVar(varIns: AbstractInsnNode, slot: Int): Unit = {
      val index = slot
      val isWide = isSize2LoadOrStore(varIns.getOpcode)

      // Ensure the length of `renumber`. Unused variable indices are mapped to -1.
      val minLength = if (isWide) index + 2 else index + 1
      for (i <- renumber.length until minLength) renumber += -1

      renumber(index) = index
      if (isWide) renumber(index + 1) = index
    }

    // first phase: collect all used local variables. if the variable at index x is used, set
    // renumber(x) = x, otherwise renumber(x) = -1. if the variable is wide (long or double), set
    // renumber(x+1) = x.

    val firstLocalIndex = parametersSize(method)
    for (i <- 0 until firstLocalIndex) renumber += i // parameters and `this` are always used.
    method.instructions.iterator().asScala foreach {
      case VarInstruction(varIns, slot) => addVar(varIns, slot)
      case _ =>
    }

    // assign the next free slot to each used local variable.
    // for example, rewrite (0, 1, -1, 3, -1, 5) to (0, 1, -1, 2, -1, 3).

    var nextIndex = firstLocalIndex
    for (i <- firstLocalIndex until renumber.length if renumber(i) != -1) {
      renumber(i) = nextIndex
      nextIndex += 1
    }

    // Update the local variable descriptors according to the renumber table, and eliminate stale entries
    val removedLocalVariableDescriptors = removeUnusedLocalVariableNodes(method)(firstLocalIndex, renumber)

    if (nextIndex == renumber.length) removedLocalVariableDescriptors
    else {
      // update variable instructions according to the renumber table
      method.maxLocals = nextIndex
      method.instructions.iterator().asScala.foreach {
        case VarInstruction(varIns, slot) =>
          val oldIndex = slot
          if (oldIndex >= firstLocalIndex && renumber(oldIndex) != oldIndex) varIns match {
            case vi: VarInsnNode => vi.`var` = renumber(slot)
            case ii: IincInsnNode => ii.`var` = renumber(slot)
          }
        case _ =>
      }
      true
    }
  }

  /**
   * Removes LineNumberNodes that don't describe any executable instructions.
   *
   * This method expects (and asserts) that the `start` label of each LineNumberNode is the
   * lexically preceding label declaration.
   */
  def removeEmptyLineNumbers(method: MethodNode): Boolean = {
    def isEmpty(node: AbstractInsnNode): Boolean = node.getNext match {
      case null => true
      case l: LineNumberNode => true
      case n if n.getOpcode >= 0 => false
      case n => isEmpty(n)
    }

    val initialSize = method.instructions.size
    val iterator = method.instructions.iterator()
    var previousLabel: LabelNode = null
    while (iterator.hasNext) {
      iterator.next match {
        case label: LabelNode => previousLabel = label
        case line: LineNumberNode if isEmpty(line) =>
          assert(line.start == previousLabel)
          iterator.remove()
        case _ =>
      }
    }
    method.instructions.size != initialSize
  }

  /**
   * Removes unreferenced label declarations, also squashes sequences of label definitions.
   *
   *      [ops];              Label(a); Label(b);  [ops];
   *   => subs([ops], b, a);  Label(a);            subs([ops], b, a);
   */
  def removeEmptyLabelNodes(method: MethodNode): Boolean = {
    val references = labelReferences(method)

    val initialSize = method.instructions.size
    val iterator = method.instructions.iterator()
    var prev: LabelNode = null
    while (iterator.hasNext) {
      iterator.next match {
        case label: LabelNode =>
          if (!references.contains(label)) iterator.remove()
          else if (prev != null) {
            references(label).foreach(substituteLabel(_, label, prev))
            iterator.remove()
          } else prev = label

        case instruction =>
          if (instruction.getOpcode >= 0) prev = null
      }
    }
    method.instructions.size != initialSize
  }

  /**
   * Apply various simplifications to branching instructions.
   */
  def simplifyJumps(method: MethodNode): Boolean = {
    var changed = false

    val allHandlers = method.tryCatchBlocks.asScala.toSet

    // A set of all exception handlers that guard the current instruction, required for simplifyGotoReturn
    var activeHandlers = Set.empty[TryCatchBlockNode]

    val jumpInsns = mutable.LinkedHashMap.empty[JumpInsnNode, Boolean]

    for (insn <- method.instructions.iterator().asScala) insn match {
      case l: LabelNode =>
        activeHandlers ++= allHandlers.filter(_.start == l)
        activeHandlers = activeHandlers.filter(_.end != l)

      case ji: JumpInsnNode =>
        jumpInsns(ji) = activeHandlers.nonEmpty

      case _ =>
    }

    var _jumpTargets: Set[AbstractInsnNode] = null
    def jumpTargets = {
      if (_jumpTargets == null) {
        _jumpTargets = jumpInsns.keysIterator.map(_.label).toSet
      }
      _jumpTargets
    }

    def removeJumpFromMap(jump: JumpInsnNode) = {
      jumpInsns.remove(jump)
      _jumpTargets = null
    }

    def replaceJumpByPop(jump: JumpInsnNode) = {
      removeJumpAndAdjustStack(method, jump)
      removeJumpFromMap(jump)
    }

    /**
     * Removes a conditional jump if it is followed by a GOTO to the same destination.
     *
     *      CondJump l;  [nops];  GOTO l;  [...]
     *      POP*;        [nops];  GOTO l;  [...]
     *
     * Introduces 1 or 2 POP instructions, depending on the number of values consumed by the CondJump.
     */
    def simplifyThenElseSameTarget(insn: AbstractInsnNode): Boolean = insn match {
      case ConditionalJump(jump) =>
        nextExecutableInstruction(insn) match {
          case Some(Goto(elseJump)) if sameTargetExecutableInstruction(jump, elseJump) =>
            replaceJumpByPop(jump)
            true

          case _ => false
        }

      case _ => false
    }

    /**
     * Replace jumps to a sequence of GOTO instructions by a jump to the final destination.
     *
     * {{{
     *      Jump l;  [any ops];  l: GOTO m;  [any ops];  m: GOTO n;  [any ops];   n: NotGOTO; [...]
     *   => Jump n;  [rest unchanged]
     * }}}
     *
     * If there's a loop of GOTOs, the initial jump is replaced by one of the labels in the loop.
     */
    def collapseJumpChains(insn: AbstractInsnNode): Boolean = insn match {
      case JumpNonJsr(jump) =>
        val target = finalJumpTarget(jump)
        if (jump.label == target) false else {
          jump.label = target
          _jumpTargets = null
          true
        }

      case _ => false
    }

    /**
     * Eliminates unnecessary jump instructions
     *
     * {{{
     *      Jump l;  [nops];  l: [...]
     *   => POP*;    [nops];  l: [...]
     * }}}
     *
     * Introduces 0, 1 or 2 POP instructions, depending on the number of values consumed by the Jump.
     */
    def removeJumpToSuccessor(insn: AbstractInsnNode): Boolean = insn match {
      case JumpNonJsr(jump) if nextExecutableInstruction(jump, alsoKeep = Set(jump.label)) contains jump.label =>
        replaceJumpByPop(jump)
        true

      case _ => false
    }

    /**
     * If the "else" part of a conditional branch is a simple GOTO, negates the conditional branch
     * and eliminates the GOTO.
     *
     * {{{
     *      CondJump l;         [nops, no jump targets];  GOTO m;  [nops];  l: [...]
     *   => NegatedCondJump m;  [nops, no jump targets];           [nops];  l: [...]
     * }}}
     *
     * Note that no jump targets are allowed in the first [nops] section. Otherwise, there could
     * be some other jump to the GOTO, and eliminating it would change behavior.
     */
    def simplifyBranchOverGoto(insn: AbstractInsnNode, inTryBlock: Boolean): Boolean = insn match {
      case ConditionalJump(jump) =>
        // don't skip over jump targets, see doc comment
        nextExecutableInstruction(jump, alsoKeep = jumpTargets) match {
          case Some(Goto(goto)) =>
            if (nextExecutableInstruction(goto, alsoKeep = Set(jump.label)) contains jump.label) {
              val newJump = new JumpInsnNode(negateJumpOpcode(jump.getOpcode), goto.label)
              method.instructions.set(jump, newJump)
              removeJumpFromMap(jump)
              jumpInsns(newJump) = inTryBlock
              replaceJumpByPop(goto)
              true
            } else false

          case _ => false
        }
      case _ => false
    }

    /**
     * Inlines xRETURN and ATHROW
     *
     * {{{
     *      GOTO l;            [any ops];  l: xRETURN/ATHROW
     *   => xRETURN/ATHROW;    [any ops];  l: xRETURN/ATHROW
     * }}}
     *
     * inlining is only done if the GOTO instruction is not part of a try block, otherwise the
     * rewrite might change the behavior. For xRETURN, the reason is that return instructions may throw
     * an IllegalMonitorStateException, as described here:
     *   http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.return
     */
    def simplifyGotoReturn(instruction: AbstractInsnNode, inTryBlock: Boolean): Boolean = !inTryBlock && (instruction match {
      case Goto(jump) =>
        nextExecutableInstruction(jump.label) match {
          case Some(target) =>
            if (isReturn(target) || target.getOpcode == ATHROW) {
              method.instructions.set(jump, target.clone(null))
              removeJumpFromMap(jump)
              true
            } else false

          case _ => false
        }
      case _ => false
    })

    def run(): Boolean = {
      var changed = false

      // `.toList` because we're modifying the map while iterating over it
      for ((jumpInsn, inTryBlock) <- jumpInsns.toList if jumpInsns.contains(jumpInsn) && isJumpNonJsr(jumpInsn)) {
        var jumpRemoved = simplifyThenElseSameTarget(jumpInsn)

        if (!jumpRemoved) {
          changed = collapseJumpChains(jumpInsn) || changed
          jumpRemoved = removeJumpToSuccessor(jumpInsn)

          if (!jumpRemoved) {
            changed = simplifyBranchOverGoto(jumpInsn, inTryBlock) || changed
            changed = simplifyGotoReturn(jumpInsn, inTryBlock) || changed
          }
        }

        changed ||= jumpRemoved
      }

      if (changed) run()
      changed
    }

    run()
  }
}
