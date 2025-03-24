/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.tools.asm.Opcodes._
import scala.tools.asm.Type
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis.Frame
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
 * box-unbox elimination (eliminates box-unbox pairs within the same method)
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
  *    - push-pop (due to artifacts of eliminating runtime type tests on primitives)
 *
 * copy propagation (replaces LOAD n to the LOAD m for the smallest m that is an alias of n)
 *   + enables downstream:
 *     - stale stores (a stored value may not be loaded anymore)
 *     - store-load pairs (a load n may now be right after a store n)
 *
 * stale stores (replace STORE by POP), rewrites `ClassTag(x).newArray`, inlines `array_apply/update`
 *   + enables UPSTREAM:
 *     - nullness optimizations (newArray rewrite or inlining may turn things non-null)
 *   + enables downstream:
 *     - push-pop (the new pop may be the single consumer for an instruction)
 *     - redundant casts (because rewrites `newArray`, the array type gets more precise)
 *
 * redundant casts and rewrite some intrinsics: eliminates casts that are statically known to
 * succeed (uses type propagation), rewrites instanceof checks, rewrites intrinsics.
 *   + enables UPSTREAM:
 *     - box-unbox elimination (a removed checkcast may be a box consumer)
 *     - copy propagation (a removed checkcast may turn an upcasted local variable into an alias)
 *   + enables downstream:
 *     - push-pop for closure allocation elimination (every indyLambda is followed by a checkcast,
 *       see scala/bug#9540)
 *     - redundant casts (changing an instanceof to true/false removes branches and can make types
 *       of other values more precise)
 *
 * push-pop (when a POP is the only consumer of a value, remove the POP and its producer)
 *   + enables UPSTREAM:
 *     - stale stores (if a LOAD is removed, a corresponding STORE may become stale)
 *     - box-unbox elimination (push-pop may eliminate a closure allocation, rendering a captured
 *       box non-escaping)
 *     - redundant casts (Int.unbox(x) is replaced by `x.asInstanceOf[Integer]; pop`)
 *     - nullness (`x.intValue` is replaced by `if (x == null) throw null`)
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
 *
 * empty line numbers (eliminates line number nodes that describe no executable instructions)
 *
 * At this point, we used to filter out redundant label nodes (sequences of labels without any
 * executable instructions in between). However, this operation is relatively expensive, and
 * unnecessary: labels don't exist in the classfile, they are lowered to bytecode offsets, so
 * redundant labels disappear by design.
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
abstract class LocalOpt {
  val postProcessor: PostProcessor

  import postProcessor._
  import bTypes._
  import bTypesFromClassfile._
  import backendUtils._
  import coreBTypes._
  import frontendAccess.compilerSettings

  import LocalOptImpls._

  val boxUnbox = new BoxUnbox { val postProcessor: LocalOpt.this.postProcessor.type = LocalOpt.this.postProcessor }
  import boxUnbox._

  val copyProp = new CopyProp { val postProcessor: LocalOpt.this.postProcessor.type = LocalOpt.this.postProcessor }
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
    if (method.instructions.size == 0) return false  // fast path for abstract methods
    if (BackendUtils.isDceDone(method)) return false // we know there is no unreachable code

    // For correctness, after removing unreachable code, we have to eliminate empty exception
    // handlers, see scaladoc of def methodOptimizations. Removing an live handler may render more
    // code unreachable and therefore requires running another round.
    def removalRound(): Boolean = {
      val insnsRemoved = removeUnreachableCodeImpl(method, ownerClassName)
      if (insnsRemoved) {
        val removeHandlersResult = removeEmptyExceptionHandlers(method)
        if (removeHandlersResult.liveHandlerRemoved) removalRound()
      }
      // Note that `removeUnreachableCodeImpl` adds `LABEL_REACHABLE_STATUS` to label.status fields. We don't clean up
      // this flag here (in `minimalRemoveUnreachableCode`), we rely on that being done later in `methodOptimizations`.
      insnsRemoved
    }

    val changed = removalRound()
    if (changed) removeUnusedLocalVariableNodes(method)()
    BackendUtils.setDceDone(method)
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
    val doTrace = compilerSettings.optTrace match {
      case Some(v) =>
        val prefix = if (v == "_") "" else v
        s"$ownerClassName.${method.name}".startsWith(prefix)

      case _ => false
    }
    def traceIfChanged(optName: String): Unit = if (doTrace) {
      val after = AsmUtils.textify(method)
      if (currentTrace != after) {
        println(s"after $optName")
        println(after)
      }
      currentTrace = after
    }

    /*
     * Runs the optimizations that depend on each other in a loop until reaching a fixpoint. See
     * comment in class [[LocalOpt]].
     *
     * Returns a pair of booleans (codeChanged, requireEliminateUnusedLocals).
     */
    def removalRound(
        requestNullness: Boolean,
        requestDCE: Boolean,
        requestBoxUnbox: Boolean,
        requestCopyProp: Boolean,
        requestStaleStores: Boolean,
        requestRedundantCasts: Boolean,
        requestPushPop: Boolean,
        requestStoreLoad: Boolean,
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
      val codeRemoved = if (runDCE) removeUnreachableCodeImpl(method, ownerClassName) else false
      traceIfChanged("dce")

      // BOX-UNBOX
      val runBoxUnbox = compilerSettings.optBoxUnbox && (requestBoxUnbox || nullnessOptChanged)
      val boxUnboxChanged = runBoxUnbox && boxUnboxElimination(method, ownerClassName)
      traceIfChanged("boxUnbox")

      // COPY PROPAGATION
      val runCopyProp = compilerSettings.optCopyPropagation && (requestCopyProp || boxUnboxChanged)
      val copyPropChanged = runCopyProp && copyPropagation(method, ownerClassName)
      traceIfChanged("copyProp")

      // STALE STORES
      val runStaleStores = compilerSettings.optCopyPropagation && (requestStaleStores || nullnessOptChanged || codeRemoved || boxUnboxChanged || copyPropChanged)
      val (storesRemoved, intrinsicRewrittenByStaleStores, callInlinedByStaleStores) = if (!runStaleStores) (false, false, false) else eliminateStaleStoresAndRewriteSomeIntrinsics(method, ownerClassName)
      traceIfChanged("staleStores")

      // REDUNDANT CASTS
      val runRedundantCasts = compilerSettings.optRedundantCasts && (requestRedundantCasts || boxUnboxChanged || intrinsicRewrittenByStaleStores || callInlinedByStaleStores)
      val (typeInsnChanged, intrinsicRewrittenByCasts) = if (!runRedundantCasts) (false, false) else eliminateRedundantCastsAndRewriteSomeIntrinsics(method, ownerClassName)
      traceIfChanged("redundantCasts")

      // PUSH-POP
      val runPushPop = compilerSettings.optCopyPropagation && (requestPushPop || storesRemoved || typeInsnChanged)
      val (pushPopRemoved, pushPopCastAdded, pushPopNullCheckAdded) = if (!runPushPop) (false, false, false) else eliminatePushPop(method, ownerClassName)
      traceIfChanged("pushPop")

      // STORE-LOAD PAIRS
      val runStoreLoad = compilerSettings.optCopyPropagation && (requestStoreLoad || boxUnboxChanged || copyPropChanged || pushPopRemoved)
      val storeLoadRemoved = runStoreLoad && eliminateStoreLoad(method)
      traceIfChanged("storeLoadPairs")

      // STALE HANDLERS
      val removeHandlersResult = if (runDCE) removeEmptyExceptionHandlers(method) else RemoveHandlersResult.NoneRemoved
      traceIfChanged("staleHandlers")

      // SIMPLIFY JUMPS
      // almost all of the above optimizations enable simplifying more jumps, so we just run it in every iteration
      val runSimplifyJumps = compilerSettings.optSimplifyJumps
      val jumpsChanged = runSimplifyJumps && simplifyJumps(method)
      traceIfChanged("simplifyJumps")

      // See doc comment in the beginning of this file (optimizations marked UPSTREAM)
      val runNullnessAgain = boxUnboxChanged || callInlinedByStaleStores || pushPopNullCheckAdded
      val runDCEAgain = removeHandlersResult.liveHandlerRemoved || jumpsChanged
      val runBoxUnboxAgain = boxUnboxChanged || typeInsnChanged || pushPopRemoved || removeHandlersResult.liveHandlerRemoved
      val runCopyPropAgain = typeInsnChanged
      val runStaleStoresAgain = pushPopRemoved
      val runRedundantCastsAgain = typeInsnChanged || pushPopCastAdded
      val runPushPopAgain = jumpsChanged
      val runStoreLoadAgain = jumpsChanged
      val runAgain = runNullnessAgain || runDCEAgain || runBoxUnboxAgain || runCopyPropAgain || runStaleStoresAgain || runRedundantCastsAgain || runPushPopAgain || runStoreLoadAgain

      val downstreamRequireEliminateUnusedLocals = runAgain && removalRound(
        requestNullness = runNullnessAgain,
        requestDCE = runDCEAgain,
        requestBoxUnbox = runBoxUnboxAgain,
        requestCopyProp = runCopyPropAgain,
        requestStaleStores = runStaleStoresAgain,
        requestRedundantCasts = runRedundantCastsAgain,
        requestPushPop = runPushPopAgain,
        requestStoreLoad = runStoreLoadAgain,
        maxRecursion = maxRecursion - 1)._2

      val requireEliminateUnusedLocals = downstreamRequireEliminateUnusedLocals ||
        nullnessOptChanged || // nullness opt may eliminate stores / loads, rendering a local unused
        codeRemoved ||        // see comment in method `methodOptimizations`
        boxUnboxChanged ||    // box-unbox renders locals (holding boxes) unused
        storesRemoved  ||
        storeLoadRemoved ||
        removeHandlersResult.handlerRemoved

      val codeChanged = nullnessOptChanged || codeRemoved || boxUnboxChanged || copyPropChanged || storesRemoved || intrinsicRewrittenByStaleStores || callInlinedByStaleStores || typeInsnChanged || intrinsicRewrittenByCasts || pushPopRemoved || storeLoadRemoved || removeHandlersResult.handlerRemoved || jumpsChanged
      (codeChanged, requireEliminateUnusedLocals)
    }

    // we run DCE even if `isDceDone(method)`: the DCE here is more thorough than
    // `minimalRemoveUnreachableCode` that run before inlining.
    val (nullnessDceBoxesCastsCopypropPushpopOrJumpsChanged, requireEliminateUnusedLocals) = removalRound(
      requestNullness = true,
      requestDCE = true,
      requestBoxUnbox = true,
      requestCopyProp = true,
      requestStaleStores = true,
      requestRedundantCasts = true,
      requestPushPop = true,
      requestStoreLoad = true)

    if (compilerSettings.optUnreachableCode) BackendUtils.setDceDone(method)

    // (*) Removing stale local variable descriptors is required for correctness, see comment in `methodOptimizations`
    val localsRemoved =
      if (compilerSettings.optCompactLocals) compactLocalVariables(method) // also removes unused
      else if (requireEliminateUnusedLocals) removeUnusedLocalVariableNodes(method)() // (*)
      else false
    traceIfChanged("localVariables")

    // The asm.MethodWriter writes redundant line numbers 1:1 to the classfile, so we filter them out
    // Note that this traversal also cleans up `LABEL_REACHABLE_STATUS` flags that were added to Label's
    // `stats` fields during `removeUnreachableCodeImpl` (both are guarded by `optUnreachableCode`).
    val lineNumbersRemoved = if (compilerSettings.optUnreachableCode) removeEmptyLineNumbers(method) else false
    traceIfChanged("lineNumbers")

    // assert that local variable annotations are empty (we don't emit them) - otherwise we'd have
    // to eliminate those covering an empty range, similar to removeUnusedLocalVariableNodes.
    def nullOrEmpty[T](l: java.util.List[T]) = l == null || l.isEmpty
    assert(nullOrEmpty(method.visibleLocalVariableAnnotations), method.visibleLocalVariableAnnotations)
    assert(nullOrEmpty(method.invisibleLocalVariableAnnotations), method.invisibleLocalVariableAnnotations)

    // clear the non-official "access" flags once we're done and no longer look at them
    BackendUtils.clearMaxsComputed(method)
    BackendUtils.clearDceDone(method)

    nullnessDceBoxesCastsCopypropPushpopOrJumpsChanged || localsRemoved || lineNumbersRemoved
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
      lazy val nullnessAnalyzer = new NullnessAnalyzer(method, ownerClassName, backendUtils.isNonNullMethodInvocation, compilerSettings.optAssumeModulesNonNull)

      // When running nullness optimizations the method may still have unreachable code. Analyzer
      // frames of unreachable instructions are `null`.
      def frameAt(insn: AbstractInsnNode): Option[Frame[NullnessValue]] = Option(nullnessAnalyzer.frameAt(insn))

      def nullness(insn: AbstractInsnNode, slot: Int): Option[NullnessValue] = {
        frameAt(insn).map(_.getValue(slot))
      }

      def isNull(insn: AbstractInsnNode, slot: Int) = nullness(insn, slot).contains(NullValue)

      // cannot change instructions while iterating, it gets the analysis out of synch (indexed by instructions)
      val toReplace = mutable.Map.empty[AbstractInsnNode, List[AbstractInsnNode]]

      val it = method.instructions.iterator
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

      // We don't need to worry about CallGraph.closureInstantiations and
      // BackendUtils.indyLambdaImplMethods, the removed instructions are not IndyLambdas
      def removeFromCallGraph(insn: AbstractInsnNode): Unit = insn match {
        case mi: MethodInsnNode => callGraph.removeCallsite(mi, method)
        case _ =>
      }

      for ((oldOp, newOps) <- toReplace) {
        for (newOp <- newOps) method.instructions.insertBefore(oldOp, newOp)
        method.instructions.remove(oldOp)
        removeFromCallGraph(oldOp)
      }

      val changed = toReplace.nonEmpty
      changed
    }
  }

  /**
   * Removes unreachable basic blocks, returns `true` if instructions were removed.
   *
   * When this method returns, each `labelNode.getLabel` has a status set whether the label is live
   * or not. This can be queried using `BackendUtils.isLabelReachable`.
   */
  def removeUnreachableCodeImpl(method: MethodNode, ownerClassName: InternalName): Boolean = {
    val size = method.instructions.size

    // queue of instruction indices where analysis should start
    var queue = new Array[Int](8)
    var top = -1
    def enq(i: Int): Unit = {
      if (top == queue.length - 1) {
        val nq = new Array[Int](queue.length * 2)
        Array.copy(queue, 0, nq, 0, queue.length)
        queue = nq
      }
      top += 1
      queue(top) = i
    }
    def deq(): Int = {
      val r = queue(top)
      top -= 1
      r
    }

    val handlers = new Array[mutable.ArrayBuffer[TryCatchBlockNode]](size)
    val tcbIt = method.tryCatchBlocks.iterator()
    while (tcbIt.hasNext) {
      val tcb = tcbIt.next()
      var i = method.instructions.indexOf(tcb.start)
      val e = method.instructions.indexOf(tcb.end)
      while (i < e) {
        var insnHandlers = handlers(i)
        if (insnHandlers == null) {
          insnHandlers = mutable.ArrayBuffer.empty[TryCatchBlockNode]
          handlers(i) = insnHandlers
        }
        insnHandlers += tcb
        i += 1
      }
    }

    val visited = mutable.BitSet.empty

    def enqInsn(insn: AbstractInsnNode): Unit = {
      enqInsnIndex(method.instructions.indexOf(insn))
    }

    def enqInsnIndex(insnIndex: Int): Unit = {
      if (insnIndex < size && !visited.contains(insnIndex))
        enq(insnIndex)
    }

    /* Subroutines are jumps, historically used for `finally` (https://www.artima.com/underthehood/finally.html)
     *   - JSR pushes the return address (next instruction) on the stack and jumps to a label
     *   - The subroutine typically saves the address to a local variable (ASTORE x)
     *   - The subroutine typically jumps back to the return address using `RET x`, where `x` is the local variable
     *
     * However, the JVM spec does not require subroutines to `RET x` to their caller, they could return back to an
     * outer subroutine caller (nested subroutines), or `RETURN`, or use a static jump. Static analysis of subroutines
     * is therefore complex (https://www21.in.tum.de/~kleing/papers/KleinW-TPHOLS03.pdf).
     *
     * The asm.Analyzer however makes the assumption that subroutines only occur in the shape emitted by early
     * javac, i.e., `RET` always returns to the next enclosing caller. So we do that as well.
     */

    enq(0)
    while (top != -1) {
      val insnIndex = deq()
      val insn = method.instructions.get(insnIndex)
      visited.addOne(insnIndex)

      if (insn.getOpcode == -1) { // frames, labels, line numbers
        enqInsnIndex(insnIndex + 1)
      } else {
        insn match {
          case j: JumpInsnNode =>
            enqInsn(j.label)
            // For conditional jumps the successor is also a possible control flow target.
            // The successor of a JSR is also enqueued, see subroutine shape assumption above.
            if (j.getOpcode != GOTO) enqInsnIndex(insnIndex + 1)

          case l: LookupSwitchInsnNode =>
            var j = 0
            while (j < l.labels.size) {
              enqInsn(l.labels.get(j)); j += 1
            }
            enqInsn(l.dflt)

          case t: TableSwitchInsnNode =>
            var j = 0
            while (j < t.labels.size) {
              enqInsn(t.labels.get(j)); j += 1
            }
            enqInsn(t.dflt)

          case r: VarInsnNode if r.getOpcode == RET =>
            // the target is already enqueued, see subroutine shape assumption above

          case _ =>
            if (insn.getOpcode != ATHROW && !isReturn(insn))
              enqInsnIndex(insnIndex + 1)
        }
      }

      val insnHandlers = handlers(insnIndex)
      if (insnHandlers != null)
        insnHandlers.foreach(h => enqInsn(h.handler))
    }

    def dce(): Boolean = {
      var i = 0
      var changed = false
      val itr = method.instructions.iterator()
      while (itr.hasNext) {
        val insn = itr.next()
        val isLive = visited.contains(i)

        insn match {
          case l: LabelNode =>
            // label nodes are not removed: they might be referenced for example in a LocalVariableNode
            if (isLive) BackendUtils.setLabelReachable(l) else BackendUtils.clearLabelReachable(l)

          case _: LineNumberNode =>

          case _ =>
            if (!isLive || insn.getOpcode == NOP) {
              // Instruction iterators allow removing during iteration.
              // Removing is O(1): instructions are doubly linked list elements.
              itr.remove()
              changed = true
              insn match {
                case invocation: MethodInsnNode => callGraph.removeCallsite(invocation, method)
              case indy: InvokeDynamicInsnNode =>
                callGraph.removeClosureInstantiation(indy, method)
                removeIndyLambdaImplMethod(ownerClassName, method, indy)
                case _ =>
              }
            }
        }
        i += 1
      }
      changed
    }

    dce()
  }

  /**
   * Eliminate `CHECKCAST` instructions that are statically known to succeed. This is safe if the
   * tested object is null: `null.asInstanceOf` always succeeds.
   *
   * Replace `INSTANCEOF` instructions with `ICONST_0/1` if the result is statically known.
   *
   * Since this optimization runs a type analysis, we use it to rewrite some intrinsic method calls
   *   - `java.lang.reflect.Arrays.getLength(x)` when `x` is statically known to be an array:
   *     rewrite to `ARRAYLENGTH`
   *   - `x.getClass` when `x` is statically known to be a primitive array. Rewrite to `LDC`.
   *
   * The type of the tested object is determined using a NonLubbingTypeFlowAnalyzer. Note that this
   * analysis collapses LUBs of non-equal references types to Object for simplicity. Example:
   * given `B <: A <: Object`, the cast in `(if (..) new B else new A).asInstanceOf[A]` would not
   * be eliminated.
   *
   * Note: to rewrite `INSTANCEOF` tests, we also run a nullness analyzer. We need to know nullness
   * because `null.isInstanceOf` is always `false`.
   *
   * Returns two booleans (typeInsnChanged, intrinsicRewritten)
   */
  def eliminateRedundantCastsAndRewriteSomeIntrinsics(method: MethodNode, owner: InternalName): (Boolean, Boolean) = if (!AsmAnalyzer.sizeOKForNullness(method)) (false, false) else {
    def isSubType(aDescOrIntN: String, bDescOrIntN: String): Boolean = {
      // Neither a nor b may be descriptors for primitive types. INSTANCEOF and CHECKCAST require
      //   - "objectref must be of type reference" and
      //   - "constant pool item must be a class, array, or interface type"
      // However we may get a mix of descriptors and internal names. The typeAnalyzer returns
      // descriptors (`Lfoo/C;`), the descriptor in a TypeInsn is an internal name (`foo/C`) or an
      // array descriptor.
      def sameClass(a: String, b: String) = {
        a == b ||
          a.length - 2 == b.length && a(0) == 'L' && a.last == ';' && a.regionMatches(1, b, 0, b.length) ||
          b.length - 2 == a.length && b(0) == 'L' && b.last == ';' && b.regionMatches(1, a, 0, a.length)
      }
      sameClass(aDescOrIntN, bDescOrIntN) || sameClass(bDescOrIntN, ObjectRef.internalName) ||
        bTypeForDescriptorOrInternalNameFromClassfile(aDescOrIntN).conformsTo(bTypeForDescriptorOrInternalNameFromClassfile(bDescOrIntN)).getOrElse(false)
    }

    // precondition: !isSubType(aDescOrIntN, bDescOrIntN)
    def isUnrelated(aDescOrIntN: String, bDescOrIntN: String): Boolean = {
      @tailrec
      def impl(aTp: BType, bTp: BType): Boolean = {
        ((aTp, bTp): @unchecked) match {
          case (aa: ArrayBType, ba: ArrayBType) =>
            impl(aa.elementType, ba.elementType)
          case (act: ClassBType, bct: ClassBType) =>
            val noItf = act.isInterface.flatMap(aIf => bct.isInterface.map(bIf => !aIf && !bIf)).getOrElse(false)
            noItf && !bct.conformsTo(act).getOrElse(true)
          case (_: PrimitiveBType, _: RefBType) | (_: RefBType, _: PrimitiveBType) =>
            true
          case (_: PrimitiveBType, _: PrimitiveBType) =>
            // note that this case happens for array element types. [S does not conform to [I.
            aTp != bTp
          case _ =>
            false
        }
      }
      impl(
        bTypeForDescriptorOrInternalNameFromClassfile(aDescOrIntN),
        bTypeForDescriptorOrInternalNameFromClassfile(bDescOrIntN))
    }

    lazy val typeAnalyzer = new NonLubbingTypeFlowAnalyzer(method, owner)
    lazy val nullnessAnalyzer = new NullnessAnalyzer(method, owner, backendUtils.isNonNullMethodInvocation, compilerSettings.optAssumeModulesNonNull)

    // cannot remove instructions while iterating, it gets the analysis out of synch (indexed by instructions)
    val toReplace = mutable.Map.empty[AbstractInsnNode, List[AbstractInsnNode]]

    val it = method.instructions.iterator
    while (it.hasNext) it.next() match {
      case ti: TypeInsnNode =>
        val opc = ti.getOpcode
        if (opc == CHECKCAST || opc == INSTANCEOF) {
          lazy val valueNullness = {
            val frame = nullnessAnalyzer.frameAt(ti)
            frame.getValue(frame.stackTop)
          }
          if (opc == INSTANCEOF && valueNullness == NullValue) {
            toReplace(ti) = List(getPop(1), new InsnNode(ICONST_0))
          } else {
            val valueDesc = typeAnalyzer.preciseAaloadTypeDesc({
              val frame = typeAnalyzer.frameAt(ti)
              frame.getValue(frame.stackTop)
            })
            if (isSubType(valueDesc, ti.desc)) {
              if (opc == CHECKCAST) {
                toReplace(ti) = Nil
              } else if (valueNullness == NotNullValue) {
                toReplace(ti) = List(getPop(1), new InsnNode(ICONST_1))
              }
            } else if (opc == INSTANCEOF && isUnrelated(valueDesc, ti.desc)) {
              // the two types are unrelated, so the instance check is known to fail
              toReplace(ti) = List(getPop(1), new InsnNode(ICONST_0))
            }
          }
        }

      case mi: MethodInsnNode =>
        // Rewrite some known method invocations
        if (BackendUtils.isArrayGetLengthOnStaticallyKnownArray(mi, typeAnalyzer)) {
          // Array.getLength(x) where x is known to be an array
          toReplace(mi) = List(new InsnNode(ARRAYLENGTH))
        } else {
          // x.getClass where x is statically known to be a primitive array
          val getClassTp = BackendUtils.getClassOnStaticallyKnownPrimitiveArray(mi, typeAnalyzer)
          if (getClassTp != null) {
            toReplace(mi) = List(getPop(1), new LdcInsnNode(getClassTp))
          }
        }

      case _ =>
    }

    var typeInsnChanged = false
    var intrinsicRewritten = false

    for ((oldOp, newOp) <- toReplace) {
      if (oldOp.isInstanceOf[TypeInsnNode]) typeInsnChanged = true
      else if (oldOp.isInstanceOf[MethodInsnNode]) intrinsicRewritten = true
      for (n <- newOp) method.instructions.insertBefore(oldOp, n)
      method.instructions.remove(oldOp)
    }

    (typeInsnChanged, intrinsicRewritten)
  }
}

object LocalOptImpls {
  /**
   * Remove exception handlers that cover empty code blocks. A block is considered empty if it
   * consist only of labels, frames, line numbers, nops and gotos.
   *
   * There are no executable instructions that we can assume don't throw (eg ILOAD). The JVM spec
   * basically says that a VirtualMachineError may be thrown at any time:
   *   https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.3
   *
   * Note that no instructions are eliminated.
   *
   * Returns a pair of booleans (handlerRemoved, liveHandlerRemoved)
   *
   * The `liveHandlerRemoved` result depends on `removeUnreachableCode` being executed
   * before, so that `BackendUtils.isLabelReachable` gives a correct answer.
   */
  def removeEmptyExceptionHandlers(method: MethodNode): RemoveHandlersResult = {
    /* True if there exists code between start and end. */
    @tailrec
    def containsExecutableCode(start: AbstractInsnNode, end: LabelNode): Boolean = {
      start != end && ((start.getOpcode: @switch) match {
        // FrameNode, LabelNode and LineNumberNode have opcode == -1.
        case -1 | GOTO => containsExecutableCode(start.getNext, end)
        case _ => true
      })
    }

    var result: RemoveHandlersResult = RemoveHandlersResult.NoneRemoved

    val handlersIter = method.tryCatchBlocks.iterator
    while (handlersIter.hasNext) {
      val handler = handlersIter.next()
      if (!containsExecutableCode(handler.start, handler.end)) {
        if (!result.handlerRemoved) result = RemoveHandlersResult.HandlerRemoved
        if (!result.liveHandlerRemoved && BackendUtils.isLabelReachable(handler.start))
          result = RemoveHandlersResult.LiveHandlerRemoved
        handlersIter.remove()
      }
    }

    result
  }

  sealed abstract class RemoveHandlersResult {
    def handlerRemoved: Boolean = false
    def liveHandlerRemoved: Boolean = false
  }
  object RemoveHandlersResult {
    object NoneRemoved extends RemoveHandlersResult
    object HandlerRemoved extends RemoveHandlersResult {
      override def handlerRemoved: Boolean = true
    }
    object LiveHandlerRemoved extends RemoveHandlersResult {
      override def handlerRemoved: Boolean = true
      override def liveHandlerRemoved: Boolean = true
    }
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
    val localsIter = method.localVariables.iterator
    while (localsIter.hasNext) {
      val local = localsIter.next()
      val index = local.index
      // parameters and `this` (the lowest indices, starting at 0) are never removed or renumbered
      if (index >= firstLocalIndex) {
        def previousOrSelf(insn: AbstractInsnNode) = insn.getPrevious match {
          case null => insn
          case i => i
        }
        val storeInsn = previousOrSelf(local.start) // the start index is after the store, see scala/scala#8897
        val used = variableIsUsed(storeInsn, local.end, index)
        if (!used) localsIter.remove()
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
      for (_ <- renumber.length until minLength) renumber += -1

      renumber(index) = index
      if (isWide) renumber(index + 1) = index
    }

    // first phase: collect all used local variables. if the variable at index x is used, set
    // renumber(x) = x, otherwise renumber(x) = -1. if the variable is wide (long or double), set
    // renumber(x+1) = x.

    val firstLocalIndex = parametersSize(method)
    for (i <- 0 until firstLocalIndex) renumber += i // parameters and `this` are always used.
    method.instructions.iterator.asScala foreach {
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
      method.instructions.iterator.asScala.foreach {
        case VarInstruction(varIns, slot) =>
          val oldIndex = slot
          if (oldIndex >= firstLocalIndex && renumber(oldIndex) != oldIndex) varIns match {
            case vi: VarInsnNode => vi.`var` = renumber(slot)
            case ii: IincInsnNode => ii.`var` = renumber(slot)
            case x                => throw new MatchError(x)
          }
        case _ =>
      }
      true
    }
  }

  /**
   * Removes LineNumberNodes that don't describe any executable instructions.
   *
   * As a side-effect, this traversal removes the `LABEL_REACHABLE_STATUS` flag from all label's
   * `status` fields.
   *
   * This method expects (and asserts) that the `start` label of each LineNumberNode is the
   * lexically preceding label declaration.
   */
  def removeEmptyLineNumbers(method: MethodNode): Boolean = {
    @tailrec
    def isEmpty(node: AbstractInsnNode): Boolean = node.getNext match {
      case null => true
      case _: LineNumberNode => true
      case n if n.getOpcode >= 0 => false
      case n => isEmpty(n)
    }

    val initialSize = method.instructions.size
    val iterator = method.instructions.iterator
    var previousLabel: LabelNode = null
    while (iterator.hasNext) {
      iterator.next match {
        case label: LabelNode =>
          BackendUtils.clearLabelReachable(label)
          previousLabel = label
        case line: LineNumberNode if isEmpty(line) =>
          assert(line.start == previousLabel)
          iterator.remove()
        case _ =>
      }
    }
    method.instructions.size != initialSize
  }

  /**
   * Apply various simplifications to branching instructions.
   */
  def simplifyJumps(method: MethodNode): Boolean = {

    val allHandlers = method.tryCatchBlocks.asScala.toSet

    // A set of all exception handlers that guard the current instruction, required for simplifyGotoReturn
    var activeHandlers = Set.empty[TryCatchBlockNode]

    val jumpInsns = mutable.LinkedHashMap.empty[JumpInsnNode, Boolean]

    for (insn <- method.instructions.iterator.asScala) insn match {
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
      jumpInsns.subtractOne(jump)
      _jumpTargets = null
    }

    def replaceJumpByPop(jump: JumpInsnNode) = {
      removeJumpAndAdjustStack(method, jump)
      removeJumpFromMap(jump)
    }

    /*
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

    /*
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

    /*
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

    /*
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

    /*
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
     *   https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.return
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

    /*
     * Replace conditional jump instructions with GOTO or NOP if statically known to be true or false.
     *
     * {{{
     *      ICONST_0; IFEQ l;
     *   => ICONST_0; POP; GOTO l;
     *
     *      ICONST_1; IFEQ l;
     *   => ICONST_1; POP;
     * }}}
     *
     * Note that the LOAD/POP pairs will be removed later by `eliminatePushPop`, and the code between
     * the GOTO and `l` will be removed by DCE (if it's not jumped into from somewhere else).
     */
    def simplifyConstantConditions(instruction: AbstractInsnNode): Boolean = {
      def replace(jump: JumpInsnNode, success: Boolean): Boolean = {
        if (success) method.instructions.insert(jump, new JumpInsnNode(GOTO, jump.label))
        replaceJumpByPop(jump)
        true
      }

      instruction match {
        case ConditionalJump(jump) =>
          previousExecutableInstruction(instruction, jumpTargets) match {
            case Some(prev) =>
              val prevOp = prev.getOpcode
              val isIConst = prevOp >= ICONST_M1 && prevOp <= ICONST_5
              (jump.getOpcode: @switch) match {
                case IFNULL if prevOp == ACONST_NULL =>
                  replace(jump, success = true)
                case IFNONNULL if prevOp == ACONST_NULL =>
                  replace(jump, success = false)
                case IFEQ if isIConst =>
                  replace(jump, success = prevOp == ICONST_0)
                case IFNE if isIConst =>
                  replace(jump, success = prevOp != ICONST_0)
                /* TODO: we also have IFLE, IF_?CMP* and friends, but how likely are they to be profitably optimizeable? */
                case _ => false
              }
            case _ => false
          }
        case _ => false
      }
    }

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
            changed = simplifyConstantConditions(jumpInsn) || changed
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
