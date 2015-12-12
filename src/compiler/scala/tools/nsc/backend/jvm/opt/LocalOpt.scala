/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.{tailrec, switch}
import scala.tools.asm.Type
import scala.tools.asm.tree.analysis.BasicInterpreter
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree._
import scala.collection.{mutable, immutable}
import scala.collection.convert.decorateAsScala._
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
 * unreachable code / DCE (removes instructions of basic blocks to which there is no branch)
 *   + enables downstream:
 *     - stale stores (loads may be eliminated, removing consumers of a store)
 *     - empty handlers (try blocks may become empty)
 *     - simplify jumps (goto l; [dead code]; l: ..) => remove goto
 *     - stale local variable descriptors
 *
 *   note that eliminating empty handlers and stale local variable descriptors is required for
 *   correctness, see the comment in the body of `methodOptimizations`.
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
 * push-pop (when a POP is the only consumer of a value, remove the POP and its producer)
 *   + eanbles UPSTREAM:
 *     - stale stores (if a LOAD is removed, a corresponding STORE may become stale)
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
 *   + enables downstream:
 *     - simplify jumps
 *
 * simplify jumps (various, like `GOTO l; l: ...`, see doc comments of individual optimizations)
 *   + enables UPSTREAM
 *     - unreachable code (`GOTO a; a: GOTO b; b: ...`, the first jump is changed to `GOTO b`, the second becomes unreachable)
 *     - store-load pairs (a `GOTO l; l: ...` is removed between store and load)
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
  import backendUtils._

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
    !compilerSettings.YoptNone && clazz.methods.asScala.foldLeft(false) {
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

    /**
     * Runs the optimizations that depend on each other in a loop until reaching a fixpoint. See
     * comment in class [[LocalOpt]].
     */
    def removalRound(runDCE: Boolean, runCopyProp: Boolean, maxRecursion: Int = 10): Boolean = {
      if (maxRecursion == 0) return false

      // Both AliasingAnalyzer (used in copyProp) and ProdConsAnalyzer (used in eliminateStaleStores)
      // require not having unreachable instructions (null frames).
      val dceRequired = runDCE || compilerSettings.YoptCopyPropagation
      val (codeRemoved, liveLabels) = if (dceRequired) removeUnreachableCodeImpl(method, ownerClassName) else (false, Set.empty[LabelNode])

      // runCopyProp is only true in the first iteration; there's no optimization below that enables
      // further copy propagation. it is still part of the fixpoint loop because it needs to run after DCE.
      val copyPropChanged = if (runCopyProp) copyProp(method, ownerClassName) else false

      val storesRemoved = if (compilerSettings.YoptCopyPropagation) eliminateStaleStores(method, ownerClassName) else false

      val pushPopRemoved = if (compilerSettings.YoptCopyPropagation) eliminatePushPop(method, ownerClassName) else false

      val storeLoadRemoved = if (compilerSettings.YoptCopyPropagation) eliminateStoreLoad(method) else false

      val removedHandlers = if (dceRequired) removeEmptyExceptionHandlers(method) else Set.empty[TryCatchBlockNode]
      val handlersRemoved = removedHandlers.nonEmpty
      val liveHandlerRemoved = removedHandlers.exists(h => liveLabels(h.start))

      val jumpsChanged = if (compilerSettings.YoptSimplifyJumps) simplifyJumps(method) else false

      // See doc comment in the beginning of this file (optimizations marked UPSTREAM)
      if (liveHandlerRemoved || jumpsChanged) {
        // changing live handlers or jumps enables DCE to remove more code, so runDCE = true
        removalRound(runDCE = true, runCopyProp = false, maxRecursion = maxRecursion - 1)
      } else if (pushPopRemoved) {
        // pushPop doesn't enable DCE, but other optimizations (stale stores). so we iterate, but without DCE.
        removalRound(runDCE = false, runCopyProp = false, maxRecursion = maxRecursion - 1)
      }

      codeRemoved || copyPropChanged || storesRemoved || pushPopRemoved || storeLoadRemoved || handlersRemoved || jumpsChanged
    }

    val dceCopyPropHandlersOrJumpsChanged = if (AsmAnalyzer.sizeOKForBasicValue(method)) {
      // we run DCE even if the method is already in the `unreachableCodeEliminated` map: the DCE
      // here is more thorough than `minimalRemoveUnreachableCode` that run before inlining.
      val r = removalRound(
        runDCE = compilerSettings.YoptUnreachableCode,
        runCopyProp = compilerSettings.YoptCopyPropagation)
      if (compilerSettings.YoptUnreachableCode) unreachableCodeEliminated += method
      r
    } else false

    // (*) Removing stale local variable descriptors is required for correctness of unreachable-code
    val localsRemoved =
      if (compilerSettings.YoptCompactLocals) compactLocalVariables(method) // also removes unused
      else if (compilerSettings.YoptUnreachableCode) removeUnusedLocalVariableNodes(method)() // (*)
      else false

    val lineNumbersRemoved = if (compilerSettings.YoptUnreachableCode) removeEmptyLineNumbers(method) else false

    val labelsRemoved = if (compilerSettings.YoptUnreachableCode) removeEmptyLabelNodes(method) else false

    // assert that local variable annotations are empty (we don't emit them) - otherwise we'd have
    // to eliminate those covering an empty range, similar to removeUnusedLocalVariableNodes.
    def nullOrEmpty[T](l: java.util.List[T]) = l == null || l.isEmpty
    assert(nullOrEmpty(method.visibleLocalVariableAnnotations), method.visibleLocalVariableAnnotations)
    assert(nullOrEmpty(method.invisibleLocalVariableAnnotations), method.invisibleLocalVariableAnnotations)

    dceCopyPropHandlersOrJumpsChanged || localsRemoved || lineNumbersRemoved || labelsRemoved
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
          maxLocals = math.max(maxLocals, v.`var` + longSize + 1) // + 1 becauase local numbers are 0-based

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
   * For every `xLOAD n`, find all local variable slots that are aliases of `n` using an
   * AliasingAnalyzer and change the instruction to `xLOAD m` where `m` is the smallest alias.
   * This leaves behind potentially stale `xSTORE n` instructions, which are then eliminated
   * by [[eliminateStaleStores]].
   */
  def copyProp(method: MethodNode, owner: InternalName): Boolean = {
    AsmAnalyzer.sizeOKForAliasing(method) && {
      var changed = false
      val numParams = parametersSize(method)
      lazy val aliasAnalysis = new AsmAnalyzer(method, owner, new AliasingAnalyzer(new BasicInterpreter))

      // Remember locals that are used in a `LOAD` instruction. Assume a program has two LOADs:
      //
      //   ...
      //   LOAD 3  // aliases of 3 here: <3>
      //   ...
      //   LOAD 1  // aliases of 1 here: <1, 3>
      //
      // In this example, we should change the second load from 1 to 3, which might render the
      // local variable 1 unused.
      val knownUsed = new Array[Boolean](method.maxLocals)

      def usedOrMinAlias(it: IntIterator, init: Int): Int = {
        if (knownUsed(init)) init
        else {
          var r = init
          while (it.hasNext) {
            val n = it.next()
            // knownUsed.lenght is the number of locals, `n` may be a stack slot
            if (n < knownUsed.length && knownUsed(n)) return n
            if (n < r) r = n
          }
          r
        }
      }

      val it = method.instructions.iterator
      while (it.hasNext) it.next() match {
        case vi: VarInsnNode if vi.`var` >= numParams && isLoad(vi) =>
          val aliases = aliasAnalysis.frameAt(vi).asInstanceOf[AliasingFrame[_]].aliasesOf(vi.`var`)
          if (aliases.size > 1) {
            val alias = usedOrMinAlias(aliases.iterator, vi.`var`)
            if (alias != -1) {
              changed = true
              vi.`var` = alias
            }
          }
          knownUsed(vi.`var`) = true

        case _ =>
      }

      changed
    }
  }

  /**
   * Eliminate `xSTORE` instructions that have no consumer. If the instruction can be completely
   * eliminated, it is replaced by a POP. The [[eliminatePushPop]] cleans up unnecessary POPs.
   *
   * Note that an `ASOTRE` can not always be eliminated: it removes a reference to the object that
   * is currently stored in that local, which potentially frees it for GC (SI-5313). Therefore
   * we replace such stores by `POP; ACONST_NULL; ASTORE x`.
   */
  def eliminateStaleStores(method: MethodNode, owner: InternalName): Boolean = {
    AsmAnalyzer.sizeOKForSourceValue(method) && {
      lazy val prodCons = new ProdConsAnalyzer(method, owner)
      def hasNoCons(varIns: AbstractInsnNode, slot: Int) = prodCons.consumersOfValueAt(varIns.getNext, slot).isEmpty

      var changed = false

      // insns to delete: IINC that have no consumer
      val toDelete = mutable.ArrayBuffer.empty[IincInsnNode]
      // xSTORE insns to be replaced by POP or POP2
      val storesToDrop = mutable.ArrayBuffer.empty[VarInsnNode]
      // ASTORE insn that have no consumer.
      //   - if the local is not live, the store is replaced by POP
      //   - otherwise, pop the argument value and store NULL instead. Unless the boolean field is
      //     `true`: then the store argument is already known to be ACONST_NULL.
      val toNullOut = mutable.ArrayBuffer.empty[(VarInsnNode, Boolean)]
      // `true` for variables that are known to be live
      val liveVars = new Array[Boolean](method.maxLocals)

      val it = method.instructions.iterator
      while (it.hasNext) it.next() match {
        case vi: VarInsnNode if isStore(vi) && hasNoCons(vi, vi.`var`) =>
          val canElim = vi.getOpcode != ASTORE || {
            val currentFieldValueProds = prodCons.initialProducersForValueAt(vi, vi.`var`)
            currentFieldValueProds.size == 1 && (currentFieldValueProds.head match {
              case ParameterProducer(0) => !isStaticMethod(method) // current field value is `this`, which won't be gc'd anyway
              case _: UninitializedLocalProducer => true // field is not yet initialized, so current value cannot leak
              case _ => false
            })
          }
          if (canElim) storesToDrop += vi
          else {
            val prods = prodCons.producersForValueAt(vi, prodCons.frameAt(vi).stackTop)
            val isStoreNull = prods.size == 1 && prods.head.getOpcode == ACONST_NULL
            toNullOut += ((vi, isStoreNull))
          }
          changed = true

        case ii: IincInsnNode if hasNoCons(ii, ii.`var`) =>
          toDelete += ii
          changed = true

        case vi: VarInsnNode =>
          val longSize = if (isSize2LoadOrStore(vi.getOpcode)) 1 else 0
          liveVars(vi.`var`) = true

        case ii: IincInsnNode =>
          liveVars(ii.`var`) = true
        case _ =>
      }

      def replaceByPop(vi: VarInsnNode): Unit = {
        val size = if (isSize2LoadOrStore(vi.getOpcode)) 2 else 1
        method.instructions.set(vi, getPop(size))
      }

      toDelete foreach method.instructions.remove
      storesToDrop foreach replaceByPop
      for ((vi, isStoreNull) <- toNullOut) {
        if (!liveVars(vi.`var`)) replaceByPop(vi) // can drop `ASTORE x` where x has only dead stores
        else {
          if (!isStoreNull) {
            val prev = vi.getPrevious
            method.instructions.insert(prev, new InsnNode(ACONST_NULL))
            method.instructions.insert(prev, getPop(1))
          }
        }
      }

      changed
    }
  }

  /**
   * When a POP instruction has a single producer, remove the POP and eliminate the producer by
   * bubbling up the POPs. For example, given
   *   ILOAD 1; ILOAD 2; IADD; POP
   * we first eliminate the POP, then the IADD, then its inputs, so the entire sequence goes away.
   * If a producer cannot be eliminated (need to keep side-effects), a POP is inserted.
   *
   * A special case eliminates the creation of unused objects with side-effect-free constructors:
   *   NEW scala/Tuple1; DUP; ALOAD 0; INVOKESPECIAL scala/Tuple1.<init>; POP
   * The POP has a signle producer (the DUP), it's easy to eliminate these two. A special case
   * is needed to eliminate the INVOKESPECIAL and NEW.
   */
  def eliminatePushPop(method: MethodNode, owner: InternalName): Boolean = {
    AsmAnalyzer.sizeOKForSourceValue(method) && {
      // A queue of instructions producing a value that has to be eliminated. If possible, the
      // instruction (and its inputs) will be removed, otherwise a POP is inserted after
      val queue = mutable.Queue.empty[ProducedValue]
      // Contains constructor invocations for values that can be eliminated if unused.
      val sideEffectFreeConstructorCalls = mutable.ArrayBuffer.empty[MethodInsnNode]

      // instructions to remove (we don't change the bytecode while analyzing it. this allows
      // running the ProdConsAnalyzer only once.)
      val toRemove = mutable.Set.empty[AbstractInsnNode]
      // instructions to insert before some instruction
      val toInsertBefore = mutable.Map.empty[AbstractInsnNode, List[InsnNode]]
      // an instruction to insert after some instruction
      val toInsertAfter = mutable.Map.empty[AbstractInsnNode, AbstractInsnNode]

      lazy val prodCons = new ProdConsAnalyzer(method, owner)

      /**
       * Returns the producers for the stack value `inputSlot` consumed by `cons`, if the consumer
       * instruction is the only consumer for all of these producers.
       *
       * If a producer has multiple consumers, or the value is the caught exception in a catch
       * block, this method returns Set.empty.
       */
      def producersIfSingleConsumer(cons: AbstractInsnNode, inputSlot: Int): Set[AbstractInsnNode] = {
        /**
         * True if the values produced by `prod` are all the same. Most instructions produce a single
         * value. DUP and DUP2 (with a size-2 input) produce two equivalent values. However, there
         * are some exotic instructions that produce multiple non-equal values (DUP_X1, SWAP, ...).
         *
         * Assume we have `DUP_X2; POP`. In order to remove the `POP` we need to change the DUP_X2
         * into something else, which is not straightforward.
         *
         * Since scalac never emits any of those exotic bytecodes, we don't optimize them.
         */
        def producerHasSingleOutput(prod: AbstractInsnNode): Boolean = prod match {
          case _: ExceptionProducer[_] | _: UninitializedLocalProducer =>
            // POP of an exception in a catch block cannot be removed. For an uninitialized local,
            // there should not be a consumer. We are conservative and include it here, so the
            // producer would not be removed.
            false

          case _: ParameterProducer =>
            true

          case _ => (prod.getOpcode: @switch) match {
            case DUP => true
            case DUP2 => prodCons.frameAt(prod).peekStack(0).getSize == 2
            case _ => InstructionStackEffect.prod(InstructionStackEffect.forAsmAnalysis(prod, prodCons.frameAt(prod))) == 1
          }
        }

        val prods = prodCons.producersForValueAt(cons, inputSlot)
        val singleConsumer = prods forall { prod =>
          producerHasSingleOutput(prod) && {
            // for DUP / DUP2, we only consider the value that is actually consumed by cons
            val conss = prodCons.consumersOfValueAt(prod.getNext, inputSlot)
            conss.size == 1 && conss.head == cons
          }
        }
        if (singleConsumer) prods else Set.empty
      }

      /**
       * For a POP instruction that is the single consumer of its producers, remove the POP and
       * enqueue the producers.
       */
      def handleInitialPop(pop: AbstractInsnNode): Unit = {
        val prods = producersIfSingleConsumer(pop, prodCons.frameAt(pop).stackTop)
        if (prods.nonEmpty) {
          toRemove += pop
          val size = if (pop.getOpcode == POP2) 2 else 1
          queue ++= prods.map(ProducedValue(_, size))
        }
      }

      /**
       * Traverse the method in its initial state and collect all POP instructions and side-effect
       * free constructor invocations that can be eliminated.
       */
      def collectInitialPopsAndPureConstrs(): Unit = {
        val it = method.instructions.iterator
        while (it.hasNext) {
          val insn = it.next()
          (insn.getOpcode: @switch) match {
            case POP | POP2 =>
              handleInitialPop(insn)

            case INVOKESPECIAL =>
              val mi = insn.asInstanceOf[MethodInsnNode]
              if (isSideEffectFreeConstructorCall(mi)) sideEffectFreeConstructorCalls += mi

            case _ =>
          }
        }
      }

      /**
       * Eliminate the `numArgs` inputs of the instruction `prod` (which was eliminated). Fo
       * each input value
       *   - if the `prod` instruction is the single consumer, enqueue the producers of the input
       *   - otherwise, insert a POP instruction to POP the input value
       */
      def handleInputs(prod: AbstractInsnNode, numArgs: Int): Unit = {
        val frame = prodCons.frameAt(prod)
        val pops = mutable.ListBuffer.empty[InsnNode]
        @tailrec def handle(stackOffset: Int): Unit = {
          if (stackOffset >= 0) {
            val prods = producersIfSingleConsumer(prod, frame.stackTop - stackOffset)
            val nSize = frame.peekStack(stackOffset).getSize
            if (prods.isEmpty) pops append getPop(nSize)
            else queue ++= prods.map(ProducedValue(_, nSize))
            handle(stackOffset - 1)
          }
        }
        handle(numArgs - 1) // handle stack offsets (numArgs - 1) to 0
        if (pops.nonEmpty) toInsertBefore(prod) = pops.toList
      }

      /**
       * Eliminate the closure value produced by `indy`. If the SAM type is known to construct
       * without side-effects (e.g. scala/runtime/java8/JFunctionN), the `indy` and its inputs
       * are eliminated, otherwise a POP is inserted.
       */
      def handleClosureInst(indy: InvokeDynamicInsnNode): Unit = {
        if (isrJFunctionType(Type.getReturnType(indy.desc).getInternalName)) {
          toRemove += indy
          callGraph.removeClosureInstantiation(indy, method)
          handleInputs(indy, Type.getArgumentTypes(indy.desc).length)
        } else {
          toInsertAfter(indy) = getPop(1)
        }
      }

      def runQueue(): Unit = while (queue.nonEmpty) {
        val ProducedValue(prod, size) = queue.dequeue()

        def prodString = s"Producer ${AsmUtils textify prod}@${method.instructions.indexOf(prod)}\n${AsmUtils textify method}"
        def popAfterProd(): Unit = {
          toInsertAfter(prod) = getPop(size) }

        (prod.getOpcode: @switch) match {
          case ACONST_NULL | ICONST_M1 | ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 | LCONST_0 | LCONST_1 | FCONST_0 | FCONST_1 | FCONST_2 | DCONST_0 | DCONST_1 |
               BIPUSH | SIPUSH | ILOAD | LLOAD | FLOAD | DLOAD | ALOAD=>
            toRemove += prod

          case opc @ (DUP | DUP2) =>
            assert(opc != 2 || size == 2, s"DUP2 for two size-1 values; $prodString") // ensured in method `producerHasSingleOutput`
            if (toRemove(prod))
              // the DUP is already scheduled for removal because one of its consumers is a POP.
              // now the second consumer is also a POP, so we need to eliminate the DUP's input.
              handleInputs(prod, 1)
            else
              toRemove += prod

          case DUP_X1 | DUP_X2 | DUP2_X1 | DUP2_X2 | SWAP =>
            // these are excluded in method `producerHasSingleOutput`
            assert(false, s"Cannot eliminate value pushed by an instruction with multiple output values; $prodString")

          case IDIV | LDIV | IREM | LREM =>
            popAfterProd() // keep potential division by zero

          case IADD | LADD | FADD | DADD | ISUB | LSUB | FSUB | DSUB | IMUL | LMUL | FMUL | DMUL | FDIV | DDIV | FREM | DREM |
               LSHL | LSHR | LUSHR |
               IAND | IOR | IXOR | LAND | LOR | LXOR |
               LCMP | FCMPL | FCMPG | DCMPL | DCMPG=>
            toRemove += prod
            handleInputs(prod, 2)

          case INEG | LNEG | FNEG | DNEG |
               I2L | I2F | I2D | L2I | L2F | L2D | F2I | F2L | F2D | D2I | D2L | D2F | I2B | I2C | I2S =>
            toRemove += prod
            handleInputs(prod, 1)

          case GETFIELD | GETSTATIC =>
            // TODO eliminate side-effect free module loads (https://github.com/scala/scala-dev/issues/16)
            if (isBoxedUnit(prod)) toRemove += prod
            else popAfterProd() // keep potential class initialization (static field) or NPE (instance field)

          case INVOKEVIRTUAL | INVOKESPECIAL | INVOKESTATIC | INVOKEINTERFACE =>
            val methodInsn = prod.asInstanceOf[MethodInsnNode]
            if (isSideEffectFreeCall(methodInsn)) {
              toRemove += prod
              callGraph.removeCallsite(methodInsn, method)
              val receiver = if (methodInsn.getOpcode == INVOKESTATIC) 0 else 1
              handleInputs(prod, Type.getArgumentTypes(methodInsn.desc).length + receiver)
            } else
              popAfterProd()

          case INVOKEDYNAMIC =>
            prod match {
              case callGraph.LambdaMetaFactoryCall(indy, _, _, _) => handleClosureInst(indy)
              case _ => popAfterProd()
            }

          case CHECKCAST =>
            // Special case for `IndyLambda JFunctionN; CHECKCAST FuncionN`. This sequence is emitted
            // for all closure instantiations, see https://issues.scala-lang.org/browse/SI-9540
            val castType = prod.asInstanceOf[TypeInsnNode].desc
            if (isScalaFunctionType(castType)) {

              // the `FunctionN` part of an internal name (e.g. `scala/runtime/java8/JFunction1$mcDI$sp`)
              def funPart(funType: String): String = {
                var end = funType.indexOf('$')
                if (end == -1) end = funType.length
                funType.substring(funType.lastIndexOf('/') + 1, end)
              }

              // true if the cast will always succeed
              def castOk(indy: InvokeDynamicInsnNode): Boolean = {
                val generatedFunctionType = Type.getReturnType(indy.desc).getInternalName
                isrJFunctionType(generatedFunctionType) && funPart(generatedFunctionType) == "J" + funPart(castType)
              }

              val castProd = producersIfSingleConsumer(prod, prodCons.frameAt(prod).stackTop)
              if (castProd.size == 1) castProd.head match {
                case callGraph.LambdaMetaFactoryCall(indy, _, _, _) if castOk(indy) =>
                  toRemove += prod        // remove the cast
                  handleClosureInst(indy) // remove the indyLambda
                case _ => popAfterProd()
              } else
                popAfterProd()
            } else
              popAfterProd()

          case NEW =>
            if (isNewForSideEffectFreeConstructor(prod)) toRemove += prod
            else popAfterProd()

          case LDC =>
            // don't remove class literals: keep the potential NoClassDefFoundError
            if (prod.asInstanceOf[LdcInsnNode].cst.isInstanceOf[Type]) popAfterProd()
            else toRemove += prod

          case NEWARRAY | ANEWARRAY =>
            toRemove += prod
            handleInputs(prod, 1)

          case MULTIANEWARRAY =>
            toRemove += prod
            handleInputs(prod, prod.asInstanceOf[MultiANewArrayInsnNode].dims)

          case _ =>
            popAfterProd()
        }
      }

      // there are two cases when we can eliminate a constructor call:
      //   - NEW T; INVOKESPECIAL T.<init> -- there's no DUP, the new object is consumed only by the constructor)
      //   - NEW T; DUP; INVOKESPECIAL T.<init>, where the DUP will be removed
      def eliminateUnusedPureConstructorCalls(): Boolean = {
        var changed = false

        def removeConstructorCall(mi: MethodInsnNode): Unit = {
          toRemove += mi
          callGraph.removeCallsite(mi, method)
          sideEffectFreeConstructorCalls -= mi
          changed = true
        }

        for (mi <- sideEffectFreeConstructorCalls.toList) { // toList to allow removing elements while traversing
          val frame = prodCons.frameAt(mi)
          val stackTop = frame.stackTop
          val numArgs = Type.getArgumentTypes(mi.desc).length
          val receiverProds = producersIfSingleConsumer(mi, stackTop - numArgs)
          if (receiverProds.size == 1) {
            val receiverProd = receiverProds.head
            if (receiverProd.getOpcode == NEW) {
              removeConstructorCall(mi)
              handleInputs(mi, numArgs + 1) // removes the producers of args and receiver
            } else if (receiverProd.getOpcode == DUP && toRemove.contains(receiverProd)) {
              val dupProds = producersIfSingleConsumer(receiverProd, prodCons.frameAt(receiverProd).stackTop)
              if (dupProds.size == 1 && dupProds.head.getOpcode == NEW) {
                removeConstructorCall(mi)
                handleInputs(mi, numArgs) // removes the producers of args. the producer of the receiver is DUP and already in toRemove.
                queue += ProducedValue(dupProds.head, 1) // removes the NEW (which is NOT the producer of the receiver!)
              }
            }
          }
        }
        changed
      }

      collectInitialPopsAndPureConstrs()

      // eliminating producers enables eliminating unused constructor calls (when a DUP gets removed).
      // vice-versa, eliminating a constructor call adds producers of constructor parameters to the queue.
      // so the two run in a loop.
      runQueue()
      while(eliminateUnusedPureConstructorCalls())
        runQueue()

      var changed = false
      toInsertAfter foreach {
        case (target, insn) =>
          nextExecutableInstructionOrLabel(target) match {
            // `insn` is of type `InsnNode`, so we only need to check the Opcode when comparing to another instruction
            case Some(next) if next.getOpcode == insn.getOpcode && toRemove(next) =>
              // Inserting and removing a POP at the same place should not enable `changed`. This happens
              // when a POP directly follows a producer that cannot be eliminated, e.g. INVOKESTATIC A.m ()I; POP
              // The POP is initially added to `toRemove`, and the `INVOKESTATIC` producer is added to the queue.
              // Because the producer cannot be elided, a POP is added to `toInsertAfter`.
              toRemove -= next

            case _ =>
              changed = true
              method.instructions.insert(target, insn)
          }
      }
      toInsertBefore foreach {
        case (target, insns) =>
          changed = true
          insns.foreach(method.instructions.insertBefore(target, _))
      }
      toRemove foreach { insn =>
        changed = true
        method.instructions.remove(insn)
      }
      changed
    }
  }

  case class ProducedValue(producer: AbstractInsnNode, size: Int) {
    override def toString = s"<${AsmUtils textify producer}>"
  }

  /**
   * Remove `xSTORE n; xLOAD n` paris if
   *   - the local variable n is not used anywhere else in the method (1), and
   *   - there are no executable instructions and no live labels (jump targets) between the two (2)
   *
   * Note: store-load pairs that cannot be eliminated could be replaced by `DUP; xSTORE n`, but
   * that's just cosmetic and doesn't help for anything.
   *
   * (1) This could be made more precise by running a prodCons analysis and checking that the load
   * is the only user of the store. Then we could eliminate the pair even if the variable is live
   * (except for ASTORE, SI-5313). Not needing an analyzer is more efficient, and catches most
   * cases.
   *
   * (2) The implementation uses a conservative estimation for liveness (if some instruction uses
   * local n, then n is considered live in the entire method). In return, it doesn't need to run an
   * Analyzer on the method, making it more efficient.
   *
   * This method also removes `ACONST_NULL; ASTORE n` if the local n is not live. This pattern is
   * introduced by [[eliminateStaleStores]].
   *
   * The implementation is a little tricky to support the following case:
   *   ISTORE 1; ISTORE 2; ILOAD 2; ACONST_NULL; ASTORE 3; ILOAD 1
   * The outer store-load pair can be removed if two the inner pairs can be.
   */
  def eliminateStoreLoad(method: MethodNode): Boolean = {
    val removePairs = mutable.Set.empty[RemovePair]
    val liveVars = new Array[Boolean](method.maxLocals)
    val liveLabels = mutable.Set.empty[LabelNode]

    def mkRemovePair(store: VarInsnNode, other: AbstractInsnNode, depends: List[RemovePairDependency]): RemovePair = {
      val r = RemovePair(store, other, depends)
      removePairs += r
      r
    }

    def registerLiveVarsLabels(insn: AbstractInsnNode): Unit = insn match {
      case vi: VarInsnNode => liveVars(vi.`var`) = true
      case ii: IincInsnNode => liveVars(ii.`var`) = true
      case j: JumpInsnNode => liveLabels += j.label
      case s: TableSwitchInsnNode => liveLabels += s.dflt; liveLabels ++= s.labels.asScala
      case s: LookupSwitchInsnNode => liveLabels += s.dflt; liveLabels ++= s.labels.asScala
      case _ =>
    }

    val pairStartStack = new mutable.Stack[(AbstractInsnNode, mutable.ListBuffer[RemovePairDependency])]

    def push(insn: AbstractInsnNode) = {
      pairStartStack push ((insn, mutable.ListBuffer.empty))
    }

    def addDepends(dependency: RemovePairDependency) = if (pairStartStack.nonEmpty) {
      val (_, depends) = pairStartStack.top
      depends += dependency
    }

    def completesStackTop(load: AbstractInsnNode) = isLoad(load) && pairStartStack.nonEmpty && {
      pairStartStack.top match {
        case (store: VarInsnNode, _) => store.`var` == load.asInstanceOf[VarInsnNode].`var`
        case _ => false
      }
    }

    /**
     * Try to pair `insn` with its correspondant on the stack
     *   - if the stack top is a store and `insn` is a corresponding load, create a pair
     *   - otherwise, check the two top stack values for `null; store`. if it matches, create
     *     a pair and continue pairing `insn` on the remaining stack
     *   - otherwise, empty the stack and mark the local variables in it live
     */
    def tryToPairInstruction(insn: AbstractInsnNode): Unit = {
      @tailrec def emptyStack(): Unit = if (pairStartStack.nonEmpty) {
        registerLiveVarsLabels(pairStartStack.pop()._1)
        emptyStack()
      }

      @tailrec def tryPairing(): Unit = {
        if (completesStackTop(insn)) {
          val (store: VarInsnNode, depends) = pairStartStack.pop()
          addDepends(mkRemovePair(store, insn, depends.toList))
        } else if (pairStartStack.nonEmpty) {
          val (top, topDepends) = pairStartStack.pop()
          if (pairStartStack.nonEmpty) {
            (pairStartStack.top, top) match {
              case ((ldNull: InsnNode, depends), store: VarInsnNode) if ldNull.getOpcode == ACONST_NULL && store.getOpcode == ASTORE =>
                pairStartStack.pop()
                addDepends(mkRemovePair(store, ldNull, depends.toList))
                // example: store; (null; store;) (store; load;) load
                //                         s1^     ^^^^^p1^^^^^        // p1 is added to s1's depends
                // then:    store; (null; store;) load
                //           s2^    ^^^^p2^^^^^                        // p1 and p2 are added to s2's depends
                topDepends foreach addDepends
                tryPairing()

              case _ =>
                // empty the stack - a non-matching insn was found, cannot create any pairs to remove
                registerLiveVarsLabels(insn)
                registerLiveVarsLabels(top)
                emptyStack()
            }
          } else {
            // stack only has one element
            registerLiveVarsLabels(insn)
            registerLiveVarsLabels(top)
          }
        } else {
          // stack is empty already
          registerLiveVarsLabels(insn)
        }
      }

      tryPairing()
    }


    var insn = method.instructions.getFirst

    @tailrec def advanceToNextExecutableOrLabel(): Unit = {
      insn = insn.getNext
      if (insn != null && !isExecutable(insn) && !insn.isInstanceOf[LabelNode]) advanceToNextExecutableOrLabel()
    }

    while (insn != null) {
      insn match {
        case _ if insn.getOpcode == ACONST_NULL          => push(insn)
        case vi: VarInsnNode if isStore(vi)              => push(insn)
        case label: LabelNode if pairStartStack.nonEmpty => addDepends(LabelNotLive(label))
        case _                                           => tryToPairInstruction(insn)
      }
      advanceToNextExecutableOrLabel()
    }

    // elide RemovePairs that depend on live labels or other RemovePair that have to be elided.
    // example:  store 1; store 2; label x; load 2; load 1
    // if x is live, the inner pair has to be elided, causing the outer pair to be elided too.

    var doneEliding = false

    def elide(removePair: RemovePair) = {
      doneEliding = false
      liveVars(removePair.store.`var`) = true
      removePairs -= removePair
    }

    while (!doneEliding) {
      doneEliding = true
      for (removePair <- removePairs.toList) {
        val slot = removePair.store.`var`
        if (liveVars(slot)) elide(removePair)
        else removePair.depends foreach {
          case LabelNotLive(label) => if (liveLabels(label)) elide(removePair)
          case other: RemovePair => if (!removePairs(other)) elide(removePair)
        }
      }
    }

    for (removePair <- removePairs) {
      method.instructions.remove(removePair.store)
      method.instructions.remove(removePair.other)
    }

    removePairs.nonEmpty
  }
}

trait RemovePairDependency
case class RemovePair(store: VarInsnNode, other: AbstractInsnNode, depends: List[RemovePairDependency]) extends RemovePairDependency {
  override def toString = s"<${AsmUtils textify store},${AsmUtils textify other}> [$depends]"
}
case class LabelNotLive(label: LabelNode) extends RemovePairDependency

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
      start != end && ((start.getOpcode : @switch) match {
        // FrameNode, LabelNode and LineNumberNode have opcode == -1.
        case -1 | GOTO => containsExecutableCode(start.getNext, end)
        case _ => true
      })
    }

    var removedHandlers = Set.empty[TryCatchBlockNode]
    val handlersIter = method.tryCatchBlocks.iterator()
    while(handlersIter.hasNext) {
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
    def variableIsUsed(start: AbstractInsnNode, end: LabelNode, varIndex: Int): Boolean = {
      start != end && (start match {
        case v: VarInsnNode if v.`var` == varIndex => true
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

    // Instructions that need to be removed. simplifyBranchOverGoto returns an instruction to be
    // removed. It cannot remove it itself because the instruction may be the successor of the current
    // instruction of the iterator, which is not supported in ASM.
    var instructionsToRemove = Set.empty[AbstractInsnNode]

    val iterator = method.instructions.iterator()
    while (iterator.hasNext) {
      val instruction = iterator.next()

      instruction match {
        case l: LabelNode =>
          activeHandlers ++= allHandlers.filter(_.start == l)
          activeHandlers = activeHandlers.filter(_.end != l)
        case _ =>
      }

      if (instructionsToRemove(instruction)) {
        iterator.remove()
        instructionsToRemove -= instruction
      } else if (isJumpNonJsr(instruction)) { // fast path - all of the below only treat jumps
        var jumpRemoved = simplifyThenElseSameTarget(method, instruction)

        if (!jumpRemoved) {
          changed = collapseJumpChains(instruction) || changed
          jumpRemoved = removeJumpToSuccessor(method, instruction)

          if (!jumpRemoved) {
            val staleGoto = simplifyBranchOverGoto(method, instruction)
            instructionsToRemove ++= staleGoto
            changed ||= staleGoto.nonEmpty
            changed = simplifyGotoReturn(method, instruction, inTryBlock = activeHandlers.nonEmpty) || changed
          }
        }
        changed ||= jumpRemoved
      }
    }
    assert(instructionsToRemove.isEmpty, "some optimization required removing a previously traversed instruction. add `instructionsToRemove.foreach(method.instructions.remove)`")
    changed
  }

  /**
   * Removes a conditional jump if it is followed by a GOTO to the same destination.
   *
   *      CondJump l;  [nops];  GOTO l;  [...]
   *      POP*;        [nops];  GOTO l;  [...]
   *
   * Introduces 1 or 2 POP instructions, depending on the number of values consumed by the CondJump.
   */
  private def simplifyThenElseSameTarget(method: MethodNode, instruction: AbstractInsnNode): Boolean = instruction match {
    case ConditionalJump(jump) =>
      nextExecutableInstruction(instruction) match {
        case Some(Goto(elseJump)) if sameTargetExecutableInstruction(jump, elseJump) =>
          removeJumpAndAdjustStack(method, jump)
          true

        case _ => false
      }
    case _ => false
  }

  /**
   * Replace jumps to a sequence of GOTO instructions by a jump to the final destination.
   *
   *      Jump l;  [any ops];  l: GOTO m;  [any ops];  m: GOTO n;  [any ops];   n: NotGOTO; [...]
   *   => Jump n;  [rest unchanged]
   *
   * If there's a loop of GOTOs, the initial jump is replaced by one of the labels in the loop.
   */
  private def collapseJumpChains(instruction: AbstractInsnNode): Boolean = instruction match {
    case JumpNonJsr(jump) =>
      val target = finalJumpTarget(jump)
      if (jump.label == target) false else {
        jump.label = target
        true
      }

    case _ => false
  }

  /**
   * Eliminates unnecessary jump instructions
   *
   *      Jump l;  [nops];  l: [...]
   *   => POP*;    [nops];  l: [...]
   *
   * Introduces 0, 1 or 2 POP instructions, depending on the number of values consumed by the Jump.
   */
  private def removeJumpToSuccessor(method: MethodNode, instruction: AbstractInsnNode) = instruction match {
    case JumpNonJsr(jump) if nextExecutableInstruction(jump, alsoKeep = Set(jump.label)) == Some(jump.label) =>
      removeJumpAndAdjustStack(method, jump)
      true
    case _ => false
  }

  /**
   * If the "else" part of a conditional branch is a simple GOTO, negates the conditional branch
   * and eliminates the GOTO.
   *
   *      CondJump l;         [nops, no labels];  GOTO m;  [nops];  l: [...]
   *   => NegatedCondJump m;  [nops, no labels];           [nops];  l: [...]
   *
   * Note that no label definitions are allowed in the first [nops] section. Otherwise, there could
   * be some other jump to the GOTO, and eliminating it would change behavior.
   *
   * For technical reasons, we cannot remove the GOTO here (*).Instead this method returns an Option
   * containing the GOTO that needs to be eliminated.
   *
   * (*) The ASM instruction iterator (used in the caller [[simplifyJumps]]) has an undefined
   *     behavior if the successor of the current instruction is removed, which may be the case here
   */
  private def simplifyBranchOverGoto(method: MethodNode, instruction: AbstractInsnNode): Option[JumpInsnNode] = instruction match {
    case ConditionalJump(jump) =>
      // don't skip over labels, see doc comment
      nextExecutableInstructionOrLabel(jump) match {
        case Some(Goto(goto)) =>
          if (nextExecutableInstruction(goto, alsoKeep = Set(jump.label)) == Some(jump.label)) {
            val newJump = new JumpInsnNode(negateJumpOpcode(jump.getOpcode), goto.label)
            method.instructions.set(jump, newJump)
            Some(goto)
          } else None

        case _ => None
      }
    case _ => None
  }

  /**
   * Inlines xRETURN and ATHROW
   *
   *      GOTO l;            [any ops];  l: xRETURN/ATHROW
   *   => xRETURN/ATHROW;    [any ops];  l: xRETURN/ATHROW
   *
   * inlining is only done if the GOTO instruction is not part of a try block, otherwise the
   * rewrite might change the behavior. For xRETURN, the reason is that return instructions may throw
   * an IllegalMonitorStateException, as described here:
   *   http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.return
   */
  private def simplifyGotoReturn(method: MethodNode, instruction: AbstractInsnNode, inTryBlock: Boolean): Boolean = !inTryBlock && (instruction match {
    case Goto(jump) =>
      nextExecutableInstruction(jump.label) match {
        case Some(target) =>
          if (isReturn(target) || target.getOpcode == ATHROW) {
            method.instructions.set(jump, target.clone(null))
            true
          } else false

        case _ => false
      }
    case _ => false
  })
}
