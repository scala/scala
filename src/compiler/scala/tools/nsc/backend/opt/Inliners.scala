/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Iulian Dragos
 */


package scala.tools.nsc
package backend.opt

import scala.collection.mutable
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.NoSourceFile

/**
 *  @author Iulian Dragos
 */
abstract class Inliners extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions.{
    NullClass, NothingClass, ObjectClass,
    PredefModule, RuntimePackage, ScalaInlineClass, ScalaNoInlineClass,
    isFunctionType
  }

  val phaseName = "inliner"

  /** Debug - for timing the inliner. */
  private def timed[T](s: String, body: => T): T = {
    val t1 = System.currentTimeMillis()
    val res = body
    val t2 = System.currentTimeMillis()
    val ms = (t2 - t1).toInt
    if (ms >= MAX_INLINE_MILLIS)
      println("%s: %d milliseconds".format(s, ms))

    res
  }

  /** Look up implementation of method 'sym in 'clazz'.
   */
  def lookupImplFor(sym: Symbol, clazz: Symbol): Symbol = {
    // TODO: verify that clazz.superClass is equivalent here to clazz.tpe.parents(0).typeSymbol (.tpe vs .info)
    def needsLookup = (
         (clazz != NoSymbol)
      && (clazz != sym.owner)
      && !sym.isEffectivelyFinal
      && clazz.isEffectivelyFinal
    )
    def lookup(clazz: Symbol): Symbol = {
      // println("\t\tlooking up " + meth + " in " + clazz.fullName + " meth.owner = " + meth.owner)
      if (sym.owner == clazz || isBottomType(clazz)) sym
      else sym.overridingSymbol(clazz) match {
        case NoSymbol  => if (sym.owner.isTrait) sym else lookup(clazz.superClass)
        case imp       => imp
      }
    }
    if (needsLookup) {
      val concreteMethod = lookup(clazz)
      debuglog("\tlooked up method: " + concreteMethod.fullName)

      concreteMethod
    }
    else sym
  }

  /* A warning threshold */
  private final val MAX_INLINE_MILLIS = 2000

  /** The maximum size in basic blocks of methods considered for inlining. */
  final val MAX_INLINE_SIZE = 16

  /** Maximum loop iterations. */
  final val MAX_INLINE_RETRY = 15

  /** Small method size (in blocks) */
  val SMALL_METHOD_SIZE = 1

  /** Create a new phase */
  override def newPhase(p: Phase) = new InliningPhase(p)

  /** The Inlining phase.
   */
  class InliningPhase(prev: Phase) extends ICodePhase(prev) {
    def name = phaseName
    val inliner = new Inliner

    override def apply(c: IClass) {
      inliner analyzeClass c
    }

    override def run() {
      try {
        super.run()
      } finally {
        inliner.clearCaches()
      }
    }
  }

  def isBottomType(sym: Symbol) = sym == NullClass || sym == NothingClass
  def posToStr(pos: util.Position) = if (pos.isDefined) pos.point.toString else "<nopos>"

  /** Is the given class a closure? */
  def isClosureClass(cls: Symbol): Boolean =
    cls.isFinal && cls.isSynthetic && !cls.isModuleClass && cls.isAnonymousFunction

  /*
      TODO now that Inliner runs faster we could consider additional "monadic methods" (in the limit, all those taking a closure as last arg)
      Any "monadic method" occurring in a given caller C that is not `isMonadicMethod()` will prevent CloseElim from eliminating
      any anonymous-closure-class any whose instances are given as argument to C invocations.
   */
  def isMonadicMethod(sym: Symbol) = {
    nme.unspecializedName(sym.name) match {
      case nme.foreach | nme.filter | nme.withFilter | nme.map | nme.flatMap => true
      case _                                                                 => false
    }
  }

  def hasInline(sym: Symbol)    = sym hasAnnotation ScalaInlineClass
  def hasNoInline(sym: Symbol)  = sym hasAnnotation ScalaNoInlineClass

  /**
   * Simple inliner.
   */
  class Inliner {
    object NonPublicRefs extends Enumeration {
      val Private, Protected, Public = Value

      /** Cache whether a method calls private members. */
      val usesNonPublics = mutable.Map.empty[IMethod, Value]
    }
    import NonPublicRefs._

    /** The current iclass */
    private var currentIClazz: IClass = _
    private def warn(pos: Position, msg: String) = currentIClazz.cunit.inlinerWarning(pos, msg)

    val recentTFAs = mutable.Map.empty[Symbol, Tuple2[Boolean, analysis.MethodTFA]]
    private def getRecentTFA(incm: IMethod): (Boolean, analysis.MethodTFA) = {

        def containsRETURN(blocks: List[BasicBlock]) = blocks exists { bb => bb.lastInstruction.isInstanceOf[RETURN] }

      val opt = recentTFAs.get(incm.symbol)
      if(opt.isDefined) {
        // FYI val cachedBBs = opt.get._2.in.keySet
        // FYI assert(incm.blocks.toSet == cachedBBs)
        // incm.code.touched plays no role here
        return opt.get
      }

      val hasRETURN = containsRETURN(incm.code.blocksList) || (incm.exh exists { eh => containsRETURN(eh.blocks) })
      var a: analysis.MethodTFA = null
      if(hasRETURN) { a = new analysis.MethodTFA(incm); a.run }

      if(hasInline(incm.symbol)) { recentTFAs.put(incm.symbol, (hasRETURN, a)) }

      (hasRETURN, a)
    }

    def clearCaches() {
      // methods
      NonPublicRefs.usesNonPublics.clear()
      recentTFAs.clear
      tfa.knownUnsafe.clear()
      tfa.knownSafe.clear()
      tfa.knownNever.clear()
      // basic blocks
      tfa.preCandidates.clear()
      tfa.relevantBBs.clear()
      // callsites
      tfa.remainingCALLs.clear()
      tfa.isOnWatchlist.clear()
    }

    def analyzeClass(cls: IClass): Unit =
      if (settings.inline.value) {
        debuglog("Analyzing " + cls)

        this.currentIClazz = cls
        val ms = cls.methods filterNot { _.symbol.isConstructor }
        ms foreach { im =>
          if(hasInline(im.symbol)) {
            log("Not inlining into " + im.symbol.originalName.decode + " because it is marked @inline.")
          } else if(im.hasCode && !im.symbol.isBridge) {
            analyzeMethod(im)
          }
        }
      }

    val tfa   = new analysis.MTFAGrowable()
    tfa.stat  = global.opt.printStats
    val staleOut      = new mutable.ListBuffer[BasicBlock]
    val splicedBlocks = mutable.Set.empty[BasicBlock]
    val staleIn       = mutable.Set.empty[BasicBlock]

    /**
     * A transformation local to the body of the IMethod received as argument.
     * An linining decision consists in replacing a callsite with the body of the callee.
     * Please notice that, because `analyzeMethod()` itself may modify a method body,
     * the particular callee bodies that end up being inlined depend on the particular order in which methods are visited
     * (no topological sorting over the call-graph is attempted).
     *
     * Making an inlining decision requires type-flow information for both caller and callee.
     * Regarding the caller, such information is needed only for basic blocks containing inlining candidates
     * (and their transitive predecessors). This observation leads to using a custom type-flow analysis (MTFAGrowable)
     * that can be re-inited, i.e. that reuses lattice elements (type-flow information computed in a previous iteration)
     * as starting point for faster convergence in a new iteration.
     *
     * The mechanics of inlining are iterative for a given invocation of `analyzeMethod(m)`,
     * and are affected by inlinings from previous iterations
     * (ie, "heuristic" rules are based on statistics tracked for that purpose):
     *
     *   (1) before the iterations proper start, so-called preinlining is performed.
     *       Those callsites whose (receiver, concreteMethod) are both known statically
     *       can be analyzed for inlining before computing a type-flow. Details in `preInline()`
     *
     *   (2) the first iteration computes type-flow information for basic blocks containing inlining candidates
     *       (and their transitive predecessors), so called `relevantBBs` basic blocks.
     *       The ensuing analysis of each candidate (performed by `analyzeInc()`)
     *       may result in a CFG isomorphic to that of the callee being inserted in place of the callsite
     *       (i.e. a CALL_METHOD instruction is replaced with a single-entry single-exit CFG,
     *        a situation we call "successful inlining").
     *
     *   (3) following iterations have `relevantBBs` updated to focus on the inlined basic blocks and their successors only.
     *       Details in `MTFAGrowable.reinit()`
     * */
    def analyzeMethod(m: IMethod): Unit = {
      // m.normalize

      var sizeBeforeInlining  = m.code.blockCount
      var instrBeforeInlining = m.code.instructionCount
      var retry = false
      var count = 0

      // fresh name counter
      val fresh = mutable.HashMap.empty[String, Int] withDefaultValue 0
      // how many times have we already inlined this method here?
      val inlinedMethodCount = mutable.HashMap.empty[Symbol, Int] withDefaultValue 0

      val caller = new IMethodInfo(m)

      def preInline(isFirstRound: Boolean): Int = {
        val inputBlocks = caller.m.linearizedBlocks()
        val callsites: Function1[BasicBlock, List[opcodes.CALL_METHOD]] = {
          if(isFirstRound) tfa.conclusives else tfa.knownBeforehand
        }
        inlineWithoutTFA(inputBlocks, callsites)
      }

      /**
       *  Inline straightforward callsites (those that can be inlined without a TFA).
       *
       *  To perform inlining, all we need to know is listed as formal params in `analyzeInc()`:
       *    - callsite and block containing it
       *    - actual (ie runtime) class of the receiver
       *    - actual (ie runtime) method being invoked
       *    - stack length just before the callsite (to check whether enough arguments have been pushed).
       *  The assert below lists the conditions under which "no TFA is needed"
       *  (the statically known receiver and method are both final, thus, at runtime they can't be any others than those).
       *
       */
      def inlineWithoutTFA(inputBlocks: Traversable[BasicBlock], callsites: Function1[BasicBlock, List[opcodes.CALL_METHOD]]): Int = {
        var inlineCount = 0
        import scala.util.control.Breaks._
        for(x <- inputBlocks; easyCake = callsites(x); if easyCake.nonEmpty) {
          breakable {
            for(ocm <- easyCake) {
              assert(ocm.method.isEffectivelyFinal && ocm.method.owner.isEffectivelyFinal)
              if(analyzeInc(ocm, x, ocm.method.owner, -1, ocm.method)) {
                inlineCount += 1
                break
              }
            }
          }
        }

        inlineCount
      }

      /**
          Decides whether it's feasible and desirable to inline the body of the method given by `concreteMethod`
          at the program point given by `i` (a callsite). The boolean result indicates whether inlining was performed.

       */
      def analyzeInc(i: CALL_METHOD, bb: BasicBlock, receiver: Symbol, stackLength: Int, concreteMethod: Symbol): Boolean = {
        var inlined = false
        val msym = i.method

        def warnNoInline(reason: String) = {
          if (hasInline(msym) && !caller.isBridge)
            warn(i.pos, "Could not inline required method %s because %s.".format(msym.originalName.decode, reason))
        }

        def isAvailable = icodes available concreteMethod.enclClass

        if (!isAvailable && shouldLoadImplFor(concreteMethod, receiver)) {
          // Until r22824 this line was:
          //   icodes.icode(concreteMethod.enclClass, true)
          //
          // Changing it to
          //   icodes.load(concreteMethod.enclClass)
          // was the proximate cause for SI-3882:
          //   error: Illegal index: 0 overlaps List((variable par1,LONG))
          //   error: Illegal index: 0 overlaps List((variable par1,LONG))
          icodes.load(concreteMethod.enclClass)
        }

        def isCandidate = (
             isClosureClass(receiver)
          || concreteMethod.isEffectivelyFinal
          || receiver.isEffectivelyFinal
        )
        def isApply     = concreteMethod.name == nme.apply
        def isCountable = !(
             isClosureClass(receiver)
          || isApply
          || isMonadicMethod(concreteMethod)
          || receiver.enclosingPackage == definitions.RuntimePackage
        )   // only count non-closures

        debuglog("Treating " + i
              + "\n\treceiver: " + receiver
              + "\n\ticodes.available: " + isAvailable
              + "\n\tconcreteMethod.isEffectivelyFinal: " + concreteMethod.isEffectivelyFinal)

        if (isAvailable && isCandidate) {
          lookupIMethod(concreteMethod, receiver) match {
            case Some(callee) =>
              val inc   = new IMethodInfo(callee)
              val pair  = new CallerCalleeInfo(caller, inc, fresh, inlinedMethodCount)

              if (pair isStampedForInlining stackLength) {
                retry = true
                inlined = true
                if (isCountable)
                  count += 1

                pair.doInline(bb, i)
                if (!inc.inline || inc.isMonadic)
                  caller.inlinedCalls += 1
                inlinedMethodCount(inc.sym) += 1

                /* Remove this method from the cache, as the calls-private relation
                 * might have changed after the inlining.
                 */
                usesNonPublics -= m
                recentTFAs     -= m.symbol
              }
              else {
                if (settings.debug.value)
                  pair logFailure stackLength

                warnNoInline(pair failureReason stackLength)
              }
            case None =>
              warnNoInline("bytecode was not available")
              debuglog("could not find icode\n\treceiver: " + receiver + "\n\tmethod: " + concreteMethod)
          }
        }
        else warnNoInline(
          if (!isAvailable) "bytecode was not available"
          else "it can be overridden"
        )

        inlined
      }

      /* Pre-inlining consists in invoking the usual inlining subroutine with (receiver class, concrete method) pairs as input
       * where both method and receiver are final, which implies that the receiver computed via TFA will always match `concreteMethod.owner`.
       *
       * As with any invocation of `analyzeInc()` the inlining outcome is based on heuristics which favor inlining an isMonadicMethod before other methods.
       * That's why preInline() is invoked twice: any inlinings downplayed by the heuristics during the first round get an opportunity to rank higher during the second.
       *
       * As a whole, both `preInline()` invocations amount to priming the inlining process,
       * so that the first TFA that is run afterwards is able to gain more information as compared to a cold-start.
       */
      val totalPreInlines = {
        val firstRound = preInline(true)
        if(firstRound == 0) 0 else (firstRound + preInline(false))
      }
      staleOut.clear()
      splicedBlocks.clear()
      staleIn.clear()

      do {
        retry = false
        log("Analyzing " + m + " count " + count + " with " + caller.length + " blocks")

        /* it's important not to inline in unreachable basic blocks. linearizedBlocks() returns only reachable ones. */
        tfa.callerLin = caller.m.linearizedBlocks()
           /* TODO Do we really want to inline inside exception handlers?
           *  Seems counterproductive (the larger the method the less likely it will be JITed).
            * The alternative would be `linearizer.linearizeAt(caller.m, caller.m.startBlock)`.
            * And, we would cut down on TFA iterations, too.
            * See also comment on the same topic in TypeFlowAnalysis. */

        tfa.reinit(m, staleOut.toList, splicedBlocks, staleIn)
        tfa.run
        staleOut.clear()
        splicedBlocks.clear()
        staleIn.clear()

        import scala.util.control.Breaks._
        for(bb <- tfa.callerLin; if tfa.preCandidates(bb)) {
          val cms = bb.toList collect { case cm : CALL_METHOD => cm }
          breakable {
            for (cm <- cms; if tfa.remainingCALLs.isDefinedAt(cm)) {
              val analysis.CallsiteInfo(_, receiver, stackLength, concreteMethod) = tfa.remainingCALLs(cm)
              if (analyzeInc(cm, bb, receiver, stackLength, concreteMethod)) {
                break
              }
            }
          }
        }

        /* As part of inlining, some instructions are moved to a new block.
         *     In detail: the instructions moved to a new block originally appeared after a (by now inlined) callsite.
         *     Their new home is an `afterBlock` created by `doInline()` to that effect.
         *     Each block in staleIn is one such `afterBlock`.
         *
         * Some of those instructions may be CALL_METHOD possibly tracked in `remainingCALLs`
         * (with an entry still noting the old containing block). However, that causes no problem:
         *
         *   (1) such callsites won't be analyzed for inlining by `analyzeInc()` (*in this iteration*)
         *       because of the `break` that abandons the original basic block where it was contained.
         *
         *   (2) Additionally, its new containing block won't be visited either (*in this iteration*)
         *       because the new blocks don't show up in the linearization computed before inlinings started:
         *       `for(bb <- tfa.callerLin; if tfa.preCandidates(bb)) {`
         *
         * For a next iteration, the new home of any instructions that have moved
         * will be tracked properly in `remainingCALLs` after `MTFAGrowable.reinit()` puts on radar their new homes.
         *
         */
        if(retry) {
          for(afterBlock <- staleIn) {
            val justCALLsAfter = afterBlock.toList collect { case c : opcodes.CALL_METHOD => c }
            for(ia <- justCALLsAfter) { tfa.remainingCALLs.remove(ia) }
          }
        }

        /*
        if(splicedBlocks.nonEmpty) { // TODO explore (saves time but leads to slightly different inlining decisions)
          // opportunistically perform straightforward inlinings before the next typeflow round
          val savedRetry = retry
          val savedStaleOut = staleOut.toSet; staleOut.clear()
          val savedStaleIn  = staleIn.toSet ; staleIn.clear()
          val howmany = inlineWithoutTFA(splicedBlocks, tfa.knownBeforehand)
          splicedBlocks ++= staleIn
          staleOut.clear(); staleOut ++= savedStaleOut;
          staleIn.clear();  staleIn  ++= savedStaleIn;
          retry = savedRetry
        }
        */

        if (tfa.stat)
          log(m.symbol.fullName + " iterations: " + tfa.iterations + " (size: " + caller.length + ")")
      }
      while (retry && count < MAX_INLINE_RETRY)

      m.normalize
      if (sizeBeforeInlining > 0) {
        val instrAfterInlining = m.code.instructionCount
        val prefix = if ((instrAfterInlining > 2 * instrBeforeInlining) && (instrAfterInlining > 200)) " !! " else ""
        log(prefix + " %s blocks before inlining: %d (%d) after: %d (%d)".format(
          m.symbol.fullName, sizeBeforeInlining, instrBeforeInlining, m.code.blockCount, instrAfterInlining))
      }
    }

    private def isHigherOrderMethod(sym: Symbol) = (
         sym.isMethod
      && beforeExplicitOuter(sym.info.paramTypes exists isFunctionType) // was "at erasurePhase.prev"
    )

    /** Should method 'sym' being called in 'receiver' be loaded from disk? */
    def shouldLoadImplFor(sym: Symbol, receiver: Symbol): Boolean = {
      def alwaysLoad    = (receiver.enclosingPackage == RuntimePackage) || (receiver == PredefModule.moduleClass)
      def loadCondition = sym.isEffectivelyFinal && isMonadicMethod(sym) && isHigherOrderMethod(sym)

      val res = hasInline(sym) || alwaysLoad || loadCondition
      debuglog("shouldLoadImplFor: " + receiver + "." + sym + ": " + res)
      res
    }

    class IMethodInfo(val m: IMethod) {
      val sym           = m.symbol
      val name          = sym.name
      def owner         = sym.owner
      def paramTypes    = sym.info.paramTypes
      def minimumStack  = paramTypes.length + 1

      def inline        = hasInline(sym)
      def noinline      = hasNoInline(sym)

      def isBridge      = sym.isBridge
      def isInClosure   = isClosureClass(owner)
      val isHigherOrder = isHigherOrderMethod(sym)
      def isMonadic     = isMonadicMethod(sym)

      def handlers      = m.exh
      def blocks        = m.blocks
      def locals        = m.locals
      def length        = blocks.length
      def openBlocks    = blocks filterNot (_.closed)
      def instructions  = m.code.instructions
      // def linearized    = linearizer linearize m

      def isSmall       = (length <= SMALL_METHOD_SIZE) && blocks(0).length < 10
      def isLarge       = length > MAX_INLINE_SIZE
      def isRecursive   = m.recursive
      def hasHandlers   = handlers.nonEmpty
      def hasNonFinalizerHandler = handlers exists {
        case _: Finalizer => true
        case _            => false
      }

      // the number of inlined calls in 'm', used by 'shouldInline'
      var inlinedCalls = 0

      def addLocals(ls: List[Local])  = m.locals ++= ls
      def addLocal(l: Local)          = addLocals(List(l))
      def addHandlers(exhs: List[ExceptionHandler]) = m.exh = exhs ::: m.exh
    }

    class CallerCalleeInfo(val caller: IMethodInfo, val inc: IMethodInfo, fresh: mutable.Map[String, Int], inlinedMethodCount: collection.Map[Symbol, Int]) {
      def isLargeSum  = caller.length + inc.length - 1 > SMALL_METHOD_SIZE

      private def freshName(s: String): TermName = {
        fresh(s) += 1
        newTermName(s + fresh(s))
      }

      /** Inline 'inc' into 'caller' at the given block and instruction.
       *  The instruction must be a CALL_METHOD.
       */
      def doInline(block: BasicBlock, instr: CALL_METHOD) {

        staleOut += block

        tfa.remainingCALLs.remove(instr) // this bookkpeeping is done here and not in MTFAGrowable.reinit due to (1st) convenience and (2nd) necessity.
        tfa.isOnWatchlist.remove(instr)  // ditto

        val targetPos = instr.pos
        log("Inlining " + inc.m + " in " + caller.m + " at pos: " + posToStr(targetPos))

        def blockEmit(i: Instruction) = block.emit(i, targetPos)
        def newLocal(baseName: String, kind: TypeKind) =
          new Local(caller.sym.newVariable(freshName(baseName), targetPos), kind, false)

        val (hasRETURN, a) = getRecentTFA(inc.m)

        /* The exception handlers that are active at the current block. */
        val activeHandlers = caller.handlers filter (_ covered block)

        /* Map 'original' blocks to the ones inlined in the caller. */
        val inlinedBlock = mutable.Map[BasicBlock, BasicBlock]()

        val varsInScope = mutable.HashSet[Local]() ++= block.varsInScope

        /** Side effects varsInScope when it sees SCOPE_ENTERs. */
        def instrBeforeFilter(i: Instruction): Boolean = {
          i match { case SCOPE_ENTER(l) => varsInScope += l ; case _ => () }
          i ne instr
        }
        val instrBefore = block.toList takeWhile instrBeforeFilter
        val instrAfter  = block.toList drop (instrBefore.length + 1)

        assert(!instrAfter.isEmpty, "CALL_METHOD cannot be the last instruction in block!")

        // store the '$this' into the special local
        val inlinedThis = newLocal("$inlThis", REFERENCE(ObjectClass))

        /** buffer for the returned value */
        val retVal = inc.m.returnType match {
          case UNIT  => null
          case x     => newLocal("$retVal", x)
        }

        val inlinedLocals = mutable.HashMap.empty[Local, Local]

        /** Add a new block in the current context. */
        def newBlock() = {
          val b = caller.m.code.newBlock
          activeHandlers foreach (_ addCoveredBlock b)
          if (retVal ne null) b.varsInScope += retVal
          b.varsInScope += inlinedThis
          b.varsInScope ++= varsInScope
          b
        }

        def translateExh(e: ExceptionHandler) = {
          val handler: ExceptionHandler = e.dup
          handler.covered = handler.covered map inlinedBlock
          handler setStartBlock inlinedBlock(e.startBlock)
          handler
        }

        /** alfa-rename `l` in caller's context. */
        def dupLocal(l: Local): Local = {
          val sym = caller.sym.newVariable(freshName(l.sym.name.toString), l.sym.pos)
          // sym.setInfo(l.sym.tpe)
          val dupped = new Local(sym, l.kind, false)
          inlinedLocals(l) = dupped
          dupped
        }

        val afterBlock = newBlock()

        /** Map from nw.init instructions to their matching NEW call */
        val pending: mutable.Map[Instruction, NEW] = new mutable.HashMap

        /** Map an instruction from the callee to one suitable for the caller. */
        def map(i: Instruction): Instruction = {
          def assertLocal(l: Local) = {
            assert(caller.locals contains l, "Could not find local '" + l + "' in locals, nor in inlinedLocals: " + inlinedLocals)
            i
          }
          def isInlined(l: Local) = inlinedLocals isDefinedAt l

          val newInstr = i match {
            case THIS(clasz)                    => LOAD_LOCAL(inlinedThis)
            case STORE_THIS(_)                  => STORE_LOCAL(inlinedThis)
            case JUMP(whereto)                  => JUMP(inlinedBlock(whereto))
            case CJUMP(succ, fail, cond, kind)  => CJUMP(inlinedBlock(succ), inlinedBlock(fail), cond, kind)
            case CZJUMP(succ, fail, cond, kind) => CZJUMP(inlinedBlock(succ), inlinedBlock(fail), cond, kind)
            case SWITCH(tags, labels)           => SWITCH(tags, labels map inlinedBlock)
            case RETURN(_)                      => JUMP(afterBlock)
            case LOAD_LOCAL(l) if isInlined(l)  => LOAD_LOCAL(inlinedLocals(l))
            case STORE_LOCAL(l) if isInlined(l) => STORE_LOCAL(inlinedLocals(l))
            case LOAD_LOCAL(l)                  => assertLocal(l)
            case STORE_LOCAL(l)                 => assertLocal(l)
            case SCOPE_ENTER(l) if isInlined(l) => SCOPE_ENTER(inlinedLocals(l))
            case SCOPE_EXIT(l) if isInlined(l)  => SCOPE_EXIT(inlinedLocals(l))

            case nw @ NEW(sym) =>
              val r = NEW(sym)
              pending(nw.init) = r
              r

            case CALL_METHOD(meth, Static(true)) if meth.isClassConstructor =>
              CALL_METHOD(meth, Static(true))

            case _ => i.clone()
          }
          // check any pending NEW's
          pending remove i foreach (_.init = newInstr.asInstanceOf[CALL_METHOD])
          newInstr
        }

        caller addLocals (inc.locals map dupLocal)
        caller addLocal inlinedThis

        if (retVal ne null)
          caller addLocal retVal

        inc.m foreachBlock { b =>
          inlinedBlock += (b -> newBlock())
          inlinedBlock(b).varsInScope ++= (b.varsInScope map inlinedLocals)
        }

        // re-emit the instructions before the call
        block.open
        block.clear
        block emit instrBefore

        // store the arguments into special locals
        inc.m.params.reverse foreach (p => blockEmit(STORE_LOCAL(inlinedLocals(p))))
        blockEmit(STORE_LOCAL(inlinedThis))

        // jump to the start block of the callee
        blockEmit(JUMP(inlinedBlock(inc.m.startBlock)))
        block.close

        // duplicate the other blocks in the callee
        val calleeLin = inc.m.linearizedBlocks()
        calleeLin foreach { bb =>
          var info = if(hasRETURN) (a in bb) else null
          def emitInlined(i: Instruction) = inlinedBlock(bb).emit(i, targetPos)
          def emitDrops(toDrop: Int)      = info.stack.types drop toDrop foreach (t => emitInlined(DROP(t)))

          for (i <- bb) {
            i match {
              case RETURN(UNIT) => emitDrops(0)
              case RETURN(kind) =>
                if (info.stack.length > 1) {
                  emitInlined(STORE_LOCAL(retVal))
                  emitDrops(1)
                  emitInlined(LOAD_LOCAL(retVal))
                }
              case _            => ()
            }
            emitInlined(map(i))
            info = if(hasRETURN) a.interpret(info, i) else null
          }
          inlinedBlock(bb).close
        }

        afterBlock emit instrAfter
        afterBlock.close

        staleIn        += afterBlock
        splicedBlocks ++= (calleeLin map inlinedBlock)

        // add exception handlers of the callee
        caller addHandlers (inc.handlers map translateExh)
        assert(pending.isEmpty, "Pending NEW elements: " + pending)
        if (settings.debug.value) icodes.checkValid(caller.m)
      }

      def isStampedForInlining(stackLength: Int) =
        !sameSymbols && inc.m.hasCode && shouldInline &&
        isSafeToInline(stackLength) // `isSafeToInline()` must be invoked last in this AND expr bc it mutates the `knownSafe` and `knownUnsafe` maps for good.

      def logFailure(stackLength: Int) = log(
        """|inline failed for %s:
           |  pair.sameSymbols: %s
           |  inc.numInlined < 2: %s
           |  inc.m.hasCode: %s
           |  isSafeToInline: %s
           |  shouldInline: %s
        """.stripMargin.format(
          inc.m, sameSymbols, inlinedMethodCount(inc.sym) < 2,
          inc.m.hasCode, isSafeToInline(stackLength), shouldInline
        )
      )

      def failureReason(stackLength: Int) =
        if (!inc.m.hasCode) "bytecode was unavailable"
        else if (inc.m.symbol.hasFlag(Flags.SYNCHRONIZED)) "method is synchronized"
        else if (!isSafeToInline(stackLength)) "it is unsafe (target may reference private fields)"
        else "of a bug (run with -Ylog:inline -Ydebug for more information)"

      def canAccess(level: NonPublicRefs.Value) = level match {
        case Private    => caller.owner == inc.owner
        case Protected  => caller.owner.tpe <:< inc.owner.tpe
        case Public     => true
      }
      private def sameSymbols = caller.sym == inc.sym
      private def sameOwner   = caller.owner == inc.owner

      /** A method is safe to inline when:
       *    - it does not contain calls to private methods when called from another class
       *    - it is not inlined into a position with non-empty stack,
       *      while having a top-level finalizer (see liftedTry problem)
       *    - it is not recursive
       * Note:
       *    - synthetic private members are made public in this pass.
       */
      def isSafeToInline(stackLength: Int): Boolean = {

        if(tfa.blackballed(inc.sym)) { return false }
        if(tfa.knownSafe(inc.sym))   { return true  }

        if(helperIsSafeToInline(stackLength)) {
          tfa.knownSafe += inc.sym;   true
        } else {
          tfa.knownUnsafe += inc.sym; false
        }
      }

      private def helperIsSafeToInline(stackLength: Int): Boolean = {
        def makePublic(f: Symbol): Boolean =
          /*
           * Completely disabling member publifying. This shouldn't have been done in the first place. :|
           */
          false
          // (inc.m.sourceFile ne NoSourceFile) && (f.isSynthetic || f.isParamAccessor) && {
          //   debuglog("Making not-private symbol out of synthetic: " + f)

          //   f setNotFlag Flags.PRIVATE
          //   true
          // }

        if (!inc.m.hasCode || inc.isRecursive)        { return false }
        if (inc.m.symbol.hasFlag(Flags.SYNCHRONIZED)) { return false }

        val accessNeeded = usesNonPublics.getOrElseUpdate(inc.m, {
          // Avoiding crashing the compiler if there are open blocks.
          inc.openBlocks foreach { b =>
            warn(inc.sym.pos,
                "Encountered open block in isSafeToInline: this indicates a bug in the optimizer!\n" +
                "  caller = " + caller.m + ", callee = " + inc.m
              )
            return false
          }
          def check(sym: Symbol, cond: Boolean) =
            if (cond) Private
            else if (sym.isProtected) Protected
            else Public

          def checkField(f: Symbol)   = check(f, f.isPrivate && !makePublic(f))
          def checkSuper(m: Symbol)   = check(m, m.isPrivate || !m.isClassConstructor)
          def checkMethod(m: Symbol)  = check(m, m.isPrivate)

          def getAccess(i: Instruction) = i match {
            case CALL_METHOD(m, SuperCall(_)) => checkSuper(m)
            case CALL_METHOD(m, _)            => checkMethod(m)
            case LOAD_FIELD(f, _)             => checkField(f)
            case STORE_FIELD(f, _)            => checkField(f)
            case _                            => Public
          }

          def iterate(): NonPublicRefs.Value = inc.instructions.foldLeft(Public)((res, inc) => getAccess(inc) match {
            case Private    => log("instruction " + inc + " requires private access.") ; return Private
            case Protected  => Protected
            case Public     => res
          })
          iterate()
        })

        canAccess(accessNeeded) && {
          val isIllegalStack = (stackLength > inc.minimumStack && inc.hasNonFinalizerHandler)

          !isIllegalStack || {
            debuglog("method " + inc.sym + " is used on a non-empty stack with finalizer.")
            false
          }
        }
      }

      /** Decide whether to inline or not. Heuristics:
       *   - it's bad to make the caller larger (> SMALL_METHOD_SIZE) if it was small
       *   - it's bad to inline large methods
       *   - it's good to inline higher order functions
       *   - it's good to inline closures functions.
       *   - it's bad (useless) to inline inside bridge methods
       */
      private def neverInline   = caller.isBridge || !inc.m.hasCode || inc.noinline
      private def alwaysInline  = inc.inline

      def shouldInline: Boolean = !neverInline && (alwaysInline || {
        debuglog("shouldInline: " + caller.m + " with " + inc.m)

        var score = 0

        // better not inline inside closures, but hope that the closure itself is repeatedly inlined
        if (caller.isInClosure)           score -= 2
        else if (caller.inlinedCalls < 1) score -= 1 // only monadic methods can trigger the first inline

        if (inc.isSmall) score += 1;
        if (inc.isLarge) score -= 1;
        if (caller.isSmall && isLargeSum) {
          score -= 1
          debuglog("shouldInline: score decreased to " + score + " because small " + caller + " would become large")
        }

        if (inc.isMonadic)          score += 3
        else if (inc.isHigherOrder) score += 1

        if (inc.isInClosure)                 score += 2;
        if (inlinedMethodCount(inc.sym) > 2) score -= 2;

        log("shouldInline(" + inc.m + ") score: " + score)

        score > 0
      })
    }

    def lookupIMethod(meth: Symbol, receiver: Symbol): Option[IMethod] = {
      def tryParent(sym: Symbol) = icodes icode sym flatMap (_ lookupMethod meth)

      (receiver.info.baseClasses.iterator map tryParent find (_.isDefined)).flatten
    }
  } /* class Inliner */
} /* class Inliners */
