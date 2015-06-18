/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Iulian Dragos
 */


package scala.tools.nsc
package backend.opt

import scala.collection.mutable
import scala.tools.nsc.symtab._
import scala.reflect.internal.util.NoSourceFile

/**
 * Inliner balances two competing goals:
 *   (a) aggressive inlining of:
 *       (a.1) the apply methods of anonymous closures, so that their anon-classes can be eliminated;
 *       (a.2) higher-order-methods defined in an external library, e.g. `Range.foreach()` among many others.
 *   (b) circumventing the barrier to inter-library inlining that private accesses in the callee impose.
 *
 * Summing up the discussion in SI-5442 and SI-5891,
 * the current implementation achieves to a large degree both goals above, and
 * overcomes a problem exhibited by previous versions:
 *
 *   (1) Problem: Attempting to access a private member `p` at runtime resulting in an `IllegalAccessError`,
 *                where `p` is defined in a library L, and is accessed from a library C (for Client),
 *                where C was compiled against L', an optimized version of L where the inliner made `p` public at the bytecode level.
 *                The only such members are fields, either synthetic or isParamAccessor, and thus having a dollar sign in their name
 *                (the accessibility of methods and constructors isn't touched by the inliner).
 *
 * Thus we add one more goal to our list:
 *   (c) Compile C (either optimized or not) against any of L or L',
 *       so that it runs with either L or L' (in particular, compile against L' and run with L).
 *
 * The chosen strategy is described in some detail in the comments for `accessRequirements()` and `potentiallyPublicized()`.
 * Documentation at http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/2011Q4/Inliner.pdf
 *
 *  @author Iulian Dragos
 */
abstract class Inliners extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions.{
    NullClass, NothingClass, ObjectClass,
    PredefModule, RuntimePackage, ScalaInlineClass, ScalaNoInlineClass,
    isFunctionType, isByNameParamType
  }

  val phaseName = "inliner"

  override val enabled: Boolean = settings.inline

  /** Debug - for timing the inliner. */
  /****
  private def timed[T](s: String, body: => T): T = {
    val t1 = System.currentTimeMillis()
    val res = body
    val t2 = System.currentTimeMillis()
    val ms = (t2 - t1).toInt
    if (ms >= MAX_INLINE_MILLIS)
      println("%s: %d milliseconds".format(s, ms))

    res
  }
  ****/

  /** Look up implementation of method 'sym in 'clazz'.
   */
  def lookupImplFor(sym: Symbol, clazz: Symbol): Symbol = {
    // TODO: verify that clazz.superClass is equivalent here to clazz.tpe.parents(0).typeSymbol (.tpe vs .info)
    def needsLookup = (
         (clazz != NoSymbol)
      && (clazz != sym.owner)
      && !sym.isEffectivelyFinalOrNotOverridden
      && clazz.isEffectivelyFinalOrNotOverridden
    )
    def lookup(clazz: Symbol): Symbol = {
      // println("\t\tlooking up " + meth + " in " + clazz.fullName + " meth.owner = " + meth.owner)
      assert(clazz != NoSymbol, "Walked up past Object.superClass looking for " + sym +
                                ", most likely this reveals the TFA at fault (receiver and callee don't match).")
      if (sym.owner == clazz || isBottomType(clazz)) sym
      else sym.overridingSymbol(clazz) orElse (
        if (sym.owner.isTrait) sym
        else lookup(clazz.superClass)
      )
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

    object iclassOrdering extends Ordering[IClass] {
      def compare(a: IClass, b: IClass) = {
        val sourceNamesComparison = (a.cunit.toString() compare b.cunit.toString())
        if(sourceNamesComparison != 0) sourceNamesComparison
        else {
          val namesComparison = (a.toString() compare b.toString())
          if(namesComparison != 0) namesComparison
          else {
            a.symbol.id compare b.symbol.id
          }
        }
      }
    }
    val queue = new mutable.PriorityQueue[IClass]()(iclassOrdering)

    override def apply(c: IClass) { queue += c }

    override def run() {
      knownLacksInline.clear()
      knownHasInline.clear()
      try {
        super.run()
        for(c <- queue) { inliner analyzeClass c }
      } finally {
        inliner.clearCaches()
        knownLacksInline.clear()
        knownHasInline.clear()
      }
    }
  }

  def isBottomType(sym: Symbol) = sym == NullClass || sym == NothingClass

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

  val knownLacksInline = mutable.Set.empty[Symbol] // cache to avoid multiple inliner.hasInline() calls.
  val knownHasInline   = mutable.Set.empty[Symbol] // as above. Motivated by the need to warn on "inliner failures".

  def hasInline(sym: Symbol)    = {
    if     (knownLacksInline(sym)) false
    else if(knownHasInline(sym))   true
    else {
      val b = (sym hasAnnotation ScalaInlineClass)
      if(b) { knownHasInline   += sym }
      else  { knownLacksInline += sym }

      b
    }
  }

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
    private def warn(pos: Position, msg: String) = currentRun.reporting.inlinerWarning(pos, msg)

    private def ownedName(sym: Symbol): String = exitingUncurry {
      val count = (
        if (!sym.isMethod) 1
        else if (sym.owner.isAnonymousFunction) 3
        else 2
      )
      (sym.ownerChain take count filterNot (_.isPackageClass)).reverseMap(_.nameString).mkString(".")
    }
    private def inlineLog(what: String, main: => String, comment: => String) {
      def cstr = comment match {
        case ""   => ""
        case str  => " // " + str
      }
      val width = if (currentIClazz eq null) 40 else currentIClazz.symbol.enclosingPackage.fullName.length + 25
      val fmt = "%8s  %-" + width + "s" + cstr
      log(fmt.format(what, main))
    }
    private def inlineLog(what: String, main: Symbol, comment: => String) {
      inlineLog(what, ownedName(main), comment)
    }

    val recentTFAs = mutable.Map.empty[Symbol, Tuple2[Boolean, analysis.MethodTFA]]

    private def getRecentTFA(incm: IMethod, forceable: Boolean): (Boolean, analysis.MethodTFA) = {

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
      if(hasRETURN) { a = new analysis.MethodTFA(incm); a.run() }

      if(forceable) { recentTFAs.put(incm.symbol, (hasRETURN, a)) }

      (hasRETURN, a)
    }

    def clearCaches() {
      // methods
      NonPublicRefs.usesNonPublics.clear()
      recentTFAs.clear()
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

    object imethodOrdering extends Ordering[IMethod] {
      def compare(a: IMethod, b: IMethod) = {
        val namesComparison = (a.toString() compare b.toString())
        if(namesComparison != 0) namesComparison
        else {
          a.symbol.id compare b.symbol.id
        }
      }
    }

    def analyzeClass(cls: IClass): Unit =
      if (settings.inline) {
        inlineLog("class", s"${cls.symbol.decodedName}", s"analyzing ${cls.methods.size} methods in $cls")

        this.currentIClazz = cls
        val ms = cls.methods sorted imethodOrdering
        ms foreach { im =>
          if (hasInline(im.symbol)) {
            inlineLog("skip", im.symbol, "no inlining into @inline methods")
          }
          else if(im.hasCode && !im.symbol.isBridge) {
            analyzeMethod(im)
          }
        }
      }

    val tfa   = new analysis.MTFAGrowable()
    tfa.stat  = global.settings.YstatisticsEnabled
    val staleOut      = new mutable.ListBuffer[BasicBlock]
    val splicedBlocks = mutable.Set.empty[BasicBlock]
    val staleIn       = mutable.Set.empty[BasicBlock]

    /**
     * A transformation local to the body of the IMethod received as argument.
     * An inlining decision consists in replacing a callsite with the body of the callee.
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
     *        a substitution we call "successful inlining").
     *
     *   (3) following iterations have `relevantBBs` updated to focus on the inlined basic blocks and their successors only.
     *       Details in `MTFAGrowable.reinit()`
     * */
    def analyzeMethod(m: IMethod): Unit = {
      // m.normalize
      if (settings.debug)
        inlineLog("caller", ownedName(m.symbol), "in " + m.symbol.owner.fullName)

      val sizeBeforeInlining  = m.code.blockCount
      val instrBeforeInlining = m.code.instructionCount
      var retry = false
      var count = 0

      // fresh name counter
      val fresh = mutable.HashMap.empty[String, Int] withDefaultValue 0
      // how many times have we already inlined this method here?
      val inlinedMethodCount = mutable.HashMap.empty[Symbol, Int] withDefaultValue 0
      val caller = new IMethodInfo(m)
      def analyzeMessage = s"Analyzing ${caller.length} blocks of $m for inlining sites."

      def preInline(isFirstRound: Boolean): Int = {
        val inputBlocks = caller.m.linearizedBlocks()
        val callsites: Function1[BasicBlock, List[opcodes.CALL_METHOD]] = {
          if(isFirstRound) tfa.conclusives else tfa.knownBeforehand
        }
        inlineWithoutTFA(inputBlocks, callsites)
      }

      /*
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
              assert(ocm.method.isEffectivelyFinalOrNotOverridden && ocm.method.owner.isEffectivelyFinalOrNotOverridden)
              if(analyzeInc(ocm, x, ocm.method.owner, -1, ocm.method)) {
                inlineCount += 1
                break()
              }
            }
          }
        }

        inlineCount
      }

      /*
       *  Decides whether it's feasible and desirable to inline the body of the method given by `concreteMethod`
       *  at the program point given by `i` (a callsite). The boolean result indicates whether inlining was performed.
       *
       */
      def analyzeInc(i: CALL_METHOD, bb: BasicBlock, receiver: Symbol, stackLength: Int, concreteMethod: Symbol): Boolean = {
        assert(bb.toList contains i, "Candidate callsite does not belong to BasicBlock.")
        val shouldWarn = hasInline(i.method)

        def warnNoInline(reason: String): Boolean = {
          def msg = "Could not inline required method %s because %s.".format(i.method.unexpandedName.decode, reason)
          if (settings.debug)
            inlineLog("fail", i.method.fullName, reason)
          if (shouldWarn)
            warn(i.pos, msg)

          false
        }

        var isAvailable = icodes available concreteMethod.enclClass

        if (!isAvailable && shouldLoadImplFor(concreteMethod, receiver)) {
          // Until r22824 this line was:
          //   icodes.icode(concreteMethod.enclClass, true)
          //
          // Changing it to
          //   icodes.load(concreteMethod.enclClass)
          // was the proximate cause for SI-3882:
          //   error: Illegal index: 0 overlaps List((variable par1,LONG))
          //   error: Illegal index: 0 overlaps List((variable par1,LONG))
          isAvailable = icodes.load(concreteMethod.enclClass)
        }

        def isCandidate = (
             isClosureClass(receiver)
          || concreteMethod.isEffectivelyFinalOrNotOverridden
          || receiver.isEffectivelyFinalOrNotOverridden
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
              + "\n\tconcreteMethod.isEffectivelyFinalOrNotOverridden: " + concreteMethod.isEffectivelyFinalOrNotOverridden)

        if (!isCandidate) warnNoInline("it can be overridden")
        else if (!isAvailable) warnNoInline("bytecode unavailable")
        else lookupIMethod(concreteMethod, receiver) filter (callee => callee.hasCode || warnNoInline("callee has no code")) exists { callee =>
          val inc   = new IMethodInfo(callee)
          val pair  = new CallerCalleeInfo(caller, inc, fresh, inlinedMethodCount)

          if (inc.hasHandlers && (stackLength == -1)) {
            // no inlining is done, yet don't warn about it, stackLength == -1 indicates we're trying to inlineWithoutTFA.
            // Shortly, a TFA will be computed and an error message reported if indeed inlining not possible.
            false
          }
          else {
            val isSafe = pair isStampedForInlining stackLength match {
              case DontInlineHere(msg)                       => warnNoInline(msg)
              case NeverSafeToInline                         => false
              case InlineableAtThisCaller                    => true
              case FeasibleInline(required, toPublicize)     =>
                for (f <- toPublicize) {
                  inlineLog("access", f, "making public")
                  f setFlag Flags.notPRIVATE
                  f setFlag Flags.notPROTECTED
                }
                // only add to `knownSafe` after all `toPublicize` fields actually made public.
                if (required == NonPublicRefs.Public)
                  tfa.knownSafe += inc.sym

                true
            }
            isSafe && {
               retry   = true
               if (isCountable) count += 1
               pair.doInline(bb, i)
               if (!pair.isInlineForced || inc.isMonadic) caller.inlinedCalls += 1
               inlinedMethodCount(inc.sym) += 1

               // Remove the caller from the cache (this inlining might have changed its calls-private relation).
               usesNonPublics -= m
               recentTFAs     -= m.symbol
               true
            }
          }
        }
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
      /*val totalPreInlines = */ { // Val name commented out to emphasize it is never used
        val firstRound = preInline(isFirstRound = true)
        if(firstRound == 0) 0 else (firstRound + preInline(isFirstRound = false))
      }
      staleOut.clear()
      splicedBlocks.clear()
      staleIn.clear()

      do {
        retry = false
        debuglog(analyzeMessage)

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
                break()
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

      for(inlFail <- tfa.warnIfInlineFails) {
        warn(inlFail.pos, "At the end of the day, could not inline @inline-marked method " + inlFail.method.unexpandedName.decode)
      }

      m.normalize()
      if (sizeBeforeInlining > 0) {
        val instrAfterInlining = m.code.instructionCount
        val inlinings = caller.inlinedCalls
        if (inlinings > 0) {
          val s1      = s"instructions $instrBeforeInlining -> $instrAfterInlining"
          val s2      = if (sizeBeforeInlining == m.code.blockCount) "" else s", blocks $sizeBeforeInlining -> ${m.code.blockCount}"
          val callees = inlinedMethodCount.toList map { case (k, v) => k.fullNameString + ( if (v == 1) "" else "/" + v ) }

          inlineLog("inlined", m.symbol.fullName, callees.sorted.mkString(inlinings + " inlined: ", ", ", ""))
          inlineLog("<<tldr>>", m.symbol.fullName, s"${m.symbol.nameString}: $s1$s2")
        }
      }
    }

    private def isHigherOrderMethod(sym: Symbol) = (
         sym.isMethod
      && enteringExplicitOuter(sym.info.paramTypes exists isFunctionType) // was "at erasurePhase.prev"
    )

    /** Should method 'sym' being called in 'receiver' be loaded from disk? */
    def shouldLoadImplFor(sym: Symbol, receiver: Symbol): Boolean = {
      def alwaysLoad    = (receiver.enclosingPackage == RuntimePackage) || (receiver == PredefModule.moduleClass)
      def loadCondition = sym.isEffectivelyFinalOrNotOverridden && isMonadicMethod(sym) && isHigherOrderMethod(sym)

      val res = hasInline(sym) || alwaysLoad || loadCondition
      debuglog("shouldLoadImplFor: " + receiver + "." + sym + ": " + res)
      res
    }

    class IMethodInfo(val m: IMethod) {
      override def toString = m.toString

      val sym           = m.symbol
      def owner         = sym.owner
      def paramTypes    = sym.info.paramTypes
      def minimumStack  = paramTypes.length + 1

      def isBridge      = sym.isBridge
      val isInClosure   = isClosureClass(owner)
      val isHigherOrder = isHigherOrderMethod(sym)
      def isMonadic     = isMonadicMethod(sym)

      def handlers      = m.exh
      def blocks        = m.blocks
      def locals        = m.locals
      def length        = blocks.length
      def openBlocks    = blocks filterNot (_.closed)
      def instructions  = m.code.instructions

      def isSmall         = (length <= SMALL_METHOD_SIZE) && blocks(0).length < 10
      def isLarge         = length > MAX_INLINE_SIZE
      def isRecursive     = m.recursive
      def hasHandlers     = handlers.nonEmpty || m.bytecodeHasEHs

      def isSynchronized         = sym.hasFlag(Flags.SYNCHRONIZED)
      def hasNonFinalizerHandler = handlers exists {
        case _: Finalizer => true
        case _            => false
      }

      // the number of inlined calls in 'm', used by 'isScoreOK'
      var inlinedCalls = 0

      def addLocals(ls: List[Local])  = m.locals ++= ls
      def addLocal(l: Local)          = addLocals(List(l))
      def addHandlers(exhs: List[ExceptionHandler]) = m.exh = exhs ::: m.exh

      /**
       * This method inspects the callee's instructions, finding out the most restrictive accessibility implied by them.
       *
       * Rather than giving up upon encountering an access to a private field `p`, it provisorily admits `p` as "can-be-made-public", provided:
       *   - `p` is being compiled as part of this compilation run, and
       *   - `p` is synthetic or param-accessor.
       *
       * This method is side-effect free, in particular it lets the invoker decide
       * whether the accessibility of the `toBecomePublic` fields should be changed or not.
       */
      def accessRequirements: AccessReq = {

        var toBecomePublic: List[Symbol] = Nil

        def check(sym: Symbol, cond: Boolean) =
          if (cond) Private
          else if (sym.isProtected) Protected
          else Public

        def canMakePublic(f: Symbol): Boolean =
          (m.sourceFile ne NoSourceFile) &&
          (f.isSynthetic || f.isParamAccessor) &&
          { toBecomePublic = f :: toBecomePublic; true }

        /* A safety check to consider as private, for the purposes of inlining, a public field that:
         *   (1) is defined in an external library, and
         *   (2) can be presumed synthetic (due to a dollar sign in its name).
         * Such field was made public by `doMakePublic()` and we don't want to rely on that,
         * because under other compilation conditions (ie no -optimize) that won't be the case anymore.
         *
         * This allows aggressive intra-library inlining (making public if needed)
         * that does not break inter-library scenarios (see comment for `Inliners`).
         *
         * TODO handle more robustly the case of a trait var changed at the source-level from public to private[this]
         *      (eg by having ICodeReader use unpickler, see SI-5442).

         DISABLED

        def potentiallyPublicized(f: Symbol): Boolean = {
          (m.sourceFile eq NoSourceFile) && f.name.containsChar('$')
        }
        */


        def isPrivateForInlining(sym: Symbol): Boolean = {
          if (sym.isJavaDefined) {
            def check(sym: Symbol) = !(sym.isPublic || sym.isProtected)
            check(sym) || check(sym.owner) // SI-7582 Must check the enclosing class *and* the symbol for Java.
          }
          else sym.isPrivate // Scala never emits package-private bytecode
        }

        def checkField(f: Symbol)   = check(f, isPrivateForInlining(f) && !canMakePublic(f))
        def checkSuper(n: Symbol)   = check(n, isPrivateForInlining(n) || !n.isClassConstructor)
        def checkMethod(n: Symbol)  = check(n, isPrivateForInlining(n))

        def getAccess(i: Instruction) = i match {
          case CALL_METHOD(n, SuperCall(_)) => checkSuper(n)
          case CALL_METHOD(n, _)            => checkMethod(n)
          case LOAD_FIELD(f, _)             => checkField(f)
          case STORE_FIELD(f, _)            => checkField(f)
          case _                            => Public
        }

        var seen = Public
        val iter = instructions.iterator
        while((seen ne Private) && iter.hasNext) {
          val i = iter.next()
          getAccess(i) match {
            case Private    =>
              inlineLog("access", s"instruction $i requires private access", "pos=" + i.pos)
              toBecomePublic = Nil
              seen = Private
            case Protected  => seen = Protected
            case _          => ()
          }
        }

        AccessReq(seen, toBecomePublic)
      }

    }

    /**
     * Classifies a pair (caller, callee) into one of four categories:
     *
     *   (a) inlining should be performed, classified in turn into:
     *       (a.1) `InlineableAtThisCaller`: unconditionally at this caller
     *       (a.2) `FeasibleInline`: it only remains for certain access requirements to be met (see `IMethodInfo.accessRequirements()`)
     *
     *   (b) inlining shouldn't be performed, classified in turn into:
     *       (b.1) `DontInlineHere`: indicates that this particular occurrence of the callee at the caller shouldn't be inlined.
     *                - Nothing is said about the outcome for other callers, or for other occurrences of the callee for the same caller.
     *                - In particular inlining might be possible, but heuristics gave a low score for it.
     *       (b.2) `NeverSafeToInline`: the callee can't be inlined anywhere, irrespective of caller.
     *
     * The classification above is computed by `isStampedForInlining()` based on which `analyzeInc()` goes on to:
     *   - either log the reason for failure --- case (b) ---,
     *   - or perform inlining --- case (a) ---.
     */
    sealed abstract class InlineSafetyInfo
    case object NeverSafeToInline           extends InlineSafetyInfo
    case object InlineableAtThisCaller      extends InlineSafetyInfo
    case class  DontInlineHere(msg: String) extends InlineSafetyInfo
    case class  FeasibleInline(accessNeeded: NonPublicRefs.Value, toBecomePublic: List[Symbol]) extends InlineSafetyInfo

    case class AccessReq(
      accessNeeded:   NonPublicRefs.Value,
      toBecomePublic: List[Symbol]
    )

    final class CallerCalleeInfo(val caller: IMethodInfo, val inc: IMethodInfo, fresh: mutable.Map[String, Int], inlinedMethodCount: scala.collection.Map[Symbol, Int]) {

      assert(!caller.isBridge && inc.m.hasCode,
             "A guard in Inliner.analyzeClass() should have prevented from getting here.")

      def isLargeSum  = caller.length + inc.length - 1 > SMALL_METHOD_SIZE

      private def freshName(s: String): TermName = {
        fresh(s) += 1
        newTermName(s + fresh(s))
      }

      private def isKnownToInlineSafely: Boolean = { tfa.knownSafe(inc.sym) }

      val isInlineForced    = hasInline(inc.sym)
      val isInlineForbidden = hasNoInline(inc.sym)
      assert(!(isInlineForced && isInlineForbidden), "method ("+inc.m+") marked both @inline and @noinline.")

      /** Inline 'inc' into 'caller' at the given block and instruction.
       *  The instruction must be a CALL_METHOD.
       */
      def doInline(block: BasicBlock, instr: CALL_METHOD) {

        staleOut += block

        tfa.remainingCALLs.remove(instr) // this bookkeeping is done here and not in MTFAGrowable.reinit due to (1st) convenience and (2nd) necessity.
        tfa.isOnWatchlist.remove(instr)  // ditto
        tfa.warnIfInlineFails.remove(instr)

        val targetPos = instr.pos

        def blockEmit(i: Instruction) = block.emit(i, targetPos)
        def newLocal(baseName: String, kind: TypeKind) =
          new Local(caller.sym.newVariable(freshName(baseName), targetPos) setInfo kind.toType, kind, false)

        val (hasRETURN, a) = getRecentTFA(inc.m, isInlineForced)

        /* The exception handlers that are active at the current block. */
        val activeHandlers = caller.handlers filter (_ covered block)

        /* Map 'original' blocks to the ones inlined in the caller. */
        val inlinedBlock = mutable.Map[BasicBlock, BasicBlock]()

        val varsInScope = mutable.HashSet[Local]() ++= block.varsInScope

        /* Side effects varsInScope when it sees SCOPE_ENTERs. */
        def instrBeforeFilter(i: Instruction): Boolean = {
          i match { case SCOPE_ENTER(l) => varsInScope += l ; case _ => () }
          i ne instr
        }
        val instrBefore = block.toList takeWhile instrBeforeFilter
        val instrAfter  = block.toList drop (instrBefore.length + 1)

        assert(!instrAfter.isEmpty, "CALL_METHOD cannot be the last instruction in block!")

        // store the '$this' into the special local
        val inlinedThis = newLocal("$inlThis", REFERENCE(ObjectClass))

        /* buffer for the returned value */
        val retVal = inc.m.returnType match {
          case UNIT  => null
          case x     => newLocal("$retVal", x)
        }

        val inlinedLocals = mutable.HashMap.empty[Local, Local]

        /* Add a new block in the current context. */
        def newBlock() = {
          val b = caller.m.code.newBlock()
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

        /* alfa-rename `l` in caller's context. */
        def dupLocal(l: Local): Local = {
          val sym = caller.sym.newVariable(freshName(l.sym.name.toString), l.sym.pos)
          // sym.setInfo(l.sym.tpe)
          val dupped = new Local(sym, l.kind, false)
          inlinedLocals(l) = dupped
          dupped
        }

        val afterBlock = newBlock()

        /* Map from nw.init instructions to their matching NEW call */
        val pending: mutable.Map[Instruction, NEW] = new mutable.HashMap

        /* Map an instruction from the callee to one suitable for the caller. */
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
              CALL_METHOD(meth, Static(onInstance = true))

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
        block.open()
        block.clear()
        block emit instrBefore

        // store the arguments into special locals
        inc.m.params.reverse foreach (p => blockEmit(STORE_LOCAL(inlinedLocals(p))))
        blockEmit(STORE_LOCAL(inlinedThis))

        // jump to the start block of the callee
        blockEmit(JUMP(inlinedBlock(inc.m.startBlock)))
        block.close()

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
          inlinedBlock(bb).close()
        }

        afterBlock emit instrAfter
        afterBlock.close()

        staleIn        += afterBlock
        splicedBlocks ++= (calleeLin map inlinedBlock)

        // add exception handlers of the callee
        caller addHandlers (inc.handlers map translateExh)
        assert(pending.isEmpty, "Pending NEW elements: " + pending)
        if (settings.debug) icodes.checkValid(caller.m)
      }

      def isStampedForInlining(stackLength: Int): InlineSafetyInfo = {

        if(tfa.blackballed(inc.sym)) { return NeverSafeToInline }

        if(!isKnownToInlineSafely) {

          if(inc.openBlocks.nonEmpty) {
            val msg = ("Encountered " + inc.openBlocks.size + " open block(s) in isSafeToInline: this indicates a bug in the optimizer!\n" +
                       "  caller = " + caller.m + ", callee = " + inc.m)
            warn(inc.sym.pos, msg)
            tfa.knownNever += inc.sym
            return DontInlineHere("Open blocks in " + inc.m)
          }

          val reasonWhyNever: String = {
            var rs: List[String] = Nil
            if(inc.isRecursive)    { rs ::= "is recursive"           }
            if(isInlineForbidden)  { rs ::= "is annotated @noinline" }
            if(inc.isSynchronized) { rs ::= "is synchronized method" }
            if(inc.m.bytecodeHasEHs) { rs ::= "bytecode contains exception handlers / finally clause" } // SI-6188
            if(inc.m.bytecodeHasInvokeDynamic) { rs ::= "bytecode contains invoke dynamic" }
            if(rs.isEmpty) null else rs.mkString("", ", and ", "")
          }

          if(reasonWhyNever != null) {
            tfa.knownNever += inc.sym
            inlineLog("never", inc.sym, reasonWhyNever)
            // next time around NeverSafeToInline is returned, thus skipping (duplicate) msg, this is intended.
            return DontInlineHere(inc.m + " " + reasonWhyNever)
          }

          if(sameSymbols) { // TODO but this also amounts to recursive, ie should lead to adding to tfa.knownNever, right?
            tfa.knownUnsafe += inc.sym
            return DontInlineHere("sameSymbols (ie caller == callee)")
          }

        }

        /*
         * From here on, two main categories of checks remain, (a) and (b) below:
         *   (a.1) either the scoring heuristics give green light; or
         *   (a.2) forced as candidate due to @inline.
         * After that, safety proper is checked:
         *   (b.1) the callee does not contain calls to private methods when called from another class
         *   (b.2) the callee is not going to be inlined into a position with non-empty stack,
         *         while having a top-level finalizer (see liftedTry problem)
         * As a result of (b), some synthetic private members can be chosen to become public.
         */

        val score    = inlinerScore
        val scoreStr = if (score > 0) "+" + score else "" + score
        val what     = if (score > 0) "ok to" else "don't"
        inlineLog(scoreStr, inc.m.symbol, s"$what inline into ${ownedName(caller.m.symbol)}")

        if (!isInlineForced && score <= 0) {
          // During inlining retry, a previous caller-callee pair that scored low may pass.
          // Thus, adding the callee to tfa.knownUnsafe isn't warranted.
          return DontInlineHere(s"inliner heuristic")
        }

        if(inc.hasHandlers && (stackLength > inc.minimumStack)) {
          return DontInlineHere("callee contains exception handlers / finally clause, and is invoked with non-empty operand stack") // SI-6157
        }

        if(isKnownToInlineSafely) { return InlineableAtThisCaller }

        if(stackLength > inc.minimumStack && inc.hasNonFinalizerHandler) {
          val msg = "method " + inc.sym + " is used on a non-empty stack with finalizer."
          debuglog(msg)
          // FYI: not reason enough to add inc.sym to tfa.knownUnsafe (because at other callsite in this caller, inlining might be ok)
          return DontInlineHere(msg)
        }

        val accReq = inc.accessRequirements
        if(!canAccess(accReq.accessNeeded)) {
          tfa.knownUnsafe += inc.sym
          val msg = "access level required by callee not matched by caller"
          inlineLog("fail", inc.sym, msg)
          return DontInlineHere(msg)
        }

        FeasibleInline(accReq.accessNeeded, accReq.toBecomePublic)

      }

      def canAccess(level: NonPublicRefs.Value) = level match {
        case Private    => caller.owner == inc.owner
        case Protected  => caller.owner.tpe <:< inc.owner.tpe
        case Public     => true
      }
      private def sameSymbols = caller.sym == inc.sym

      /** Gives green light for inlining (which may still be vetoed later). Heuristics:
       *   - it's bad to make the caller larger (> SMALL_METHOD_SIZE) if it was small
       *   - it's bad to inline large methods
       *   - it's good to inline higher order functions
       *   - it's good to inline closures functions.
       *   - it's bad (useless) to inline inside bridge methods
       */
      def inlinerScore: Int = {
        var score = 0

        // better not inline inside closures, but hope that the closure itself is repeatedly inlined
        if (caller.isInClosure)           score -= 2
        else if (caller.inlinedCalls < 1) score -= 1 // only monadic methods can trigger the first inline

        if (inc.isSmall) score += 1
        // if (inc.hasClosureParam) score += 2
        if (inc.isLarge) score -= 1
        if (caller.isSmall && isLargeSum) {
          score -= 1
          debuglog(s"inliner score decreased to $score because small caller $caller would become large")
        }

        if (inc.isMonadic)          score += 3
        else if (inc.isHigherOrder) score += 1

        if (inc.isInClosure)                 score += 2
        if (inlinedMethodCount(inc.sym) > 2) score -= 2
        score
      }
    }

    def lookupIMethod(meth: Symbol, receiver: Symbol): Option[IMethod] = {
      def tryParent(sym: Symbol) = icodes icode sym flatMap (_ lookupMethod meth)

      (receiver.info.baseClasses.iterator map tryParent find (_.isDefined)).flatten
    }
  } /* class Inliner */
} /* class Inliners */
