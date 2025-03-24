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

package scala
package tools.nsc
package backend
package jvm

import scala.collection.immutable
import scala.tools.asm

/*
 *
 *  @author  Miguel Garcia, https://lampwww.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *
 */
abstract class BCodeSyncAndTry extends BCodeBodyBuilder {
  import global._
  import bTypes._
  import coreBTypes._

  /*
   * Functionality to lower `synchronized` and `try` expressions.
   */
  class SyncAndTryBuilder(cunit: CompilationUnit) extends PlainBodyBuilder(cunit) {

    def genSynchronized(tree: Apply, expectedType: BType): BType = {
      val Apply(fun, args) = tree
      val monitor = locals.makeLocal(ObjectRef, "monitor")
      val monCleanup = new asm.Label

      // if the synchronized block returns a result, store it in a local variable.
      // Just leaving it on the stack is not valid in MSIL (stack is cleaned when leaving try-blocks).
      val hasResult = (expectedType != UNIT)
      val monitorResult: Symbol = if (hasResult) locals.makeLocal(tpeTK(args.head), "monitorResult") else null

      /* ------ (1) pushing and entering the monitor, also keeping a reference to it in a local var. ------ */
      genLoadQualifier(fun)
      bc dup ObjectRef
      locals.store(monitor)
      emit(asm.Opcodes.MONITORENTER)

      /* ------ (2) Synchronized block.
       *            Reached by fall-through from (1).
       *            Protected by:
       *            (2.a) the EH-version of the monitor-exit, and
       *            (2.b) whatever protects the whole synchronized expression.
       * ------
       */
      val startProtected = currProgramPoint()
      registerCleanup(monCleanup)
      genLoad(args.head, expectedType /* toTypeKind(tree.tpe.resultType) */)
      unregisterCleanup(monCleanup)
      if (hasResult) { locals.store(monitorResult) }
      nopIfNeeded(startProtected)
      val endProtected = currProgramPoint()

      /* ------ (3) monitor-exit after normal, non-early-return, termination of (2).
       *            Reached by fall-through from (2).
       *            Protected by whatever protects the whole synchronized expression.
       * ------
       */
      locals.load(monitor)
      emit(asm.Opcodes.MONITOREXIT)
      if (hasResult) { locals.load(monitorResult) }
      val postHandler = new asm.Label
      bc goTo postHandler

      /* ------ (4) exception-handler version of monitor-exit code.
       *            Reached upon abrupt termination of (2).
       *            Protected by whatever protects the whole synchronized expression.
       *            null => "any" exception in bytecode, like we emit for finally.
       *            Important not to use j/l/Throwable which dooms the method to a life of interpretation! (scala/scala-dev#233)
       * ------
       */
      protect(startProtected, endProtected, currProgramPoint(), null)
      locals.load(monitor)
      emit(asm.Opcodes.MONITOREXIT)
      emit(asm.Opcodes.ATHROW)

      /* ------ (5) cleanup version of monitor-exit code.
       *            Reached upon early-return from (2).
       *            Protected by whatever protects the whole synchronized expression.
       * ------
       */
      if (shouldEmitCleanup) {
        markProgramPoint(monCleanup)
        locals.load(monitor)
        emit(asm.Opcodes.MONITOREXIT)
        pendingCleanups()
      }

      /* ------ (6) normal exit of the synchronized expression.
       *            Reached after normal, non-early-return, termination of (3).
       *            Protected by whatever protects the whole synchronized expression.
       * ------
       */
      mnode visitLabel postHandler

      lineNumber(tree)

      expectedType
    }

    /*
     *  Detects whether no instructions have been emitted since label `lbl` and if so emits a NOP.
     *  Useful to avoid emitting an empty try-block being protected by exception handlers,
     *  which results in "java.lang.ClassFormatError: Illegal exception table range". See scala/bug#6102.
     */
    def nopIfNeeded(lbl: asm.Label): Unit = {
      val noInstructionEmitted = isAtProgramPoint(lbl)
      if (noInstructionEmitted) { emit(asm.Opcodes.NOP) }
    }

    /*
     *  Emitting try-catch is easy, emitting try-catch-finally not quite so.
     *  A finally-block (which always has type Unit, thus leaving the operand stack unchanged)
     *  affects control-transfer from protected regions, as follows:
     *
     *    (a) `return` statement:
     *
     *        First, the value to return (if any) is evaluated.
     *        Afterwards, all enclosing finally-blocks are run, from innermost to outermost.
     *        Only then is the return value (if any) returned.
     *
     *        Some terminology:
     *          (a.1) Executing a return statement that is protected
     *                by one or more finally-blocks is called "early return"
     *          (a.2) the chain of code sections (a code section for each enclosing finally-block)
     *                to run upon early returns is called "cleanup chain"
     *
     *        As an additional spin, consider a return statement in a finally-block.
     *        In this case, the value to return depends on how control arrived at that statement:
     *        in case it arrived via a previous return, the previous return enjoys priority:
     *        the value to return is given by that statement.
     *
     *    (b) A finally-block protects both the try-clause and the catch-clauses.
     *
     *           Sidenote:
     *             A try-clause may contain an empty block. On CLR, a finally-block has special semantics
     *             regarding Abort interruptions; but on the JVM it's safe to elide an exception-handler
     *             that protects an "empty" range ("empty" as in "containing NOPs only",
     *             see `asm.optimiz.DanglingExcHandlers` and scala/bug#6720).
     *
     *        This means a finally-block indicates instructions that can be reached:
     *          (b.1) Upon normal (non-early-returning) completion of the try-clause or a catch-clause
     *                In this case, the next-program-point is that following the try-catch-finally expression.
     *          (b.2) Upon early-return initiated in the try-clause or a catch-clause
     *                In this case, the next-program-point is the enclosing cleanup section (if any), otherwise return.
     *          (b.3) Upon abrupt termination (due to unhandled exception) of the try-clause or a catch-clause
     *                In this case, the unhandled exception must be re-thrown after running the finally-block.
     *
     *    (c) finally-blocks are implicit to `synchronized` (a finally-block is added to just release the lock)
     *        that's why `genSynchronized()` too emits cleanup-sections.
     *
     *  A number of code patterns can be emitted to realize the intended semantics.
     *
     *  A popular alternative (GenICode, javac) consists in duplicating the cleanup-chain at each early-return position.
     *  The principle at work being that once control is transferred to a cleanup-section,
     *  control will always stay within the cleanup-chain.
     *  That is, barring an exception being thrown in a cleanup-section, in which case the enclosing try-block
     *  (reached via abrupt termination) takes over.
     *
     *  The observations above hint at another code layout, less verbose, for the cleanup-chain.
     *
     *  The code layout that GenBCode emits takes into account that once a cleanup section has been reached,
     *  jumping to the next cleanup-section (and so on, until the outermost one) realizes the correct semantics.
     *
     *  There is still code duplication in that two cleanup-chains are needed (but this is unavoidable, anyway):
     *  one for normal control flow and another chain consisting of exception handlers.
     *  The in-line comments below refer to them as
     *    - "early-return-cleanups" and
     *    - "exception-handler-version-of-finally-block" respectively.
     *
     */
    def genLoadTry(tree: Try): BType = {

      val Try(block, catches, finalizer) = tree
      val kind = tpeTK(tree)

      val caseHandlers: List[EHClause] =
        for (CaseDef(pat, _, caseBody) <- catches) yield {
          pat match {
            case Typed(Ident(nme.WILDCARD), tpt)  => NamelessEH(tpeTK(tpt).asClassBType, caseBody)
            case Ident(nme.WILDCARD)              => NamelessEH(jlThrowableRef,  caseBody)
            case Bind(_, _)                       => BoundEH   (pat.symbol, caseBody)
            case _                                => throw new Exception(s"Unexpected try case pattern tree $pat of class ${pat.shortClass}")
          }
        }

      // ------ (0) locals used later ------

      /*
       * `postHandlers` is a program point denoting:
       *     (a) the finally-clause conceptually reached via fall-through from try-catch-finally
       *         (in case a finally-block is present); or
       *     (b) the program point right after the try-catch
       *         (in case there's no finally-block).
       * The name choice emphasizes that the code section lies "after all exception handlers",
       * where "all exception handlers" includes those derived from catch-clauses as well as from finally-blocks.
       */
      val postHandlers = new asm.Label

      val hasFinally   = (finalizer != EmptyTree)

      /*
       * used in the finally-clause reached via fall-through from try-catch, if any.
       */
      val guardResult  = hasFinally && (kind != UNIT) && mayCleanStack(finalizer)

      /*
       * please notice `tmp` has type tree.tpe, while `earlyReturnVar` has the method return type.
       * Because those two types can be different, dedicated vars are needed.
       */
      val tmp          = if (guardResult) locals.makeLocal(tpeTK(tree), "tmp") else null

      /*
       * upon early return from the try-body or one of its EHs (but not the EH-version of the finally-clause)
       * AND hasFinally, a cleanup is needed.
       */
      val finCleanup   = if (hasFinally) new asm.Label else null

      /* ------ (1) try-block, protected by:
       *                       (1.a) the EHs due to case-clauses,   emitted in (2),
       *                       (1.b) the EH  due to finally-clause, emitted in (3.A)
       *                       (1.c) whatever protects the whole try-catch-finally expression.
       * ------
       */

      val startTryBody = currProgramPoint()
      registerCleanup(finCleanup)
      genLoad(block, kind)
      unregisterCleanup(finCleanup)
      nopIfNeeded(startTryBody)
      val endTryBody = currProgramPoint()
      bc goTo postHandlers

      /*
       * A return within a `try` or `catch` block where a `finally` is present ("early return")
       * emits a store of the result to a local, jump to a "cleanup" version of the `finally` block,
       * and sets `shouldEmitCleanup = true` (see [[PlainBodyBuilder.genReturn]]).
       *
       * If the try-catch is nested, outer `finally` blocks need to be emitted in a cleanup version
       * as well, so the `shouldEmitCleanup` variable remains `true` until the outermost `finally`.
       * Nested cleanup `finally` blocks jump to the next enclosing one. For the outermost, we emit
       * a read of the local variable, a return, and we set `shouldEmitCleanup = false` (see
       * [[pendingCleanups]]).
       *
       * Now, assume we have
       *
       *     try { return 1 } finally {
       *       try { println() } finally { println() }
       *     }
       *
       * Here, the outer `finally` needs a cleanup version, but the inner one does not. The method
       * here makes sure that `shouldEmitCleanup` is only propagated outwards, not inwards to
       * nested `finally` blocks.
       */
      def withFreshCleanupScope(body: => Unit) = {
        val savedShouldEmitCleanup = shouldEmitCleanup
        shouldEmitCleanup = false
        body
        shouldEmitCleanup = savedShouldEmitCleanup || shouldEmitCleanup
      }

      /* ------ (2) One EH for each case-clause (this does not include the EH-version of the finally-clause)
       *            An EH in (2) is reached upon abrupt termination of (1).
       *            An EH in (2) is protected by:
       *                         (2.a) the EH-version of the finally-clause, if any.
       *                         (2.b) whatever protects the whole try-catch-finally expression.
       * ------
       */

      for (ch <- caseHandlers) withFreshCleanupScope {
        // (2.a) emit case clause proper
        val startHandler = currProgramPoint()
        var endHandler: asm.Label = null
        var excType: ClassBType = null
        registerCleanup(finCleanup)
        ch match {
          case NamelessEH(typeToDrop, caseBody) =>
            bc drop typeToDrop
            genLoad(caseBody, kind) // adapts caseBody to `kind`, thus it can be stored, if `guardResult`, in `tmp`.
            nopIfNeeded(startHandler)
            endHandler = currProgramPoint()
            excType = typeToDrop

          case BoundEH   (patSymbol,  caseBody) =>
            // test/files/run/contrib674.scala , a local-var already exists for patSymbol.
            // rather than creating on first-access, we do it right away to emit debug-info for the created local var.
            val Local(patTK, _, patIdx, _) = locals.getOrMakeLocal(patSymbol)
            bc.store(patIdx, patTK)
            genLoad(caseBody, kind)
            nopIfNeeded(startHandler)
            endHandler = currProgramPoint()
            emitLocalVarScope(patSymbol, startHandler, endHandler)
            excType = patTK.asClassBType
        }
        unregisterCleanup(finCleanup)
        // (2.b)  mark the try-body as protected by this case clause.
        protect(startTryBody, endTryBody, startHandler, excType)
        // (2.c) emit jump to the program point where the finally-clause-for-normal-exit starts, or in effect `after` if no finally-clause was given.
        bc goTo postHandlers
      }

      // Need to save the state of `shouldEmitCleanup` at this point: while emitting the first
      // version of the `finally` block below, the variable may become true. But this does not mean
      // that we need a cleanup version for the current block, only for the enclosing ones.
      val currentFinallyBlockNeedsCleanup = shouldEmitCleanup

      /* ------ (3.A) The exception-handler-version of the finally-clause.
       *              Reached upon abrupt termination of (1) or one of the EHs in (2).
       *              Protected only by whatever protects the whole try-catch-finally expression.
       * ------
       */

      // a note on terminology: this is not "postHandlers", despite appearances.
      // "postHandlers" as in the source-code view. And from that perspective, both (3.A) and (3.B) are invisible implementation artifacts.
      if (hasFinally) withFreshCleanupScope {
        nopIfNeeded(startTryBody)
        val finalHandler = currProgramPoint() // version of the finally-clause reached via unhandled exception.
        protect(startTryBody, finalHandler, finalHandler, null)
        val Local(eTK, _, eIdx, _) = locals(locals.makeLocal(jlThrowableRef, "exc"))
        bc.store(eIdx, eTK)
        emitFinalizer(finalizer, null, isDuplicate = true)
        bc.load(eIdx, eTK)
        emit(asm.Opcodes.ATHROW)
      }

      /* ------ (3.B) Cleanup-version of the finally-clause.
       *              Reached upon early RETURN from (1) or upon early RETURN from one of the EHs in (2)
       *              (and only from there, ie reached only upon early RETURN from
       *               program regions bracketed by registerCleanup/unregisterCleanup).
       *              Protected only by whatever protects the whole try-catch-finally expression.
       *
       *              Given that control arrives to a cleanup section only upon early RETURN,
       *              the value to return (if any) is always available. Therefore, a further RETURN
       *              found in a cleanup section is always ignored (a warning is displayed, @see `genReturn()`).
       *              In order for `genReturn()` to know whether the return statement is enclosed in a cleanup section,
       *              the variable `insideCleanupBlock` is used.
       * ------
       */

      // this is not "postHandlers" either.
      // `shouldEmitCleanup` can be set, and at the same time this try expression may lack a finally-clause.
      // In other words, all combinations of (hasFinally, shouldEmitCleanup) are valid.
      if (hasFinally && currentFinallyBlockNeedsCleanup) {
        markProgramPoint(finCleanup)
        // regarding return value, the protocol is: in place of a `return-stmt`, a sequence of `adapt, store, jump` are inserted.
        emitFinalizer(finalizer, null, isDuplicate = true)
        pendingCleanups()
      }

      /* ------ (4) finally-clause-for-normal-nonEarlyReturn-exit
       *            Reached upon normal, non-early-return termination of (1) or of an EH in (2).
       *            Protected only by whatever protects the whole try-catch-finally expression.
       * TODO explain what happens upon RETURN contained in (4)
       * ------
       */

      markProgramPoint(postHandlers)
      if (hasFinally) {
        emitFinalizer(finalizer, tmp, isDuplicate = false) // the only invocation of emitFinalizer with `isDuplicate == false`
      }

      kind
    } // end of genLoadTry()

    /* if no more pending cleanups, all that remains to do is return. Otherwise jump to the next (outer) pending cleanup. */
    private def pendingCleanups(): Unit = {
      cleanups match {
        case Nil =>
          if (earlyReturnVar != null) {
            locals.load(earlyReturnVar)
            bc.emitRETURN(locals(earlyReturnVar).tk)
          } else {
            bc emitRETURN UNIT
          }
          shouldEmitCleanup = false

        case nextCleanup :: _ =>
          bc goTo nextCleanup
      }
    }

    def protect(start: asm.Label, end: asm.Label, handler: asm.Label, excType: ClassBType): Unit = {
      val excInternalName: String =
        if (excType == null) null
        else excType.internalName
      assert(start != end, "protecting a range of zero instructions leads to illegal class format. Solution: add a NOP to that range.")
      mnode.visitTryCatchBlock(start, end, handler, excInternalName)
    }

    /* `tmp` (if non-null) is the symbol of the local-var used to preserve the result of the try-body, see `guardResult` */
    def emitFinalizer(finalizer: Tree, tmp: Symbol, isDuplicate: Boolean): Unit = {
      var saved: immutable.Map[ /* LabelDef */ Symbol, JumpDestination ] = null
      if (isDuplicate) {
        saved = jumpDest
        for(ldef <- labelDefsAtOrUnder.getOrElse(finalizer, Nil)) {
          jumpDest -= ldef.symbol
        }
      }
      // when duplicating, the above guarantees new asm.Labels are used for LabelDefs contained in the finalizer (their vars are reused, that's ok)
      if (tmp != null) { locals.store(tmp) }
      genLoad(finalizer, UNIT)
      if (tmp != null) { locals.load(tmp)  }
      if (isDuplicate) {
        jumpDest = saved
      }
    }

    /* Does this tree have a try-catch block? */
    def mayCleanStack(tree: Tree): Boolean = tree exists { t => t.isInstanceOf[Try] }

    sealed trait EHClause
    case class NamelessEH(typeToDrop: ClassBType,  caseBody: Tree) extends EHClause
    case class BoundEH    (patSymbol: Symbol, caseBody: Tree) extends EHClause

  }

}
