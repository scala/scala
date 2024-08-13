/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
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

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.tools.asm
import scala.tools.asm.Opcodes._
import scala.tools.asm.Type
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis.Value
import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.nsc.backend.jvm.analysis._
import scala.tools.nsc.backend.jvm.analysis.BackendUtils.LambdaMetaFactoryCall
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

abstract class Inliner {
  val postProcessor: PostProcessor

  import postProcessor._
  import bTypes._
  import bTypesFromClassfile._
  import backendUtils._
  import callGraph._
  import frontendAccess.{backendReporting, compilerSettings}
  import inlinerHeuristics._

  // A callsite that was inlined and the IllegalAccessInstructions warning that was delayed.
  // The inliner speculatively inlines a callsite even if the method then has instructions that would
  // cause an IllegalAccessError in the target class. If all of those instructions are eliminated
  // (by inlining) in a later round, everything is fine. Otherwise the method is reverted.
  final case class InlinedCallsite(eliminatedCallsite: Callsite, warning: Option[IllegalAccessInstructions]) {
    // If this InlinedCallsite has a warning about a given instruction, return a copy where the warning
    // only contains that instruction.
    def filterForWarning(insn: AbstractInsnNode): Option[InlinedCallsite] = warning match {
      case Some(w) if w.instructions.contains(insn) => Some(this.copy(warning = Some(w.copy(instructions = List(insn)))))
      case _ => None
    }
  }

  // The state accumulated across inlining rounds for a single MethodNode
  final class MethodInlinerState {
    // Instructions that were copied into a method and would cause an IllegalAccess. They need to
    // be inlined in a later round, otherwise the method is rolled back to its original state.
    val illegalAccessInstructions = mutable.Set.empty[AbstractInsnNode]

    // A map from invocation instructions that were copied (inlined) into this method to the
    // inlined callsite from which they originate.
    // Note: entries are not removed from this map, even if an inlined callsite gets inlined in a
    // later round. This allows re-constructing the inline chain.
    val inlinedCalls = mutable.Map.empty[AbstractInsnNode, InlinedCallsite]

    var undoLog: UndoLog = NoUndoLogging

    var inlineLog = new InlineLog

    override def clone(): MethodInlinerState = {
      val r = new MethodInlinerState
      r.illegalAccessInstructions ++= illegalAccessInstructions
      r.inlinedCalls ++= inlinedCalls
      // The clone references the same InlineLog, so no logs are discarded when rolling back
      r.inlineLog = inlineLog
      // Skip undoLog: clone() is only called when undoLog == NoUndoLogging
      r
    }

    def outerCallsite(call: AbstractInsnNode): Option[Callsite] = inlinedCalls.get(call).map(_.eliminatedCallsite)

    // The chain of inlined callsites that that lead to some (call) instruction. Don't include
    // synthetic forwarders if skipForwarders is true (don't show those in inliner warnings, as they
    // don't show up in the source code).
    // Also used to detect inlining cycles.
    def inlineChain(call: AbstractInsnNode, skipForwarders: Boolean): List[Callsite] = {
      @tailrec def impl(insn: AbstractInsnNode, res: List[Callsite]): List[Callsite] = inlinedCalls.get(insn) match {
        case Some(inlinedCallsite) =>
          val cs = inlinedCallsite.eliminatedCallsite
          val res1 = if (skipForwarders && backendUtils.isTraitSuperAccessorOrMixinForwarder(cs.callee.get.callee, cs.callee.get.calleeDeclarationClass)) res else cs :: res
          impl(cs.callsiteInstruction, res1)
        case _ =>
          res
      }
      impl(call, Nil)
    }

    // In a chain of inlined calls which lead to some (call) instruction, return the root `InlinedCallsite`
    // which has a delayed warning . When inlining `call` fails, warn about the root instruction instead of
    // the downstream inline request that tried to eliminate an illegalAccess instruction.
    // This method skips over forwarders. For example in `trait T { def m = ... }; class A extends T`
    // the inline chain is `A.m (mixin forwarder) - T.m$ (static accessor) - T.m`. The method returns
    // `T.m` (even if the root callsite is `A.m`.
    // If the chain has only forwarders, `returnForwarderIfNoOther` determines whether to return `None`
    // or the last inlined forwarder.
    def rootInlinedCallsiteWithWarning(call: AbstractInsnNode, returnForwarderIfNoOther: Boolean): Option[InlinedCallsite] = {
      def isForwarder(callsite: Callsite) = backendUtils.isTraitSuperAccessorOrMixinForwarder(callsite.callee.get.callee, callsite.callee.get.calleeDeclarationClass)
      def result(res: Option[InlinedCallsite]) = res match {
        case Some(r) if returnForwarderIfNoOther || !isForwarder(r.eliminatedCallsite) => res
        case _ => None
      }
      @tailrec def impl(insn: AbstractInsnNode, res: Option[InlinedCallsite]): Option[InlinedCallsite] = inlinedCalls.get(insn) match {
        case Some(inlinedCallsite) =>
          val w = inlinedCallsite.filterForWarning(insn)
          if (w.isEmpty) result(res)
          else {
            val cs = inlinedCallsite.eliminatedCallsite
            // The returned InlinedCallsite can be a forwarder if that forwarder was the initial callsite in the method
            val nextRes = if (isForwarder(cs) && res.nonEmpty && !isForwarder(res.get.eliminatedCallsite)) res else w
            impl(cs.callsiteInstruction, nextRes)
          }

        case _ => result(res)
      }
      impl(call, None)
    }
  }

  final class InlineLog {
    import InlineLog._

    private var _active = false

    var roots: mutable.ArrayBuffer[InlineLogResult] = null
    var downstream: mutable.HashMap[Callsite, mutable.ArrayBuffer[InlineLogResult]] = null
    var callsiteInfo: String = null

    // A bit of a hack.. We check the -Yopt-log-inline flag when logging the first inline request.
    // Because the InlineLog is part of the MethodInlinerState, subsequent requests will all be
    // for the same callsite class / method. At the point where the MethodInlinerState is created
    // we don't have access to the enclosing class.
    private def active(callsiteClass: ClassBType, callsiteMethod: MethodNode): Boolean = {
      if (roots == null) {
        compilerSettings.optLogInline match {
          case Some("_") => _active = true
          case Some(prefix) => _active = s"${callsiteClass.internalName}.${callsiteMethod.name}" startsWith prefix
          case _ => _active = false
        }
        if (_active) {
          roots = mutable.ArrayBuffer.empty[InlineLogResult]
          downstream = mutable.HashMap.empty[Callsite, mutable.ArrayBuffer[InlineLogResult]]
          callsiteInfo = s"Inlining into ${callsiteClass.internalName}.${callsiteMethod.name}"
        }
      }
      _active
    }

    private def active(callsite: Callsite): Boolean = active(callsite.callsiteClass, callsite.callsiteMethod)

    private def bufferForOuter(outer: Option[Callsite]) = outer match {
      case Some(o) => downstream.getOrElse(o, roots)
      case _ => roots
    }

    def logSuccess(request: InlineRequest, sizeBefore: Int, sizeAfter: Int, outer: Option[Callsite]) = if (active(request.callsite)) {
      bufferForOuter(outer) += InlineLogSuccess(request, sizeBefore, sizeAfter)
      downstream(request.callsite) = mutable.ArrayBuffer.empty
    }

    def logClosureRewrite(closureInit: ClosureInstantiation, invocations: mutable.ArrayBuffer[(MethodInsnNode, Int)], outer: Option[Callsite]) = if (active(closureInit.ownerClass, closureInit.ownerMethod)) {
      bufferForOuter(outer) += InlineLogRewrite(closureInit, invocations.map(_._1).toList)
    }

    def logFail(request: InlineRequest, warning: CannotInlineWarning, outer: Option[Callsite]) = if (active(request.callsite)) {
      bufferForOuter(outer) += InlineLogFail(request, warning)
    }

    def logRollback(callsite: Callsite, reason: String, outer: Option[Callsite]) = if (active(callsite)) {
      bufferForOuter(outer) += InlineLogRollback(reason)
    }

    def nonEmpty = roots != null

    def print(): Unit = if (roots != null) {
      def printChildren(indent: Int, callsite: Callsite): Unit = downstream.get(callsite) match {
        case Some(logs) => logs.foreach(l => printLog(indent, l))
        case _ =>
      }
      def printLog(indent: Int, log: InlineLogResult): Unit = {
        println(log.entryString(indent))
        log match {
          case s: InlineLogSuccess => printChildren(indent + 1, s.request.callsite)
          case _ =>
        }
      }
      roots.size match {
        case 0 =>
        case 1 =>
          Console.print(callsiteInfo)
          Console.print(": ")
          printLog(0, roots(0))
        case _ =>
          println(callsiteInfo)
          for (log <- roots) printLog(1, log)
      }
    }
  }

  object InlineLog {
    sealed trait InlineLogResult {
      def entryString(indent: Int): String = {
        def calleeString(r: InlineRequest) = {
          val callee = r.callsite.callee.get
          callee.calleeDeclarationClass.internalName + "." + callee.callee.name
        }
        val indentString = " " * indent
        this match {
          case InlineLogSuccess(r, sizeBefore, sizeAfter) =>
            s"${indentString}inlined ${calleeString(r)} (${r.logText}). Before: $sizeBefore ins, after: $sizeAfter ins."

          case InlineLogRewrite(closureInit, invocations) =>
            s"${indentString}rewrote invocations of closure allocated in ${closureInit.ownerClass.internalName}.${closureInit.ownerMethod.name} with body ${closureInit.lambdaMetaFactoryCall.implMethod.getName}: ${invocations.map(AsmUtils.textify).mkString(", ")}"

          case InlineLogFail(r, w) =>
            s"${indentString}failed ${calleeString(r)} (${r.logText}). ${w.toString.replace('\n', ' ')}"

          case InlineLogRollback(reason) =>
            s"${indentString}rolled back: $reason."
        }
      }
    }
    final case class InlineLogSuccess(request: InlineRequest, sizeBefore: Int, sizeAfter: Int) extends InlineLogResult
    final case class InlineLogRewrite(closureInit: ClosureInstantiation, invocations: List[MethodInsnNode]) extends InlineLogResult
    final case class InlineLogFail(request: InlineRequest, warning: CannotInlineWarning) extends InlineLogResult
    final case class InlineLogRollback(reason: String) extends InlineLogResult
  }

  // True if all instructions (they would cause an IllegalAccessError otherwise) can potentially be
  // inlined in a later inlining round.
  // Note that this method has a side effect. It allows inlining `INVOKESPECIAL` calls of static
  // super accessors that we emit in traits. The inlined calls are marked in the call graph as
  // `staticallyResolvedInvokespecial`. When looking up the MethodNode for the cloned `INVOKESPECIAL`,
  // the call graph will always return the corresponding method in the trait.
  def maybeInlinedLater(callsite: Callsite, insns: List[AbstractInsnNode]): Boolean = {
    insns.forall({
      case mi: MethodInsnNode =>
        (mi.getOpcode != INVOKESPECIAL) || {
          // Special handling for invokespecial T.f that appears within T, and T defines f.
          // Such an instruction can be inlined into a different class, but it needs to be inlined in
          // turn in a later inlining round.
          // The call graph needs to treat it specially: the normal dynamic lookup needs to be
          // avoided, it needs to resolve to T.f, no matter in which class the invocation appears.
          def hasMethod(c: ClassNode): Boolean = {
            val r = c.methods.iterator.asScala.exists(m => m.name == mi.name && m.desc == mi.desc)
            if (r) callGraph.staticallyResolvedInvokespecial += mi
            r
          }

          mi.name != GenBCode.INSTANCE_CONSTRUCTOR_NAME &&
            mi.owner == callsite.callee.get.calleeDeclarationClass.internalName &&
            byteCodeRepository.classNode(mi.owner).map(hasMethod).getOrElse(false)
        }
      case _ => false
    })
  }

  def runInlinerAndClosureOptimizer(): Unit = {
    val runClosureOptimizer = compilerSettings.optClosureInvocations
    var round = 0
    var changedByClosureOptimizer = mutable.LinkedHashSet.empty[MethodNode]

    val inlinerState = mutable.Map.empty[MethodNode, MethodInlinerState]

    // Don't try again to inline failed callsites
    val failedToInline = mutable.Set.empty[MethodInsnNode]

    while (round < 10 && (round == 0 || changedByClosureOptimizer.nonEmpty)) {
      val specificMethodsForInlining = if (round == 0) None else Some(changedByClosureOptimizer)
      val changedByInliner = runInliner(specificMethodsForInlining, inlinerState, failedToInline)

      if (runClosureOptimizer) {
        val specificMethodsForClosureRewriting = if (round == 0) None else Some(changedByInliner)
        // TODO: remove cast by moving `MethodInlinerState` and other classes from inliner to a separate PostProcessor component
        changedByClosureOptimizer = closureOptimizer.rewriteClosureApplyInvocations(specificMethodsForClosureRewriting, inlinerState.asInstanceOf[mutable.Map[MethodNode, postProcessor.closureOptimizer.postProcessor.inliner.MethodInlinerState]])
      }

      var logs = List.empty[(MethodNode, InlineLog)]
      for (m <- inlinerState.keySet if !changedByClosureOptimizer(m)) {
        val log = inlinerState.remove(m).get.inlineLog
        if (log.nonEmpty) logs ::= ((m, log))
      }
      if (logs.nonEmpty) {
        // Deterministic inline log
        val sortedLogs = logs.sorted(Ordering.by[(MethodNode, InlineLog), (String, String)](p => (p._1.name, p._1.desc)))
        sortedLogs.foreach(_._2.print())
      }

      round += 1
    }
  }

  /**
   * @param methods The methods to check for callsites to inline. If not defined, check all methods.
   * @return The set of changed methods, in no deterministic order.
   */
  def runInliner(methods: Option[mutable.LinkedHashSet[MethodNode]], inlinerState: mutable.Map[MethodNode, MethodInlinerState], failed: mutable.Set[MethodInsnNode]): Iterable[MethodNode] = {
    // Inline requests are grouped by method for performance: we only update the call graph (which
    // runs analyzers) once all callsites are inlined.
    val requests: mutable.Queue[(MethodNode, List[InlineRequest])] =
      if (methods.isEmpty) collectAndOrderInlineRequests
      else mutable.Queue.empty

    // Methods that were changed (inlined into), they will be checked for more callsites to inline
    val changedMethods = {
      val r = mutable.Queue.empty[MethodNode]
      methods.foreach(r.addAll)
      r
    }

    var changedMethodHasIllegalAccess = false

    // TODO: remove those that were rolled back to their original form?
    val overallChangedMethods = mutable.Set.empty[MethodNode]

    // Show chain of inlines that lead to a failure in inliner warnings
    def inlineChainSuffix(callsite: Callsite, chain: List[Callsite]): String =
      if (chain.isEmpty) "" else
        s"""
           |Note that this callsite was itself inlined into ${BackendReporting.methodSignature(callsite.callsiteClass.internalName, callsite.callsiteMethod)}
           |by inlining the following methods:
           |${chain.map(cs => BackendReporting.methodSignature(cs.callee.get.calleeDeclarationClass.internalName, cs.callee.get.callee)).mkString("  - ", "\n  - ", "")}""".stripMargin

    while (requests.nonEmpty || changedMethods.nonEmpty) {
      // First inline all requests that were initially collected. Then check methods that changed
      // for more callsites to inline.
      // Alternatively, we could find more callsites directly after inlining the initial requests
      // of a method, before inlining into other methods. But that could cause work duplication. If
      // a callee is inlined before the inliner has run on it, the inliner needs to do the work on
      // both the callee and the cloned version(s).
      // Exception: if, after inlining, `m` has instructions that would cause an IllegalAccessError,
      // continue inlining into `m`. These instructions might get inlined as well, otherwise `m` is
      // rolled back. This avoid cloning the illegal instructions in case `m` itself gets inlined.
      if (requests.nonEmpty && !changedMethodHasIllegalAccess) {
        val (method, rs) = requests.dequeue()
        val state = inlinerState.getOrElseUpdate(method, new MethodInlinerState)
        var changed = false

        def doInline(r: InlineRequest, aliasFrame: AliasingFrame[Value], w: Option[IllegalAccessInstructions]): Map[AbstractInsnNode, AbstractInsnNode] = {
          val sizeBefore = method.instructions.size // cheap (a field read)
          val instructionMap = inlineCallsite(r.callsite, Some(aliasFrame), updateCallGraph = false)
          val inlined = InlinedCallsite(r.callsite, w.map(iw => iw.copy(instructions = iw.instructions.map(instructionMap))))
          instructionMap.valuesIterator foreach {
            case mi: MethodInsnNode => state.inlinedCalls(mi) = inlined
            case _ =>
          }
          for (warn <- w; ins <- warn.instructions) {
            state.illegalAccessInstructions += instructionMap(ins)
          }
          val callInsn = r.callsite.callsiteInstruction
          state.illegalAccessInstructions.subtractOne(callInsn)
          if (state.illegalAccessInstructions.isEmpty)
            state.undoLog = NoUndoLogging
          state.inlineLog.logSuccess(r, sizeBefore, method.instructions.size, state.outerCallsite(r.callsite.callsiteInstruction))
          changed = true
          instructionMap
        }

        val rsWithAliasFrames = {
          val cs = rs.head.callsite
          val a = new BasicAliasingAnalyzer(cs.callsiteMethod, cs.callsiteClass.internalName)
          rs.map(r => (r, a.frameAt(r.callsite.callsiteInstruction).asInstanceOf[AliasingFrame[Value]]))
        }

        var currentMethodRolledBack = false

        for ((r, aliasFrame) <- rsWithAliasFrames) if (!currentMethodRolledBack) {
          canInlineCallsite(r.callsite) match {
            case None =>
              doInline(r, aliasFrame, None)

            case Some(w: IllegalAccessInstructions) if maybeInlinedLater(r.callsite, w.instructions) =>
              if (state.undoLog == NoUndoLogging) {
                val undo = new UndoLog()
                val currentState = state.clone()
                // undo actions for the method and global state
                undo.saveMethodState(r.callsite.callsiteClass, method)
                undo {
                  // undo actions for the state of the inliner loop
                  failed += r.callsite.callsiteInstruction
                  inlinerState(method) = currentState
                  // method is not in changedMethods in both places where `rollback` is invoked
                  changedMethods.enqueue(method)
                }
                state.undoLog = undo
              }
              doInline(r, aliasFrame, Some(w))

            case Some(w) =>
              val callInsn = r.callsite.callsiteInstruction

              state.inlineLog.logFail(r, w, state.outerCallsite(r.callsite.callsiteInstruction))

              if (state.illegalAccessInstructions(callInsn)) {
                state.inlineLog.logRollback(r.callsite, "The callsite could not be inlined, keeping it would cause an IllegalAccessError", state.outerCallsite(r.callsite.callsiteInstruction))
                state.undoLog.rollback()
                currentMethodRolledBack = true
              }

              state.rootInlinedCallsiteWithWarning(r.callsite.callsiteInstruction, returnForwarderIfNoOther = false) match {
                case Some(inlinedCallsite) =>
                  val rw = inlinedCallsite.warning.get
                  if (rw.emitWarning(compilerSettings)) {
                    backendReporting.optimizerWarning(
                      inlinedCallsite.eliminatedCallsite.callsitePosition,
                      rw.toString + inlineChainSuffix(r.callsite, state.inlineChain(inlinedCallsite.eliminatedCallsite.callsiteInstruction, skipForwarders = true)),
                      backendUtils.optimizerWarningSiteString(inlinedCallsite.eliminatedCallsite))
                  }
                case _ =>
                  if (w.emitWarning(compilerSettings))
                    backendReporting.optimizerWarning(
                      r.callsite.callsitePosition,
                      w.toString + inlineChainSuffix(r.callsite, state.inlineChain(r.callsite.callsiteInstruction, skipForwarders = true)),
                      backendUtils.optimizerWarningSiteString(r.callsite))
              }
          }
        }

        if (changed) {
          callGraph.refresh(method, rs.head.callsite.callsiteClass)
          if (state.illegalAccessInstructions.nonEmpty) {
            changedMethods.prepend(method)
            changedMethodHasIllegalAccess = true
          } else
            changedMethods.enqueue(method)
          overallChangedMethods += method
        }

      } else {
        // look at all callsites in a methods again, also those that were previously not selected for
        // inlining. after inlining, types might get more precise and make a callsite inlineable.
        val method = changedMethods.dequeue()
        val state = inlinerState.getOrElseUpdate(method, new MethodInlinerState)

        def isLoop(call: MethodInsnNode, callee: Callee): Boolean =
          callee.callee == method || {
            state.inlineChain(call, skipForwarders = false).exists(_.callee.get.callee == callee.callee)
          }

        val rs = mutable.ListBuffer.empty[InlineRequest]
        callGraph.callsites(method).valuesIterator foreach {
          // Don't inline: recursive calls, callsites that failed inlining before
          case cs: Callsite if !failed(cs.callsiteInstruction) && cs.callee.isRight && !isLoop(cs.callsiteInstruction, cs.callee.get) =>
            inlineRequest(cs) match {
              case Some(Right(req)) => rs += req
              case _ =>
            }
          case _ =>
        }
        val newRequests = selectRequestsForMethodSize(method, rs.toList.sorted(inlineRequestOrdering), mutable.Map.empty)

        state.illegalAccessInstructions.find(insn => newRequests.forall(_.callsite.callsiteInstruction != insn)) match {
          case None =>
            // why prepend: see changedMethodHasIllegalAccess
            if (newRequests.nonEmpty) requests.prepend(method -> newRequests)

          case Some(notInlinedIllegalInsn) =>
            state.undoLog.rollback()
            state.rootInlinedCallsiteWithWarning(notInlinedIllegalInsn, returnForwarderIfNoOther = true) match {
              case Some(inlinedCallsite) =>
                val callsite = inlinedCallsite.eliminatedCallsite
                val w = inlinedCallsite.warning.get
                state.inlineLog.logRollback(callsite, s"Instruction ${AsmUtils.textify(notInlinedIllegalInsn)} would cause an IllegalAccessError, and is not selected for (or failed) inlining", state.outerCallsite(notInlinedIllegalInsn))
                if (w.emitWarning(compilerSettings))
                  backendReporting.optimizerWarning(
                    callsite.callsitePosition,
                    w.toString + inlineChainSuffix(callsite, state.inlineChain(callsite.callsiteInstruction, skipForwarders = true)),
                    backendUtils.optimizerWarningSiteString(callsite))
              case _ =>
                // TODO: replace by dev warning after testing
                assert(false, "should not happen")
            }
        }

        changedMethodHasIllegalAccess = false
      }
    }

    overallChangedMethods
  }

  /**
   * Ordering for inline requests. Required to make the inliner deterministic:
   *   - Always remove the same request when breaking inlining cycles
   *   - Perform inlinings in a consistent order
   */
  object callsiteOrdering extends Ordering[Callsite] {
    override def compare(x: Callsite, y: Callsite): Int = {
      if (x eq y) return 0

      val cls = x.callsiteClass.internalName compareTo y.callsiteClass.internalName
      if (cls != 0) return cls

      val name = x.callsiteMethod.name compareTo y.callsiteMethod.name
      if (name != 0) return name

      val desc = x.callsiteMethod.desc compareTo y.callsiteMethod.desc
      if (desc != 0) return desc

      def pos(c: Callsite) = c.callsiteMethod.instructions.indexOf(c.callsiteInstruction)
      pos(x) - pos(y)
    }
  }

  val inlineRequestOrdering = Ordering.by[InlineRequest, Callsite](_.callsite)(callsiteOrdering)

  /**
   * Returns the callsites that can be inlined, grouped by method. Ensures that the returned inline
   * request graph does not contain cycles.
   *
   * The resulting list is sorted such that the leaves of the inline request graph are on the left.
   * Once these leaves are inlined, the successive elements will be leaves, etc.
   */
  private def collectAndOrderInlineRequests: mutable.Queue[(MethodNode, List[InlineRequest])] = {
    val requestsByMethod = selectCallsitesForInlining withDefaultValue Set.empty

    val elided = mutable.Set.empty[InlineRequest]
    def nonElidedRequests(methodNode: MethodNode): Set[InlineRequest] = requestsByMethod(methodNode) diff elided

    /*
     * Break cycles in the inline request graph by removing callsites.
     *
     * The list `requests` is traversed left-to-right, removing those callsites that are part of a
     * cycle. Elided callsites are also removed from the `inlineRequestsForMethod` map.
     */
    def breakInlineCycles: List[(MethodNode, List[InlineRequest])] = {
      // is there a path of inline requests from start to goal?
      def isReachable(start: MethodNode, goal: MethodNode): Boolean = {
        @tailrec def reachableImpl(check: Set[MethodNode], visited: Set[MethodNode]): Boolean = {
          if (check.isEmpty) false
          else {
            val x = check.head
            if (x == goal) true
            else if (visited(x)) reachableImpl(check - x, visited)
            else {
              val callees = nonElidedRequests(x).map(_.callsite.callee.get.callee)
              reachableImpl(check - x ++ callees, visited + x)
            }
          }
        }
        reachableImpl(Set(start), Set.empty)
      }

      val requests = requestsByMethod.valuesIterator.flatten.toArray
      // sort the inline requests to ensure that removing requests is deterministic
      // Callsites within the same method are next to each other in the sorted array.
      java.util.Arrays.sort(requests, inlineRequestOrdering)

      val result = new mutable.ListBuffer[(MethodNode, List[InlineRequest])]()
      var currentMethod: MethodNode = null
      val currentMethodRequests = mutable.ListBuffer.empty[InlineRequest]
      for (r <- requests) {
        // is there a chain of inlining requests that would inline the callsite method into the callee?
        if (isReachable(r.callsite.callee.get.callee, r.callsite.callsiteMethod))
          elided += r
        else {
          val m = r.callsite.callsiteMethod
          if (m == currentMethod) {
            currentMethodRequests += r
          } else {
            if (currentMethod != null)
              result += ((currentMethod, currentMethodRequests.toList))
            currentMethod = m
            currentMethodRequests.clear()
            currentMethodRequests += r
          }
        }
      }
      if (currentMethod != null)
        result += ((currentMethod, currentMethodRequests.toList))
      result.toList
    }

    // sort the remaining inline requests such that the leaves appear first, then those requests
    // that become leaves, etc.
    def leavesFirst(requests: List[(MethodNode, List[InlineRequest])]): mutable.Queue[(MethodNode, List[InlineRequest])] = {
      val result = mutable.Queue.empty[(MethodNode, List[InlineRequest])]
      val visited = mutable.Set.empty[MethodNode]

      @tailrec def impl(toAdd: List[(MethodNode, List[InlineRequest])]): Unit =
        if (toAdd.nonEmpty) {
          val rest = mutable.ListBuffer.empty[(MethodNode, List[InlineRequest])]
          toAdd.foreach { case r @ (_, rs) =>
            val callees = rs.iterator.map(_.callsite.callee.get.callee)
            if (callees.forall(c => visited(c) || nonElidedRequests(c).isEmpty)) {
              result += r
              visited += r._1
            } else
              rest += r
          }
          impl(rest.toList)
        }

      impl(requests)
      result
    }

    val sortedRequests = leavesFirst(breakInlineCycles)
    val methodSizes = mutable.Map.empty[MethodNode, Int]
    val result = mutable.Queue.empty[(MethodNode, List[InlineRequest])]
    for ((method, rs) <- sortedRequests) {
      val sizeOkRs = selectRequestsForMethodSize(method, rs, methodSizes)
      if (sizeOkRs.nonEmpty)
      result += ((method, sizeOkRs))
    }
    result
  }

  class UndoLog(active: Boolean = true) {
    import java.util.{ArrayList => JArrayList}

    private var actions = List.empty[() => Unit]

    def apply(a: => Unit): Unit = if (active) actions = (() => a) :: actions
    def rollback(): Unit = if (active) actions.foreach(_.apply())

    def saveMethodState(ownerClass: ClassBType, methodNode: MethodNode): Unit = if (active) {
      val currentInstructions = methodNode.instructions.toArray
      val currentLocalVariables = new JArrayList(methodNode.localVariables)
      val currentTryCatchBlocks = new JArrayList(methodNode.tryCatchBlocks)
      val currentMaxLocals = methodNode.maxLocals
      val currentMaxStack = methodNode.maxStack

      val currentIndyLambdaBodyMethods = indyLambdaBodyMethods(ownerClass.internalName, methodNode)

      // Instead of saving / restoring the CallGraph's callsites / closureInstantiations, we call
      // callGraph.refresh on rollback. The call graph might not be up to date at the point where
      // we save the method state, because it might be in the middle of inlining some callsites of
      // that method. The call graph is only updated at the end (in the inliner loop).
      // We don't save / restore the CallGraph's
      //   - callsitePositions
      //   - inlineAnnotatedCallsites
      //   - noInlineAnnotatedCallsites
      //   - staticallyResolvedInvokespecial
      // These contain instructions, and we never remove from them. So when rolling back a method's
      // instruction list, the old instructions are still in there.

      apply {
        // `methodNode.instructions.clear()` doesn't work: it keeps the `prev` / `next` / `index` of
        // instruction nodes. `instructions.removeAll(true)` would work, but is not public.
        methodNode.instructions.iterator.asScala.toList.foreach(methodNode.instructions.remove)
        for (i <- currentInstructions) methodNode.instructions.add(i)

        methodNode.localVariables.clear()
        methodNode.localVariables.addAll(currentLocalVariables)

        methodNode.tryCatchBlocks.clear()
        methodNode.tryCatchBlocks.addAll(currentTryCatchBlocks)

        methodNode.maxLocals = currentMaxLocals
        methodNode.maxStack = currentMaxStack

        BackendUtils.clearDceDone(methodNode)
        callGraph.refresh(methodNode, ownerClass)

        onIndyLambdaImplMethodIfPresent(ownerClass.internalName)(_.subtractOne(methodNode))
        if (currentIndyLambdaBodyMethods.nonEmpty)
          onIndyLambdaImplMethod(ownerClass.internalName)(ms => ms(methodNode) = mutable.Map.empty ++= currentIndyLambdaBodyMethods)
      }
    }
  }

  val NoUndoLogging = new UndoLog(active = false)

  /**
   * Copy and adapt the instructions of a method to a callsite.
   *
   * Preconditions:
   *   - The callsite can safely be inlined (canInlineBody is true)
   *   - The maxLocals and maxStack values of the callsite method are correctly computed
   *
   * @return A map associating instruction nodes of the callee with the corresponding cloned
   *         instruction in the callsite method.
   */
  def inlineCallsite(callsite: Callsite, aliasFrame: Option[AliasingFrame[Value]] = None, updateCallGraph: Boolean = true): Map[AbstractInsnNode, AbstractInsnNode] = {
    import callsite._
    val Right(callsiteCallee) = callsite.callee: @unchecked
    import callsiteCallee.{callee, calleeDeclarationClass, sourceFilePath}

    val isStatic = isStaticMethod(callee)

    // Inlining requires the callee not to have unreachable code, the analyzer used below should not
    // return any `null` frames. Note that inlining a method can create unreachable code. Example:
    //   def f = throw e
    //   def g = f; println() // println is unreachable after inlining f
    // If we have an inline request for a call to g, and f has been already inlined into g, we
    // need to run DCE on g's body before inlining g.
    localOpt.minimalRemoveUnreachableCode(callee, calleeDeclarationClass.internalName)

    // If the callsite was eliminated by DCE, do nothing.
    if (!callGraph.containsCallsite(callsite)) return Map.empty

    // New labels for the cloned instructions
    val labelsMap = cloneLabels(callee)
    val sameSourceFile = sourceFilePath match {
      case Some(calleeSource) => byteCodeRepository.compilingClasses.get(callsiteClass.internalName) match {
        case Some((_, `calleeSource`)) => true
        case _ => false
      }
      case _ => false
    }
    val (clonedInstructions, instructionMap, writtenLocals) = cloneInstructions(callee, labelsMap, callsitePosition, keepLineNumbers = sameSourceFile)

    val refLocals = mutable.BitSet.empty

    val calleAsmType = asm.Type.getMethodType(callee.desc)
    val calleeParamTypes = calleAsmType.getArgumentTypes

    val f = aliasFrame.getOrElse({
      val aliasAnalysis = new BasicAliasingAnalyzer(callsiteMethod, callsiteClass.internalName)
      aliasAnalysis.frameAt(callsiteInstruction).asInstanceOf[AliasingFrame[Value]]
    })

    //// find out for which argument values on the stack there is already a local variable ////

    val calleeFirstNonParamSlot = BytecodeUtils.parametersSize(callee)

    // Maps callee-local-variable-index to callsite-local-variable-index.
    val calleeParamLocals = new Array[Int](calleeFirstNonParamSlot)

    // Counter for stack slots at the callsite holding the arguments (1 slot also for long / double)
    var callsiteStackSlot = f.getLocals + f.getStackSize - calleeParamTypes.length - (if (isStatic) 0 else 1)
    // Counter for param slots of the callee (long / double use 2 slots)
    var calleeParamSlot = 0
    var nextLocalIndex = BackendUtils.maxLocals(callsiteMethod)

    val numLocals = f.getLocals

    // used later, but computed here
    var skipReceiverNullCheck = receiverKnownNotNull || isStatic

    val paramSizes = (if (isStatic) Iterator.empty else Iterator(1)) ++ calleeParamTypes.iterator.map(_.getSize)
    for (paramSize <- paramSizes) {
      val min = f.aliasesOf(callsiteStackSlot).iterator.min
      if (calleeParamSlot == 0 && !isStatic && min == 0)
        skipReceiverNullCheck = true // no need to null-check `this`
      val isWritten = writtenLocals(calleeParamSlot) || paramSize == 2 && writtenLocals(calleeParamSlot + 1)
      if (min < numLocals && !isWritten) {
        calleeParamLocals(calleeParamSlot) = min
      } else {
        calleeParamLocals(calleeParamSlot) = nextLocalIndex
        nextLocalIndex += paramSize
      }
      if (paramSize == 2)
        calleeParamLocals(calleeParamSlot + 1) = calleeParamLocals(calleeParamSlot) + 1
      callsiteStackSlot += 1
      calleeParamSlot += paramSize
    }

    val numSavedParamSlots = BackendUtils.maxLocals(callsiteMethod) + calleeFirstNonParamSlot - nextLocalIndex

    // local var indices in the callee are adjusted
    val localVarShift = BackendUtils.maxLocals(callsiteMethod) - numSavedParamSlots
    clonedInstructions.iterator.asScala foreach {
      case varInstruction: VarInsnNode =>
        if (varInstruction.`var` < calleeParamLocals.length)
          varInstruction.`var` = calleeParamLocals(varInstruction.`var`)
        else {
          varInstruction.`var` += localVarShift
          if (varInstruction.getOpcode == ASTORE) refLocals += varInstruction.`var`
        }
      case iinc: IincInsnNode =>
        iinc.`var` += localVarShift
      case _ =>
    }

    // add a STORE instruction for each expected argument, including for THIS instance if any
    val argStores = new InsnList
    val nullOutLocals = new InsnList
    val numCallsiteLocals = BackendUtils.maxLocals(callsiteMethod)
    calleeParamSlot = 0
    if (!isStatic) {
      def addNullCheck(): Unit = {
        val nonNullLabel = newLabelNode
        argStores.add(new JumpInsnNode(IFNONNULL, nonNullLabel))
        argStores.add(new InsnNode(ACONST_NULL))
        argStores.add(new InsnNode(ATHROW))
        argStores.add(nonNullLabel)
      }
      val argLocalSlot = calleeParamLocals(calleeParamSlot)
      if (argLocalSlot >= numCallsiteLocals) {
        if (!skipReceiverNullCheck) {
          argStores.add(new InsnNode(DUP))
          addNullCheck()
        }
        argStores.add(new VarInsnNode(ASTORE, argLocalSlot))
        nullOutLocals.add(new InsnNode(ACONST_NULL))
        nullOutLocals.add(new VarInsnNode(ASTORE, argLocalSlot))
      } else if (skipReceiverNullCheck) {
        argStores.add(getPop(1))
      } else {
        addNullCheck()
      }
      calleeParamSlot += 1
    }

    for(argTp <- calleeParamTypes) {
      val argLocalSlot = calleeParamLocals(calleeParamSlot)
      if (argLocalSlot >= numCallsiteLocals) {
        val opc = argTp.getOpcode(ISTORE) // returns the correct xSTORE instruction for argTp
        argStores.insert(new VarInsnNode(opc, argLocalSlot)) // "insert" is "prepend" - the last argument is on the top of the stack
        if (opc == ASTORE) {
          nullOutLocals.add(new InsnNode(ACONST_NULL))
          nullOutLocals.add(new VarInsnNode(ASTORE, argLocalSlot))
        }
      } else
        argStores.insert(getPop(argTp.getSize))
      calleeParamSlot += argTp.getSize
    }

    for (i <- refLocals) {
      nullOutLocals.add(new InsnNode(ACONST_NULL))
      nullOutLocals.add(new VarInsnNode(ASTORE, i))
    }

    clonedInstructions.insert(argStores)

    // label for the exit of the inlined functions. xRETURNs are replaced by GOTOs to this label.
    val postCallLabel = newLabelNode
    clonedInstructions.add(postCallLabel)
    if (sameSourceFile) {
      BytecodeUtils.previousLineNumber(callsiteInstruction) match {
        case Some(line) =>
          BytecodeUtils.nextExecutableInstruction(callsiteInstruction).flatMap(BytecodeUtils.previousLineNumber) match {
            case Some(line1) =>
              if (line == line1)
              // SD-479 code follows on the same line, restore the line number
                clonedInstructions.add(new LineNumberNode(line, postCallLabel))
            case None =>
          }
        case None =>
      }
    }

    // replace xRETURNs:
    //   - store the return value (if any)
    //   - clear the stack of the inlined method (insert DROPs)
    //   - load the return value
    //   - GOTO postCallLabel

    val returnType = calleAsmType.getReturnType
    val hasReturnValue = returnType.getSort != asm.Type.VOID
    // Use a fresh slot for the return value. We could re-use local variable slot of the inlined
    // code, but this makes some cleanups (in LocalOpt) fail / generate less clean code.
    val returnValueIndex = BackendUtils.maxLocals(callsiteMethod) + BackendUtils.maxLocals(callee) - numSavedParamSlots
    var needNullOutReturnValue: Boolean = false

    def returnValueStore(returnInstruction: AbstractInsnNode) = {
      val opc = returnInstruction.getOpcode match {
        case IRETURN => ISTORE
        case LRETURN => LSTORE
        case FRETURN => FSTORE
        case DRETURN => DSTORE
        case ARETURN => ASTORE
      }
      new VarInsnNode(opc, returnValueIndex)
    }

    // We run an interpreter to know the stack height at each xRETURN instruction and the sizes
    // of the values on the stack.
    // We don't need to worry about the method being too large for running an analysis. Callsites of
    // large methods are not added to the call graph.
    val analyzer = new BasicAnalyzer(callee, calleeDeclarationClass.internalName)

    for (originalReturn <- callee.instructions.iterator.asScala if isReturn(originalReturn)) {
      val frame = analyzer.frameAt(originalReturn)
      var stackHeight = frame.getStackSize

      val inlinedReturn = instructionMap(originalReturn)
      val returnReplacement = new InsnList

      def drop(slot: Int) = returnReplacement add getPop(frame.peekStack(slot).getSize)

      if (stackHeight == (if (hasReturnValue) 1 else 0)) {
        // In most cases, the xRETURN we found should be at an empty stack height,
        // either because it comes from Java in statement position, or because
        // it comes from Scala in a tail expression position. For this common
        // case, we don't have to manipulate the stack at all; we only leave
        // the optional return value on the stack before jumping
      } else {
        // Otherwise, we have to empty the stack and only leave the optional
        // return value on the stack.

        // for non-void methods, store the stack top into the return local variable
        if (hasReturnValue) {
          returnReplacement add returnValueStore(originalReturn)
          stackHeight -= 1
          if (originalReturn.getOpcode() == ARETURN)
            needNullOutReturnValue = true
        }

        // drop the rest of the stack
        for (i <- 0 until stackHeight) drop(i)

        // load the return value back on the stack
        if (hasReturnValue)
          returnReplacement add new VarInsnNode(returnType.getOpcode(ILOAD), returnValueIndex)
      }

      returnReplacement add new JumpInsnNode(GOTO, postCallLabel)
      clonedInstructions.insert(inlinedReturn, returnReplacement)
      clonedInstructions.remove(inlinedReturn)
    }

    if (needNullOutReturnValue) {
      nullOutLocals.add(new InsnNode(ACONST_NULL))
      nullOutLocals.add(new VarInsnNode(ASTORE, returnValueIndex))
    }

    val hasNullOutInsn = nullOutLocals.size > 0 // save here, the next line sets the size to 0
    clonedInstructions.add(nullOutLocals)

    callsiteMethod.instructions.insert(callsiteInstruction, clonedInstructions)
    callsiteMethod.instructions.remove(callsiteInstruction)

    val localIndexMap: Int => Int = oldIdx => {
      if (oldIdx < 0) oldIdx
      else if (oldIdx >= calleeParamLocals.length) oldIdx + localVarShift
      else {
        val newIdx = calleeParamLocals(oldIdx)
        if (newIdx >= numCallsiteLocals) newIdx
        else -1 // don't copy a local variable entry for params where an existing local of the callsite is re-used
      }
    }
    callsiteMethod.localVariables.addAll(cloneLocalVariableNodes(callee, labelsMap, callee.name, localIndexMap).asJava)
    // prepend the handlers of the callee. the order of handlers matters: when an exception is thrown
    // at some instruction, the first handler guarding that instruction and having a matching exception
    // type is executed. prepending the callee's handlers makes sure to test those handlers first if
    // an exception is thrown in the inlined code.
    callsiteMethod.tryCatchBlocks.addAll(0, cloneTryCatchBlockNodes(callee, labelsMap).asJava)

    callsiteMethod.maxLocals = BackendUtils.maxLocals(callsiteMethod) + BackendUtils.maxLocals(callee) - numSavedParamSlots + returnType.getSize
    val maxStackOfInlinedCode = {
      // One slot per value is correct for long / double, see comment in the `analysis` package object.
      val numStoredArgs = calleeParamTypes.length + (if (isStatic) 0 else 1)
      BackendUtils.maxStack(callee) + callsiteStackHeight - numStoredArgs
    }
    val stackHeightAtNullCheck = {
      val stackSlotForNullCheck =
        if (!skipReceiverNullCheck && calleeParamTypes.isEmpty) {
          // When adding a null check for the receiver, a DUP is inserted, which might cause a new maxStack.
          // If the callsite has other argument values than the receiver on the stack, these are pop'ed
          // and stored into locals before the null check, so in that case the maxStack doesn't grow.
          1
        }
        else if (hasNullOutInsn) {
          // after the return value is loaded, local variables and the return local variable are
          // nulled out, which means `null` is loaded to the stack. the max stack height is the
          // callsite stack height +1 (receiver consumed, result produced, null loaded), but +2
          // for static calls
          if (isStatic) 2 else 1
        }
        else 0
      callsiteStackHeight + stackSlotForNullCheck
    }

    callsiteMethod.maxStack = math.max(BackendUtils.maxStack(callsiteMethod), math.max(stackHeightAtNullCheck, maxStackOfInlinedCode))

    lazy val callsiteLambdaBodyMethods = onIndyLambdaImplMethod(callsiteClass.internalName)(_.getOrElseUpdate(callsiteMethod, mutable.Map.empty))
    onIndyLambdaImplMethodIfPresent(calleeDeclarationClass.internalName)(methods => methods.getOrElse(callee, Nil) foreach {
      case (indy, handle) => instructionMap.get(indy) match {
        case Some(clonedIndy: InvokeDynamicInsnNode) =>
          callsiteLambdaBodyMethods(clonedIndy) = handle
        case _ =>
      }
    })

    // Don't remove the inlined instruction from callsitePositions, inlineAnnotatedCallsites so that
    // the information is still there in case the method is rolled back (UndoLog).

    if (updateCallGraph) callGraph.refresh(callsiteMethod, callsiteClass)

    // Inlining a method body can render some code unreachable, see example above in this method.
    BackendUtils.clearDceDone(callsiteMethod)

    instructionMap
  }

  /**
   * Check whether an inlining can be performed. This method performs tests that don't change even
   * if the body of the callee is changed by the inliner / optimizer, so it can be used early
   * (when looking at the call graph and collecting inline requests for the program).
   *
   * The tests that inspect the callee's instructions are implemented in method `canInlineBody`,
   * which is queried when performing an inline.
   *
   * @return `Some(message)` if inlining cannot be performed, `None` otherwise
   */
  def earlyCanInlineCheck(callsite: Callsite): Option[CannotInlineWarning] = {
    import callsite.{callsiteClass, callsiteMethod}
    val Right(callsiteCallee) = callsite.callee: @unchecked
    import callsiteCallee.{callee, calleeDeclarationClass}

    if (isSynchronizedMethod(callee)) {
      // Could be done by locking on the receiver, wrapping the inlined code in a try and unlocking
      // in finally. But it's probably not worth the effort, scala never emits synchronized methods.
      Some(SynchronizedMethod(calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated))
    } else if (isStrictfpMethod(callsiteMethod) != isStrictfpMethod(callee)) {
      Some(StrictfpMismatch(
        calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
        callsiteClass.internalName, callsiteMethod.name, callsiteMethod.desc))
    } else
      None
  }

  /**
   * Check whether the body of the callee contains any instructions that prevent the callsite from
   * being inlined. See also method `earlyCanInlineCheck`.
   *
   * The result of this check depends on changes to the callee method's body. For example, if the
   * callee initially invokes a private method, it cannot be inlined into a different class. If the
   * private method is inlined into the callee, inlining the callee becomes possible. Therefore
   * we don't query it while traversing the call graph and selecting callsites to inline - it might
   * rule out callsites that can be inlined just fine.
   *
   * Returns
   *  - `None` if the callsite can be inlined
   *  - `Some((message, Nil))` if there was an issue performing the access checks, for example
   *    because of a missing classfile
   *  - `Some((message, instructions))` if inlining `instructions` into the callsite method would
   *    cause an IllegalAccessError
   */
  def canInlineCallsite(callsite: Callsite): Option[CannotInlineWarning] = {
    import callsite.{callsiteClass, callsiteInstruction, callsiteMethod, callsiteStackHeight}
    val Right(callsiteCallee) = callsite.callee: @unchecked
    import callsiteCallee.{callee, calleeDeclarationClass}

    def calleeDesc = s"${callee.name} of type ${callee.desc} in ${calleeDeclarationClass.internalName}"
    def methodMismatch = s"Wrong method node for inlining ${textify(callsiteInstruction)}: $calleeDesc"
    assert(callsiteInstruction.name == callee.name, methodMismatch)
    assert(callsiteInstruction.desc == callee.desc, methodMismatch)
    assert(!isConstructor(callee), s"Constructors cannot be inlined: $calleeDesc")
    assert(!BytecodeUtils.isAbstractMethod(callee), s"Callee is abstract: $calleeDesc")
    assert(callsiteMethod.instructions.contains(callsiteInstruction), s"Callsite ${textify(callsiteInstruction)} is not an instruction of $callsiteClass.${callsiteMethod.name}${callsiteMethod.desc}")

    // When an exception is thrown, the stack is cleared before jumping to the handler. When
    // inlining a method that catches an exception, all values that were on the stack before the
    // call (in addition to the arguments) would be cleared (scala/bug#6157). So we don't inline methods
    // with handlers in case there are values on the stack.
    // Alternatively, we could save all stack values below the method arguments into locals, but
    // that would be inefficient: we'd need to pop all parameters, save the values, and push the
    // parameters back for the (inlined) invocation. Similarly for the result after the call.
    def stackHasNonParameters: Boolean = {
      val expectedArgs = asm.Type.getArgumentTypes(callsiteInstruction.desc).length + (callsiteInstruction.getOpcode match {
        case INVOKEVIRTUAL | INVOKESPECIAL | INVOKEINTERFACE => 1
        case INVOKESTATIC => 0
        case INVOKEDYNAMIC =>
          assertionError(s"Unexpected opcode, cannot inline ${textify(callsiteInstruction)}")
      })
      callsiteStackHeight > expectedArgs
    }

    if (callsiteTooLargeAfterInlining(callsiteMethod, callee)) {
      val warning = ResultingMethodTooLarge(
        calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
        callsiteClass.internalName, callsiteMethod.name, callsiteMethod.desc)
      Some(warning)
    } else if (!callee.tryCatchBlocks.isEmpty && stackHasNonParameters) {
      val warning = MethodWithHandlerCalledOnNonEmptyStack(
        calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
        callsiteClass.internalName, callsiteMethod.name, callsiteMethod.desc)
      Some(warning)
    } else findIllegalAccess(callee.instructions, calleeDeclarationClass, callsiteClass) match {
      case Right(Nil) =>
        None

      case Right(illegalAccessInsns) =>
        val warning = IllegalAccessInstructions(
          calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
          callsiteClass.internalName, illegalAccessInsns)
        Some(warning)

      case Left((illegalAccessIns, cause)) =>
        val warning = IllegalAccessCheckFailed(
          calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
          callsiteClass.internalName, illegalAccessIns, cause)
        Some(warning)
    }
  }

  /**
   * Check if a type is accessible to some class, as defined in JVMS 5.4.4.
   *  (A1) C is public
   *  (A2) C and D are members of the same run-time package
   */
  @tailrec
  final def classIsAccessible(accessed: BType, from: ClassBType): Either[OptimizerWarning, Boolean] = (accessed: @unchecked) match {
    // TODO: A2 requires "same run-time package", which seems to be package + classloader (JVMS 5.3.). is the below ok?
    case c: ClassBType     => c.isPublic.map(_ || c.packageInternalName == from.packageInternalName)
    case a: ArrayBType     => classIsAccessible(a.elementType, from)
    case _: PrimitiveBType => Right(true)
  }

  /**
   * Check if a member reference is accessible from the `destinationClass`, as defined in the
   * JVMS 5.4.4. Note that the class name in a field / method reference is not necessarily the
   * class in which the member is declared:
   *
   *   class A { def f = 0 }; class B extends A { f }
   *
   * The INVOKEVIRTUAL instruction uses a method reference "B.f ()I". Therefore this method has
   * two parameters:
   *
   * @param memberDeclClass The class in which the member is declared (A)
   * @param memberRefClass  The class used in the member reference (B)
   *
   * (B0) JVMS 5.4.3.2 / 5.4.3.3: when resolving a member of class C in D, the class C is resolved
   * first. According to 5.4.3.1, this requires C to be accessible in D.
   *
   * JVMS 5.4.4 summary: A field or method R is accessible to a class D (destinationClass) iff
   *  (B1) R is public
   *  (B2) R is protected, declared in C (memberDeclClass) and D is a subclass of C.
   *       If R is not static, R must contain a symbolic reference to a class T (memberRefClass),
   *       such that T is either a subclass of D, a superclass of D, or D itself.
   *       Also (P) needs to be satisfied.
   *  (B3) R is either protected or has default access and declared by a class in the same
   *       run-time package as D.
   *       If R is protected, also (P) needs to be satisfied.
   *  (B4) R is private and is declared in D.
   *
   *  (P) When accessing a protected instance member, the target object on the stack (the receiver)
   *      has to be a subtype of D (destinationClass). This is enforced by classfile verification
   *      (https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.10.1.8).
   *
   * TODO: we cannot currently implement (P) because we don't have the necessary information
   * available. Once we have a type propagation analysis implemented, we can extract the receiver
   * type from there (https://github.com/scala-opt/scala/issues/13).
   */
  def memberIsAccessible(memberFlags: Int, memberDeclClass: ClassBType, memberRefClass: ClassBType, from: ClassBType): Either[OptimizerWarning, Boolean] = {
    // TODO: B3 requires "same run-time package", which seems to be package + classloader (JVMS 5.3.). is the below ok?
    def samePackageAsDestination = memberDeclClass.packageInternalName == from.packageInternalName
    def targetObjectConformsToDestinationClass = false // needs type propagation analysis, see above

    def memberIsAccessibleImpl = {
      val key = (ACC_PUBLIC | ACC_PROTECTED | ACC_PRIVATE) & memberFlags
      key match {
        case ACC_PUBLIC => // B1
          Right(true)

        case ACC_PROTECTED => // B2
          val isStatic = (ACC_STATIC & memberFlags) != 0
          tryEither {
            val condB2 = from.isSubtypeOf(memberDeclClass).orThrow && {
              isStatic || memberRefClass.isSubtypeOf(from).orThrow || from.isSubtypeOf(memberRefClass).orThrow
            }
            Right(
              (condB2 || samePackageAsDestination /* B3 (protected) */) &&
              (isStatic || targetObjectConformsToDestinationClass) // (P)
            )
          }

        case 0 => // B3 (default access)
          Right(samePackageAsDestination)

        case ACC_PRIVATE => // B4
          Right(memberDeclClass == from)
      }
    }

    classIsAccessible(memberDeclClass, from) match { // B0
      case Right(true) => memberIsAccessibleImpl
      case r => r
    }
  }

  /**
   * Returns
   *   - `Right(Nil)` if all instructions can be safely inlined
   *   - `Right(insns)` if inlining any of `insns` would cause a [[java.lang.IllegalAccessError]]
   *     when inlined into the `destinationClass`
   *   - `Left((insn, warning))` if validity of some instruction could not be checked because an
   *     error occurred
   */
  def findIllegalAccess(instructions: InsnList, calleeDeclarationClass: ClassBType, destinationClass: ClassBType): Either[(AbstractInsnNode, OptimizerWarning), List[AbstractInsnNode]] = {
    /*
     * Check if `instruction` can be transplanted to `destinationClass`.
     *
     * If the instruction references a class, method or field that cannot be found in the
     * byteCodeRepository, it is considered as not legal. This is known to happen in mixed
     * compilation: for Java classes there is no classfile that could be parsed, nor does the
     * compiler generate any bytecode.
     *
     * Returns a warning message describing the problem if checking the legality for the instruction
     * failed.
     */
    def isLegal(instruction: AbstractInsnNode): Either[OptimizerWarning, Boolean] = instruction match {
      case ti: TypeInsnNode  =>
        // NEW, ANEWARRAY, CHECKCAST or INSTANCEOF. For these instructions, the reference
        // "must be a symbolic reference to a class, array, or interface type" (JVMS 6), so
        // it can be an internal name, or a full array descriptor.
        classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(ti.desc), destinationClass)

      case ma: MultiANewArrayInsnNode =>
        // "a symbolic reference to a class, array, or interface type"
        classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(ma.desc), destinationClass)

      case fi: FieldInsnNode =>
        val fieldRefClass = classBTypeFromParsedClassfile(fi.owner)
        for {
          (fieldNode, fieldDeclClassNode) <- byteCodeRepository.fieldNode(fieldRefClass.internalName, fi.name, fi.desc): Either[OptimizerWarning, (FieldNode, InternalName)]
          fieldDeclClass                  =  classBTypeFromParsedClassfile(fieldDeclClassNode)
          res                             <- memberIsAccessible(fieldNode.access, fieldDeclClass, fieldRefClass, destinationClass)
        } yield {
          // ensure the result ClassBType is cached (for stack map frame calculation)
          if (res) bTypeForDescriptorFromClassfile(fi.desc)
          res
        }

      case mi: MethodInsnNode =>
        if (mi.owner.charAt(0) == '[') {
          // ensure the result ClassBType is cached (for stack map frame calculation)
          if (mi.name == "getClass") bTypeForDescriptorFromClassfile("Ljava/lang/Class;")
          // array methods are accessible
          Right(true)
        } else {
          def canInlineCall(opcode: Int, methodFlags: Int, methodDeclClass: ClassBType, methodRefClass: ClassBType): Either[OptimizerWarning, Boolean] = {
            opcode match {
              case INVOKESPECIAL if mi.name != GenBCode.INSTANCE_CONSTRUCTOR_NAME =>
                // invokespecial is used for private method calls, super calls and instance constructor calls.
                // private method and super calls can only be inlined into the same class.
                Right(destinationClass == calleeDeclarationClass)

              case _ => // INVOKEVIRTUAL, INVOKESTATIC, INVOKEINTERFACE and INVOKESPECIAL of constructors
                memberIsAccessible(methodFlags, methodDeclClass, methodRefClass, destinationClass)
            }
          }

          val methodRefClass = classBTypeFromParsedClassfile(mi.owner)
          for {
            (methodNode, methodDeclClassNode) <- byteCodeRepository.methodNode(methodRefClass.internalName, mi.name, mi.desc): Either[OptimizerWarning, (MethodNode, InternalName)]
            methodDeclClass                   =  classBTypeFromParsedClassfile(methodDeclClassNode)
            res                               <- canInlineCall(mi.getOpcode, methodNode.access, methodDeclClass, methodRefClass)
          } yield {
            // ensure the result ClassBType is cached (for stack map frame calculation)
            if (res) bTypeForDescriptorFromClassfile(Type.getReturnType(mi.desc).getDescriptor)
            res
          }
        }

      case _: InvokeDynamicInsnNode if destinationClass == calleeDeclarationClass =>
        // Within the same class, any indy instruction can be inlined. Since that class is currently
        // being emitted, we don't need to worry about caching BTypes (for stack map frame calculation).
        // The necessary BTypes were cached during code gen.
        Right(true)

      // does the InvokeDynamicInsnNode call LambdaMetaFactory?
      case LambdaMetaFactoryCall(indy, _, implMethod, _, _) =>
        // an indy instr points to a "call site specifier" (CSP) [1]
        //  - a reference to a bootstrap method [2]
        //    - bootstrap method name
        //    - references to constant arguments, which can be:
        //      - constant (string, long, int, float, double)
        //      - class
        //      - method type (without name)
        //      - method handle
        //  - a method name+type
        //
        // execution [3]
        //  - resolve the CSP, yielding the bootstrap method handle, the static args and the name+type
        //    - resolution entails accessibility checking [4]
        //  - execute the `invoke` method of the bootstrap method handle (which is signature polymorphic, check its javadoc)
        //    - the descriptor for the call is made up from the actual arguments on the stack:
        //      - the first parameters are "MethodHandles.Lookup, String, MethodType", then the types of the constant arguments,
        //      - the return type is CallSite
        //    - the values for the call are
        //      - the bootstrap method handle of the CSP is the receiver
        //      - the Lookup object for the class in which the callsite occurs (obtained as through calling MethodHandles.lookup())
        //      - the method name of the CSP
        //      - the method type of the CSP
        //      - the constants of the CSP (primitives are not boxed)
        //  - the resulting `CallSite` object
        //    - has as `type` the method type of the CSP
        //    - is popped from the operand stack
        //  - the `invokeExact` method (signature polymorphic!) of the `target` method handle of the CallSite is invoked
        //    - the method descriptor is that of the CSP
        //    - the receiver is the target of the CallSite
        //    - the other argument values are those that were on the operand stack at the indy instruction (indyLambda: the captured values)
        //
        // [1] https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.10
        // [2] https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.23
        // [3] https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.invokedynamic
        // [4] https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-5.html#jvms-5.4.3

        // We cannot generically check if an `invokedynamic` instruction can be safely inlined into
        // a different class, that depends on the bootstrap method. The Lookup object passed to the
        // bootstrap method is a capability to access private members of the callsite class. We can
        // only move the invokedynamic to a new class if we know that the bootstrap method doesn't
        // use this capability for otherwise non-accessible members.
        // In the case of indyLambda, it depends on the visibility of the implMethod handle. If
        // the implMethod is public, lambdaMetaFactory doesn't use the Lookup object's extended
        // capability, and we can safely inline the instruction into a different class.

        val methodRefClass = classBTypeFromParsedClassfile(implMethod.getOwner)
        for {
          (methodNode, methodDeclClassNode) <- byteCodeRepository.methodNode(methodRefClass.internalName, implMethod.getName, implMethod.getDesc): Either[OptimizerWarning, (MethodNode, InternalName)]
          methodDeclClass                   =  classBTypeFromParsedClassfile(methodDeclClassNode)
          res                               <- memberIsAccessible(methodNode.access, methodDeclClass, methodRefClass, destinationClass)
        } yield {
          // ensure the result ClassBType is cached (for stack map frame calculation)
          if (res) bTypeForDescriptorFromClassfile(Type.getReturnType(indy.desc).getDescriptor)
          res
        }

      case _: InvokeDynamicInsnNode => Left(UnknownInvokeDynamicInstruction)

      case ci: LdcInsnNode => ci.cst match {
        case t: asm.Type => classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(t.getInternalName), destinationClass)
          // TODO: method handle -- check if method accessible?
        case _           => Right(true)
      }

      case _ => Right(true)
    }

    val it = instructions.iterator.asScala
    val illegalAccess = mutable.ListBuffer.empty[AbstractInsnNode]
    while (it.hasNext) {
      val i = it.next()
      isLegal(i) match {
        case Left(warning) => return Left((i, warning)) // checking isLegal for i failed
        case Right(false)  => illegalAccess += i        // an illegal instruction was found
        case _ =>
      }
    }
    Right(illegalAccess.toList)
  }
}
