/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.tailrec
import scala.tools.asm
import asm.Opcodes._
import asm.tree._
import scala.collection.JavaConverters._
import AsmUtils._
import BytecodeUtils._
import collection.mutable
import BackendReporting._
import scala.tools.nsc.backend.jvm.BTypes.InternalName

class Inliner[BT <: BTypes](val btypes: BT) {
  import btypes._
  import callGraph._
  import inlinerHeuristics._
  import backendUtils._

  sealed trait InlineLog {
    def request: InlineRequest
  }
  final case class InlineLogSuccess(request: InlineRequest, sizeBefore: Int, sizeInlined: Int) extends InlineLog {
    var downstreamLog: mutable.Buffer[InlineLog] = mutable.ListBuffer.empty
  }
  final case class InlineLogFail(request: InlineRequest, warning: CannotInlineWarning) extends InlineLog
  final case class InlineLogRollback(request: InlineRequest, warnings: List[CannotInlineWarning]) extends InlineLog

  object InlineLog {
    private def shouldLog(request: InlineRequest): Boolean = {
      def logEnabled = compilerSettings.YoptLogInline.isSetByUser
      def matchesName = {
        val prefix = compilerSettings.YoptLogInline.value match {
          case "_" => ""
          case p => p
        }
        val name: String = request.callsite.callsiteClass.internalName + "." + request.callsite.callsiteMethod.name
        name startsWith prefix
      }
      logEnabled && (upstream != null || (isTopLevel && matchesName))
    }

    // indexed by callsite method
    private val logs = mutable.Map.empty[MethodNode, mutable.LinkedHashSet[InlineLog]]

    private var upstream: InlineLogSuccess = _
    private var isTopLevel = true

    def withInlineLogging[T](request: InlineRequest)(inlineRequest: => Unit)(inlinePost: => T): T = {
      def doInlinePost(): T = {
        val savedIsTopLevel = isTopLevel
        isTopLevel = false
        try inlinePost
        finally isTopLevel = savedIsTopLevel
      }
      if (shouldLog(request)) {
        val sizeBefore = request.callsite.callsiteMethod.instructions.size
        inlineRequest
        val log = InlineLogSuccess(request, sizeBefore, request.callsite.callee.get.callee.instructions.size)
        apply(log)

        val savedUpstream = upstream
        upstream = log
        try doInlinePost()
        finally upstream = savedUpstream
      } else {
        inlineRequest
        doInlinePost()
      }
    }

    def apply(log: => InlineLog): Unit = if (shouldLog(log.request)) {
      if (upstream != null) upstream.downstreamLog += log
      else {
        val methodLogs = logs.getOrElseUpdate(log.request.callsite.callsiteMethod, mutable.LinkedHashSet.empty)
        methodLogs += log
      }
    }

    def entryString(log: InlineLog, indent: Int = 0): String = {
      val callee = log.request.callsite.callee.get
      val calleeString = callee.calleeDeclarationClass.internalName + "." + callee.callee.name
      val indentString = " " * indent
      log match {
        case s @ InlineLogSuccess(_, sizeBefore, sizeInlined) =>
          val self = s"${indentString}inlined $calleeString. Before: $sizeBefore ins, inlined: $sizeInlined ins."
          if (s.downstreamLog.isEmpty) self
          else s.downstreamLog.iterator.map(entryString(_, indent + 2)).mkString(self + "\n", "\n", "")

        case InlineLogFail(_, w) =>
          s"${indentString}failed $calleeString. ${w.toString.replace('\n', ' ')}"

        case InlineLogRollback(_, _) =>
          s"${indentString}rolling back, nested inline failed."
      }
    }

    def print(): Unit = if (compilerSettings.YoptLogInline.isSetByUser) {
      val byClassAndMethod: List[(InternalName, mutable.Map[MethodNode, mutable.LinkedHashSet[InlineLog]])] = {
        logs.
          groupBy(_._2.head.request.callsite.callsiteClass.internalName).
          toList.sortBy(_._1)
      }
      for {
        (c, methodLogs) <- byClassAndMethod
        (m, mLogs) <- methodLogs.toList.sortBy(_._1.name)
        mLog <- mLogs // insertion order
      } {
        println(s"Inline into $c.${m.name}: ${entryString(mLog)}")
      }
    }
  }

  def runInliner(): Unit = {
    for (request <- collectAndOrderInlineRequests) {
      val Right(callee) = request.callsite.callee // collectAndOrderInlineRequests returns callsites with a known callee
      val warnings = inline(request)
      for (warning <- warnings) {
        if (warning.emitWarning(compilerSettings))
          backendReporting.inlinerWarning(request.callsite.callsitePosition, warning.toString)
      }
    }
    InlineLog.print()
  }

  /**
   * Ordering for inline requests. Required to make the inliner deterministic:
   *   - Always remove the same request when breaking inlining cycles
   *   - Perform inlinings in a consistent order
   */
  object callsiteOrdering extends Ordering[InlineRequest] {
    override def compare(x: InlineRequest, y: InlineRequest): Int = {
      val xCs = x.callsite
      val yCs = y.callsite
      val cls = xCs.callsiteClass.internalName compareTo yCs.callsiteClass.internalName
      if (cls != 0) return cls

      val name = xCs.callsiteMethod.name compareTo yCs.callsiteMethod.name
      if (name != 0) return name

      val desc = xCs.callsiteMethod.desc compareTo yCs.callsiteMethod.desc
      if (desc != 0) return desc

      def pos(c: Callsite) = c.callsiteMethod.instructions.indexOf(c.callsiteInstruction)
      pos(xCs) - pos(yCs)
    }
  }

  /**
   * Returns the callsites that can be inlined. Ensures that the returned inline request graph does
   * not contain cycles.
   *
   * The resulting list is sorted such that the leaves of the inline request graph are on the left.
   * Once these leaves are inlined, the successive elements will be leaves, etc.
   */
  private def collectAndOrderInlineRequests: List[InlineRequest] = {
    val requestsByMethod = selectCallsitesForInlining withDefaultValue Set.empty

    val elided = mutable.Set.empty[InlineRequest]
    def nonElidedRequests(methodNode: MethodNode): Set[InlineRequest] = requestsByMethod(methodNode) diff elided

    def allCallees(r: InlineRequest): Set[MethodNode] = r.post.flatMap(allCallees).toSet + r.callsite.callee.get.callee

    /**
     * Break cycles in the inline request graph by removing callsites.
     *
     * The list `requests` is traversed left-to-right, removing those callsites that are part of a
     * cycle. Elided callsites are also removed from the `inlineRequestsForMethod` map.
     */
    def breakInlineCycles: List[InlineRequest] = {
      // is there a path of inline requests from start to goal?
      def isReachable(start: Set[MethodNode], goal: MethodNode): Boolean = {
        @tailrec def reachableImpl(check: Set[MethodNode], visited: Set[MethodNode]): Boolean = {
          if (check.isEmpty) false
          else {
            val x = check.head
            if (x == goal) true
            else if (visited(x)) reachableImpl(check - x, visited)
            else {
              val callees = nonElidedRequests(x).flatMap(allCallees)
              reachableImpl(check - x ++ callees, visited + x)
            }
          }
        }
        reachableImpl(start, Set.empty)
      }

      val result = new mutable.ListBuffer[InlineRequest]()
      val requests = requestsByMethod.valuesIterator.flatten.toArray
      // sort the inline requests to ensure that removing requests is deterministic
      java.util.Arrays.sort(requests, callsiteOrdering)
      for (r <- requests) {
        // is there a chain of inlining requests that would inline the callsite method into the callee?
        if (isReachable(allCallees(r), r.callsite.callsiteMethod))
          elided += r
        else
          result += r
        ()
      }
      result.toList
    }

    // sort the remaining inline requests such that the leaves appear first, then those requests
    // that become leaves, etc.
    def leavesFirst(requests: List[InlineRequest], visited: Set[InlineRequest] = Set.empty): List[InlineRequest] = {
      if (requests.isEmpty) Nil
      else {
        val (leaves, others) = requests.partition(r => {
          val inlineRequestsForCallees = allCallees(r).flatMap(nonElidedRequests)
          inlineRequestsForCallees.forall(visited)
        })
        assert(leaves.nonEmpty, requests)
        leaves ::: leavesFirst(others, visited ++ leaves)
      }
    }

    leavesFirst(breakInlineCycles)
  }

  /**
   * Given an InlineRequest(mainCallsite, post = List(postCallsite)), the postCallsite is a callsite
   * in the method `mainCallsite.callee`. Once the mainCallsite is inlined into the target method
   * (mainCallsite.callsiteMethod), we need to find the cloned callsite that corresponds to the
   * postCallsite so we can inline that into the target method as well.
   *
   * However, it is possible that there is no cloned callsite at all that corresponds to the
   * postCallsite, for example if the corresponding callsite already inlined. Example:
   *
   *   def a() = 1
   *   def b() = a() + 2
   *   def c() = b() + 3
   *   def d() = c() + 4
   *
   * We have the following callsite objects in the call graph:
   *
   *   c1 = a() in b
   *   c2 = b() in c
   *   c3 = c() in d
   *
   * Assume we have the following inline request
   *   r = InlineRequest(c3,
   *         post = List(InlineRequest(c2,
   *           post = List(InlineRequest(c1, post = Nil)))))
   *
   * But before inlining r, assume a separate InlineRequest(c2, post = Nil) is inlined first. We get
   *
   *   c1' = a() in c                  // added to the call graph
   *   c1.inlinedClones += (c1' at c2) // remember that c1' was created when inlining c2
   *   ~c2~                            // c2 is removed from the call graph
   *
   * If we now inline r, we first inline c3. We get
   *
   *   c1'' = a() in d                   // added to call graph
   *   c1'.inlinedClones += (c1'' at c3) // remember that c1'' was created when inlining c3
   *   ~c3~
   *
   * Now we continue with the post-requests for r, i.e. c2.
   *   - we try to find the clone of c2 that was created when inlining c3 - but there is none. c2
   *     was already inlined before
   *   - we continue with the post-request of c2: c1
   *     - we search for the callsite of c1 that was cloned when inlining c2, we find c1'
   *     - recursively we search for the callsite of c1' that was cloned when inlining c3, we find c1''
   *     - so we create an inline request for c1''
   */
  def adaptPostRequestForMainCallsite(post: InlineRequest, mainCallsite: Callsite): List[InlineRequest] = {
    def impl(post: InlineRequest, at: Callsite): List[InlineRequest] = {
      post.callsite.inlinedClones.find(_.clonedWhenInlining == at) match {
        case Some(clonedCallsite) =>
          List(InlineRequest(clonedCallsite.callsite, post.post, post.reason))
        case None =>
          post.post.flatMap(impl(_, post.callsite)).flatMap(impl(_, at))
      }
    }
    impl(post, mainCallsite)
  }

  class UndoLog(active: Boolean = true) {
    import java.util.{ ArrayList => JArrayList }

    private var actions = List.empty[() => Unit]
    private var methodStateSaved = false

    def apply(a: => Unit): Unit = if (active) actions = (() => a) :: actions
    def rollback(): Unit = if (active) actions.foreach(_.apply())

    def saveMethodState(methodNode: MethodNode): Unit = if (active && !methodStateSaved) {
      methodStateSaved = true
      val currentInstructions = methodNode.instructions.toArray
      val currentLocalVariables = new JArrayList(methodNode.localVariables)
      val currentTryCatchBlocks = new JArrayList(methodNode.tryCatchBlocks)
      val currentMaxLocals = methodNode.maxLocals
      val currentMaxStack = methodNode.maxStack

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
      }
    }
  }

  val NoUndoLogging = new UndoLog(active = false)

  /**
   * Inline the callsite of an inlining request and its post-inlining requests.
   *
   * @return An inliner warning for each callsite that could not be inlined.
   */
  def inline(request: InlineRequest, undo: UndoLog = NoUndoLogging): List[CannotInlineWarning] = {
    def doInline(undo: UndoLog, callRollback: Boolean = false): List[CannotInlineWarning] = {
      InlineLog.withInlineLogging(request) {
        inlineCallsite(request.callsite, undo)
      } {
        val postRequests = request.post.flatMap(adaptPostRequestForMainCallsite(_, request.callsite))
        val warnings = postRequests.flatMap(inline(_, undo))
        if (callRollback && warnings.nonEmpty) {
          undo.rollback()
          InlineLog(InlineLogRollback(request, warnings))
        }
        warnings
      }
    }

    def inlinedByPost(insns: List[AbstractInsnNode]): Boolean =
      insns.nonEmpty && insns.forall(ins => request.post.exists(_.callsite.callsiteInstruction == ins))

    canInlineCallsite(request.callsite) match {
      case None =>
        doInline(undo)

      case Some((_, illegalAccessInsns)) if inlinedByPost(illegalAccessInsns) =>
        // speculatively inline, roll back if an illegalAccessInsn cannot be eliminated
        if (undo == NoUndoLogging) doInline(new UndoLog(), callRollback = true)
        else doInline(undo)

      case Some((w, _)) =>
        InlineLog(InlineLogFail(request, w))
        List(w)
    }
  }

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
  def inlineCallsite(callsite: Callsite, undo: UndoLog = NoUndoLogging): Unit = {
    import callsite.{callsiteClass, callsiteMethod, callsiteInstruction, receiverKnownNotNull, callsiteStackHeight}
    val Right(callsiteCallee) = callsite.callee
    import callsiteCallee.{callee, calleeDeclarationClass, sourceFilePath}

    // Inlining requires the callee not to have unreachable code, the analyzer used below should not
    // return any `null` frames. Note that inlining a method can create unreachable code. Example:
    //   def f = throw e
    //   def g = f; println() // println is unreachable after inlining f
    // If we have an inline request for a call to g, and f has been already inlined into g, we
    // need to run DCE on g's body before inlining g.
    localOpt.minimalRemoveUnreachableCode(callee, calleeDeclarationClass.internalName)

    // If the callsite was eliminated by DCE, do nothing.
    if (!callGraph.containsCallsite(callsite)) return

    // New labels for the cloned instructions
    val labelsMap = cloneLabels(callee)
    val sameSourceFile = sourceFilePath match {
      case Some(calleeSource) => byteCodeRepository.compilingClasses.get(callsiteClass.internalName) match {
        case Some((_, `calleeSource`)) => true
        case _ => false
      }
      case _ => false
    }
    val (clonedInstructions, instructionMap, targetHandles) = cloneInstructions(callee, labelsMap, keepLineNumbers = sameSourceFile)

    // local vars in the callee are shifted by the number of locals at the callsite
    val localVarShift = callsiteMethod.maxLocals
    clonedInstructions.iterator.asScala foreach {
      case varInstruction: VarInsnNode => varInstruction.`var` += localVarShift
      case iinc: IincInsnNode          => iinc.`var` += localVarShift
      case _ => ()
    }

    // add a STORE instruction for each expected argument, including for THIS instance if any
    val argStores = new InsnList
    var nextLocalIndex = callsiteMethod.maxLocals
    if (!isStaticMethod(callee)) {
      if (!receiverKnownNotNull) {
        argStores.add(new InsnNode(DUP))
        val nonNullLabel = newLabelNode
        argStores.add(new JumpInsnNode(IFNONNULL, nonNullLabel))
        argStores.add(new InsnNode(ACONST_NULL))
        argStores.add(new InsnNode(ATHROW))
        argStores.add(nonNullLabel)
      }
      argStores.add(new VarInsnNode(ASTORE, nextLocalIndex))
      nextLocalIndex += 1
    }

    // We just use an asm.Type here, no need to create the MethodBType.
    val calleAsmType = asm.Type.getMethodType(callee.desc)
    val calleeParamTypes = calleAsmType.getArgumentTypes

    for(argTp <- calleeParamTypes) {
      val opc = argTp.getOpcode(ISTORE) // returns the correct xSTORE instruction for argTp
      argStores.insert(new VarInsnNode(opc, nextLocalIndex)) // "insert" is "prepend" - the last argument is on the top of the stack
      nextLocalIndex += argTp.getSize
    }

    clonedInstructions.insert(argStores)

    // label for the exit of the inlined functions. xRETURNs are replaced by GOTOs to this label.
    val postCallLabel = newLabelNode
    clonedInstructions.add(postCallLabel)

    // replace xRETURNs:
    //   - store the return value (if any)
    //   - clear the stack of the inlined method (insert DROPs)
    //   - load the return value
    //   - GOTO postCallLabel

    val returnType = calleAsmType.getReturnType
    val hasReturnValue = returnType.getSort != asm.Type.VOID
    val returnValueIndex = callsiteMethod.maxLocals + callee.maxLocals
    nextLocalIndex += returnType.getSize

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
    val analyzer = new AsmAnalyzer(callee, calleeDeclarationClass.internalName)

    for (originalReturn <- callee.instructions.iterator().asScala if isReturn(originalReturn)) {
      val frame = analyzer.frameAt(originalReturn)
      var stackHeight = frame.getStackSize

      val inlinedReturn = instructionMap(originalReturn)
      val returnReplacement = new InsnList

      def drop(slot: Int) = returnReplacement add getPop(frame.peekStack(slot).getSize)

      // for non-void methods, store the stack top into the return local variable
      if (hasReturnValue) {
        returnReplacement add returnValueStore(originalReturn)
        stackHeight -= 1
      }

      // drop the rest of the stack
      for (i <- 0 until stackHeight) drop(i)

      returnReplacement add new JumpInsnNode(GOTO, postCallLabel)
      clonedInstructions.insert(inlinedReturn, returnReplacement)
      clonedInstructions.remove(inlinedReturn)
    }

    // Load instruction for the return value
    if (hasReturnValue) {
      val retVarLoad = {
        val opc = returnType.getOpcode(ILOAD)
        new VarInsnNode(opc, returnValueIndex)
      }
      clonedInstructions.insert(postCallLabel, retVarLoad)
    }

    undo.saveMethodState(callsiteMethod)

    callsiteMethod.instructions.insert(callsiteInstruction, clonedInstructions)
    callsiteMethod.instructions.remove(callsiteInstruction)

    callsiteMethod.localVariables.addAll(cloneLocalVariableNodes(callee, labelsMap, callee.name, localVarShift).asJava)
    // prepend the handlers of the callee. the order of handlers matters: when an exception is thrown
    // at some instruction, the first handler guarding that instruction and having a matching exception
    // type is executed. prepending the callee's handlers makes sure to test those handlers first if
    // an exception is thrown in the inlined code.
    callsiteMethod.tryCatchBlocks.addAll(0, cloneTryCatchBlockNodes(callee, labelsMap).asJava)

    callsiteMethod.maxLocals += returnType.getSize + callee.maxLocals
    val maxStackOfInlinedCode = {
      // One slot per value is correct for long / double, see comment in the `analysis` package object.
      val numStoredArgs = calleeParamTypes.length + (if (isStaticMethod(callee)) 0 else 1)
      callee.maxStack + callsiteStackHeight - numStoredArgs
    }
    val stackHeightAtNullCheck = {
      // When adding a null check for the receiver, a DUP is inserted, which might cause a new maxStack.
      // If the callsite has other argument values than the receiver on the stack, these are pop'ed
      // and stored into locals before the null check, so in that case the maxStack doesn't grow.
      val stackSlotForNullCheck = if (!isStaticMethod(callee) && !receiverKnownNotNull && calleeParamTypes.isEmpty) 1 else 0
      callsiteStackHeight + stackSlotForNullCheck
    }

    callsiteMethod.maxStack = math.max(callsiteMethod.maxStack, math.max(stackHeightAtNullCheck, maxStackOfInlinedCode))

    val added = addIndyLambdaImplMethod(callsiteClass.internalName, targetHandles)
    undo { removeIndyLambdaImplMethod(callsiteClass.internalName, added) }

    callGraph.addIfMissing(callee, calleeDeclarationClass)

    def mapArgInfo(argInfo: (Int, ArgInfo)): Option[(Int, ArgInfo)] = argInfo match {
      case lit @ (_, FunctionLiteral)             => Some(lit)
      case (argIndex, ForwardedParam(paramIndex)) => callsite.argInfos.get(paramIndex).map((argIndex, _))
    }

    // Add all invocation instructions and closure instantiations that were inlined to the call graph
    callGraph.callsites(callee).valuesIterator foreach { originalCallsite =>
      val newCallsiteIns = instructionMap(originalCallsite.callsiteInstruction).asInstanceOf[MethodInsnNode]
      val argInfos = originalCallsite.argInfos flatMap mapArgInfo
      val newCallsite = originalCallsite.copy(
        callsiteInstruction = newCallsiteIns,
        callsiteMethod = callsiteMethod,
        callsiteClass = callsiteClass,
        argInfos = argInfos,
        callsiteStackHeight = callsiteStackHeight + originalCallsite.callsiteStackHeight
      )
      val clonedCallsite = ClonedCallsite(newCallsite, callsite)
      originalCallsite.inlinedClones += clonedCallsite
      callGraph.addCallsite(newCallsite)
      undo {
        originalCallsite.inlinedClones -= clonedCallsite
        callGraph.removeCallsite(newCallsite.callsiteInstruction, newCallsite.callsiteMethod)
      }
    }

    callGraph.closureInstantiations(callee).valuesIterator foreach { originalClosureInit =>
      val newIndy = instructionMap(originalClosureInit.lambdaMetaFactoryCall.indy).asInstanceOf[InvokeDynamicInsnNode]
      val capturedArgInfos = originalClosureInit.capturedArgInfos flatMap mapArgInfo
      val newClosureInit = ClosureInstantiation(
        originalClosureInit.lambdaMetaFactoryCall.copy(indy = newIndy),
        callsiteMethod,
        callsiteClass,
        capturedArgInfos)
      originalClosureInit.inlinedClones += newClosureInit
      callGraph.addClosureInstantiation(newClosureInit)
      undo {
        callGraph.removeClosureInstantiation(newClosureInit.lambdaMetaFactoryCall.indy, newClosureInit.ownerMethod)
      }
    }

    // Remove the elided invocation from the call graph
    callGraph.removeCallsite(callsiteInstruction, callsiteMethod)
    undo { callGraph.addCallsite(callsite) }

    // Inlining a method body can render some code unreachable, see example above in this method.
    unreachableCodeEliminated -= callsiteMethod
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
    import callsite.{callsiteMethod, callsiteClass}
    val Right(callsiteCallee) = callsite.callee
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
  def canInlineCallsite(callsite: Callsite): Option[(CannotInlineWarning, List[AbstractInsnNode])] = {
    import callsite.{callsiteInstruction, callsiteMethod, callsiteClass, callsiteStackHeight}
    val Right(callsiteCallee) = callsite.callee
    import callsiteCallee.{callee, calleeDeclarationClass}

    def calleeDesc = s"${callee.name} of type ${callee.desc} in ${calleeDeclarationClass.internalName}"
    def methodMismatch = s"Wrong method node for inlining ${textify(callsiteInstruction)}: $calleeDesc"
    assert(callsiteInstruction.name == callee.name, methodMismatch)
    assert(callsiteInstruction.desc == callee.desc, methodMismatch)
    assert(!isConstructor(callee), s"Constructors cannot be inlined: $calleeDesc")
    assert(!BytecodeUtils.isAbstractMethod(callee), s"Callee is abstract: $calleeDesc")
    assert(callsiteMethod.instructions.contains(callsiteInstruction), s"Callsite ${textify(callsiteInstruction)} is not an instruction of $calleeDesc")

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

    if (codeSizeOKForInlining(callsiteMethod, callee)) {
      val warning = ResultingMethodTooLarge(
        calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
        callsiteClass.internalName, callsiteMethod.name, callsiteMethod.desc)
      Some((warning, Nil))
    } else if (!callee.tryCatchBlocks.isEmpty && stackHasNonParameters) {
      val warning = MethodWithHandlerCalledOnNonEmptyStack(
        calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
        callsiteClass.internalName, callsiteMethod.name, callsiteMethod.desc)
      Some((warning, Nil))
    } else findIllegalAccess(callee.instructions, calleeDeclarationClass, callsiteClass) match {
      case Right(Nil) =>
        None

      case Right(illegalAccessInsns) =>
        val warning = IllegalAccessInstruction(
          calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
          callsiteClass.internalName, illegalAccessInsns.head)
        Some((warning, illegalAccessInsns))

      case Left((illegalAccessIns, cause)) =>
        val warning = IllegalAccessCheckFailed(
          calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
          callsiteClass.internalName, illegalAccessIns, cause)
        Some((warning, Nil))
    }
  }

  /**
   * Check if a type is accessible to some class, as defined in JVMS 5.4.4.
   *  (A1) C is public
   *  (A2) C and D are members of the same run-time package
   */
  def classIsAccessible(accessed: BType, from: ClassBType): Either[OptimizerWarning, Boolean] = (accessed: @unchecked) match {
    // TODO: A2 requires "same run-time package", which seems to be package + classloader (JVMS 5.3.). is the below ok?
    case c: ClassBType     => c.isPublic.map(_ || c.packageInternalName == from.packageInternalName)
    case a: ArrayBType     => classIsAccessible(a.elementType, from)
    case _: PrimitiveBType => Right(true)
  }

  /**
   * Check if a member reference is accessible from the [[destinationClass]], as defined in the
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
    /**
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
          res
        }

      case mi: MethodInsnNode =>
        if (mi.owner.charAt(0) == '[') Right(true) // array methods are accessible
        else {
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
            res
          }
        }

      case _: InvokeDynamicInsnNode if destinationClass == calleeDeclarationClass =>
        // within the same class, any indy instruction can be inlined
         Right(true)

      // does the InvokeDynamicInsnNode call LambdaMetaFactory?
      case LambdaMetaFactoryCall(_, _, implMethod, _) =>
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
        // [1] http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.10
        // [2] http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.23
        // [3] http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.invokedynamic
        // [4] http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-5.html#jvms-5.4.3

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
          res
        }

      case _: InvokeDynamicInsnNode => Left(UnknownInvokeDynamicInstruction)

      case ci: LdcInsnNode => ci.cst match {
        case t: asm.Type => classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(t.getInternalName), destinationClass)
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
