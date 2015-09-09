/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.tailrec
import scala.tools.asm
import asm.Handle
import asm.Opcodes._
import asm.tree._
import scala.collection.convert.decorateAsScala._
import scala.collection.convert.decorateAsJava._
import AsmUtils._
import BytecodeUtils._
import collection.mutable
import scala.tools.asm.tree.analysis.SourceInterpreter
import BackendReporting._
import scala.tools.nsc.backend.jvm.BTypes.InternalName

class Inliner[BT <: BTypes](val btypes: BT) {
  import btypes._
  import callGraph._

  def eliminateUnreachableCodeAndUpdateCallGraph(methodNode: MethodNode, definingClass: InternalName): Unit = {
    localOpt.minimalRemoveUnreachableCode(methodNode, definingClass) foreach {
      case invocation: MethodInsnNode  => callGraph.callsites.remove(invocation)
      case indy: InvokeDynamicInsnNode => callGraph.closureInstantiations.remove(indy)
      case _ =>
    }
  }

  def runInliner(): Unit = {
    rewriteFinalTraitMethodInvocations()

    for (request <- collectAndOrderInlineRequests) {
      val Right(callee) = request.callee // collectAndOrderInlineRequests returns callsites with a known callee

      // Inlining a method can create unreachable code. Example:
      //   def f = throw e
      //   def g = f; println() // println is unreachable after inlining f
      // If we have an inline request for a call to g, and f has been already inlined into g, we
      // need to run DCE before inlining g.
      eliminateUnreachableCodeAndUpdateCallGraph(callee.callee, callee.calleeDeclarationClass.internalName)

      // DCE above removes unreachable callsites from the call graph. If the inlining request denotes
      // such an eliminated callsite, do nothing.
      if (callGraph.callsites contains request.callsiteInstruction) {
        val r = inline(request.callsiteInstruction, request.callsiteStackHeight, request.callsiteMethod, request.callsiteClass,
          callee.callee, callee.calleeDeclarationClass,
          request.receiverKnownNotNull, keepLineNumbers = false)

        for (warning <- r) {
          if ((callee.annotatedInline && btypes.compilerSettings.YoptWarningEmitAtInlineFailed) || warning.emitWarning(compilerSettings)) {
            val annotWarn = if (callee.annotatedInline) " is annotated @inline but" else ""
            val msg = s"${BackendReporting.methodSignature(callee.calleeDeclarationClass.internalName, callee.callee)}$annotWarn could not be inlined:\n$warning"
            backendReporting.inlinerWarning(request.callsitePosition, msg)
          }
        }
      }
    }
  }

  /**
   * Ordering for inline requests. Required to make the inliner deterministic:
   *   - Always remove the same request when breaking inlining cycles
   *   - Perform inlinings in a consistent order
   */
  object callsiteOrdering extends Ordering[Callsite] {
    override def compare(x: Callsite, y: Callsite): Int = {
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

  /**
   * Select callsites from the call graph that should be inlined. The resulting list of inlining
   * requests is allowed to have cycles, and the callsites can appear in any order.
   */
  def selectCallsitesForInlining: List[Callsite] = {
    callsites.valuesIterator.filter({
      case callsite @ Callsite(_, _, _, Right(Callee(callee, calleeDeclClass, safeToInline, _, annotatedInline, _, warning)), _, _, _, pos) =>
        val res = doInlineCallsite(callsite)

        if (!res) {
          if (annotatedInline && btypes.compilerSettings.YoptWarningEmitAtInlineFailed) {
            // if the callsite is annotated @inline, we report an inline warning even if the underlying
            // reason is, for example, mixed compilation (which has a separate -Yopt-warning flag).
            def initMsg = s"${BackendReporting.methodSignature(calleeDeclClass.internalName, callee)} is annotated @inline but cannot be inlined"
            def warnMsg = warning.map(" Possible reason:\n" + _).getOrElse("")
            if (doRewriteTraitCallsite(callsite))
              backendReporting.inlinerWarning(pos, s"$initMsg: the trait method call could not be rewritten to the static implementation method." + warnMsg)
            else if (!safeToInline)
              backendReporting.inlinerWarning(pos, s"$initMsg: the method is not final and may be overridden." + warnMsg)
            else
              backendReporting.inlinerWarning(pos, s"$initMsg." + warnMsg)
          } else if (warning.isDefined && warning.get.emitWarning(compilerSettings)) {
            // when annotatedInline is false, and there is some warning, the callsite metadata is possibly incomplete.
            backendReporting.inlinerWarning(pos, s"there was a problem determining if method ${callee.name} can be inlined: \n"+ warning.get)
          }
        }

        res

      case Callsite(ins, _, _, Left(warning), _, _, _, pos) =>
        if (warning.emitWarning(compilerSettings))
          backendReporting.inlinerWarning(pos, s"failed to determine if ${ins.name} should be inlined:\n$warning")
        false
    }).toList
  }

  /**
   * The current inlining heuristics are simple: inline calls to methods annotated @inline.
   */
  def doInlineCallsite(callsite: Callsite): Boolean = callsite match {
    case Callsite(_, _, _, Right(Callee(callee, calleeDeclClass, safeToInline, _, annotatedInline, _, warning)), _, _, _, pos) =>
      if (compilerSettings.YoptInlineHeuristics.value == "everything") safeToInline
      else annotatedInline && safeToInline

    case _ => false
  }

  def rewriteFinalTraitMethodInvocations(): Unit = {
    // Rewriting final trait method callsites to the implementation class enables inlining.
    // We cannot just iterate over the values of the `callsites` map because the rewrite changes the
    // map. Therefore we first copy the values to a list.
    callsites.values.toList.foreach(rewriteFinalTraitMethodInvocation)
  }

  /**
   * True for statically resolved trait callsites that should be rewritten to the static implementation method.
   */
  def doRewriteTraitCallsite(callsite: Callsite) = callsite.callee match {
    case Right(Callee(callee, calleeDeclarationClass, safeToInline, true, annotatedInline, annotatedNoInline, infoWarning)) => true
    case _ => false
  }

  /**
   * Rewrite the INVOKEINTERFACE callsite of a final trait method invocation to INVOKESTATIC of the
   * corresponding method in the implementation class. This enables inlining final trait methods.
   *
   * In a final trait method callsite, the callee is safeToInline and the callee method is abstract
   * (the receiver type is the interface, so the method is abstract).
   */
  def rewriteFinalTraitMethodInvocation(callsite: Callsite): Unit = {
    if (doRewriteTraitCallsite(callsite)) {
      val Right(Callee(callee, calleeDeclarationClass, _, _, annotatedInline, annotatedNoInline, infoWarning)) = callsite.callee

      val traitMethodArgumentTypes = asm.Type.getArgumentTypes(callee.desc)

      val implClassInternalName = calleeDeclarationClass.internalName + "$class"

      val selfParamTypeV: Either[OptimizerWarning, ClassBType] = calleeDeclarationClass.info.map(_.inlineInfo.traitImplClassSelfType match {
        case Some(internalName) => classBTypeFromParsedClassfile(internalName)
        case None               => calleeDeclarationClass
      })

      def implClassMethodV(implMethodDescriptor: String): Either[OptimizerWarning, MethodNode] = {
        byteCodeRepository.methodNode(implClassInternalName, callee.name, implMethodDescriptor).map(_._1)
      }

      // The rewrite reading the implementation class and the implementation method from the bytecode
      // repository. If either of the two fails, the rewrite is not performed.
      val res = for {
        selfParamType        <- selfParamTypeV
        implMethodDescriptor =  asm.Type.getMethodDescriptor(asm.Type.getReturnType(callee.desc), selfParamType.toASMType +: traitMethodArgumentTypes: _*)
        implClassMethod      <- implClassMethodV(implMethodDescriptor)
        implClassBType       =  classBTypeFromParsedClassfile(implClassInternalName)
        selfTypeOk           <- calleeDeclarationClass.isSubtypeOf(selfParamType)
      } yield {

        // The self parameter type may be incompatible with the trait type.
        //   trait T { self: S => def foo = 1 }
        // The $self parameter type of T$class.foo is S, which may be unrelated to T. If we re-write
        // a call to T.foo to T$class.foo, we need to cast the receiver to S, otherwise we get a
        // VerifyError. We run a `SourceInterpreter` to find all producer instructions of the
        // receiver value and add a cast to the self type after each.
        if (!selfTypeOk) {
          // there's no need to run eliminateUnreachableCode here. building the call graph does that
          // already, no code can become unreachable in the meantime.
          val analyzer = new AsmAnalyzer(callsite.callsiteMethod, callsite.callsiteClass.internalName, new SourceInterpreter)
          val receiverValue = analyzer.frameAt(callsite.callsiteInstruction).peekStack(traitMethodArgumentTypes.length)
          for (i <- receiverValue.insns.asScala) {
            val cast = new TypeInsnNode(CHECKCAST, selfParamType.internalName)
            callsite.callsiteMethod.instructions.insert(i, cast)
          }
        }

        val newCallsiteInstruction = new MethodInsnNode(INVOKESTATIC, implClassInternalName, callee.name, implMethodDescriptor, false)
        callsite.callsiteMethod.instructions.insert(callsite.callsiteInstruction, newCallsiteInstruction)
        callsite.callsiteMethod.instructions.remove(callsite.callsiteInstruction)

        callGraph.callsites.remove(callsite.callsiteInstruction)
        val staticCallsite = Callsite(
          callsiteInstruction = newCallsiteInstruction,
          callsiteMethod      = callsite.callsiteMethod,
          callsiteClass       = callsite.callsiteClass,
          callee              = Right(Callee(
            callee                 = implClassMethod,
            calleeDeclarationClass = implClassBType,
            safeToInline           = true,
            safeToRewrite          = false,
            annotatedInline        = annotatedInline,
            annotatedNoInline      = annotatedNoInline,
            calleeInfoWarning      = infoWarning)),
          argInfos            = Nil,
          callsiteStackHeight = callsite.callsiteStackHeight,
          receiverKnownNotNull = callsite.receiverKnownNotNull,
          callsitePosition = callsite.callsitePosition
        )
        callGraph.callsites(newCallsiteInstruction) = staticCallsite
      }

      for (warning <- res.left) {
        val Right(callee) = callsite.callee
        val newCallee = callee.copy(calleeInfoWarning = Some(RewriteTraitCallToStaticImplMethodFailed(calleeDeclarationClass.internalName, callee.callee.name, callee.callee.desc, warning)))
        callGraph.callsites(callsite.callsiteInstruction) = callsite.copy(callee = Right(newCallee))
      }
    }
  }

  /**
   * Returns the callsites that can be inlined. Ensures that the returned inline request graph does
   * not contain cycles.
   *
   * The resulting list is sorted such that the leaves of the inline request graph are on the left.
   * Once these leaves are inlined, the successive elements will be leaves, etc.
   */
  private def collectAndOrderInlineRequests: List[Callsite] = {
    val requests = selectCallsitesForInlining

    // This map is an index to look up the inlining requests for a method. The value sets are mutable
    // to allow removing elided requests (to break inlining cycles). The map itself is mutable to
    // allow efficient building: requests.groupBy would build values as List[Callsite] that need to
    // be transformed to mutable sets.
    val inlineRequestsForMethod: mutable.Map[MethodNode, mutable.Set[Callsite]] = mutable.HashMap.empty.withDefaultValue(mutable.HashSet.empty)
    for (r <- requests) inlineRequestsForMethod.getOrElseUpdate(r.callsiteMethod, mutable.HashSet.empty) += r

    /**
     * Break cycles in the inline request graph by removing callsites.
     *
     * The list `requests` is traversed left-to-right, removing those callsites that are part of a
     * cycle. Elided callsites are also removed from the `inlineRequestsForMethod` map.
     */
    def breakInlineCycles(requests: List[Callsite]): List[Callsite] = {
      // is there a path of inline requests from start to goal?
      def isReachable(start: MethodNode, goal: MethodNode): Boolean = {
        @tailrec def reachableImpl(check: List[MethodNode], visited: Set[MethodNode]): Boolean = check match {
          case x :: xs =>
            if (x == goal) true
            else if (visited(x)) reachableImpl(xs, visited)
            else {
              val callees = inlineRequestsForMethod(x).map(_.callee.get.callee)
              reachableImpl(xs ::: callees.toList, visited + x)
            }

          case Nil =>
            false
        }
        reachableImpl(List(start), Set.empty)
      }

      val result = new mutable.ListBuffer[Callsite]()
      // sort the inline requests to ensure that removing requests is deterministic
      for (r <- requests.sorted(callsiteOrdering)) {
        // is there a chain of inlining requests that would inline the callsite method into the callee?
        if (isReachable(r.callee.get.callee, r.callsiteMethod))
          inlineRequestsForMethod(r.callsiteMethod) -= r
        else
          result += r
      }
      result.toList
    }

    // sort the remaining inline requests such that the leaves appear first, then those requests
    // that become leaves, etc.
    def leavesFirst(requests: List[Callsite], visited: Set[Callsite] = Set.empty): List[Callsite] = {
      if (requests.isEmpty) Nil
      else {
        val (leaves, others) = requests.partition(r => {
          val inlineRequestsForCallee = inlineRequestsForMethod(r.callee.get.callee)
          inlineRequestsForCallee.forall(visited)
        })
        assert(leaves.nonEmpty, requests)
        leaves ::: leavesFirst(others, visited ++ leaves)
      }
    }

    leavesFirst(breakInlineCycles(requests))
  }


  /**
   * Copy and adapt the instructions of a method to a callsite.
   *
   * Preconditions:
   *   - The maxLocals and maxStack values of the callsite method are correctly computed
   *   - The callsite method contains no unreachable basic blocks, i.e., running an [[Analyzer]]
   *     does not produce any `null` frames
   *
   * @param callsiteInstruction     The invocation instruction
   * @param callsiteStackHeight     The stack height at the callsite
   * @param callsiteMethod          The method in which the invocation occurs
   * @param callsiteClass           The class in which the callsite method is defined
   * @param callee                  The invoked method
   * @param calleeDeclarationClass  The class in which the invoked method is defined
   * @param receiverKnownNotNull    `true` if the receiver is known to be non-null
   * @param keepLineNumbers         `true` if LineNumberNodes should be copied to the call site
   * @return                        `Some(message)` if inlining cannot be performed, `None` otherwise
   */
  def inline(callsiteInstruction: MethodInsnNode, callsiteStackHeight: Int, callsiteMethod: MethodNode, callsiteClass: ClassBType,
             callee: MethodNode, calleeDeclarationClass: ClassBType,
             receiverKnownNotNull: Boolean, keepLineNumbers: Boolean): Option[CannotInlineWarning] = {
    canInline(callsiteInstruction, callsiteStackHeight, callsiteMethod, callsiteClass, callee, calleeDeclarationClass) orElse {
      // New labels for the cloned instructions
      val labelsMap = cloneLabels(callee)
      val (clonedInstructions, instructionMap) = cloneInstructions(callee, labelsMap)
      if (!keepLineNumbers) {
        removeLineNumberNodes(clonedInstructions)
      }

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

      for(argTp <- calleAsmType.getArgumentTypes) {
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

      callsiteMethod.instructions.insert(callsiteInstruction, clonedInstructions)
      callsiteMethod.instructions.remove(callsiteInstruction)

      callsiteMethod.localVariables.addAll(cloneLocalVariableNodes(callee, labelsMap, callee.name + "_").asJava)
      callsiteMethod.tryCatchBlocks.addAll(cloneTryCatchBlockNodes(callee, labelsMap).asJava)

      // Add all invocation instructions and closure instantiations that were inlined to the call graph
      callee.instructions.iterator().asScala foreach {
        case originalCallsiteIns: MethodInsnNode =>
          callGraph.callsites.get(originalCallsiteIns) match {
            case Some(originalCallsite) =>
              val newCallsiteIns = instructionMap(originalCallsiteIns).asInstanceOf[MethodInsnNode]
              callGraph.callsites(newCallsiteIns) = Callsite(
                callsiteInstruction = newCallsiteIns,
                callsiteMethod = callsiteMethod,
                callsiteClass = callsiteClass,
                callee = originalCallsite.callee,
                argInfos = Nil, // TODO: re-compute argInfos for new destination (once we actually compute them)
                callsiteStackHeight = callsiteStackHeight + originalCallsite.callsiteStackHeight,
                receiverKnownNotNull = originalCallsite.receiverKnownNotNull,
                callsitePosition = originalCallsite.callsitePosition
              )

            case None =>
          }

        case indy: InvokeDynamicInsnNode =>
          callGraph.closureInstantiations.get(indy) match {
            case Some(closureInit) =>
              val newIndy = instructionMap(indy).asInstanceOf[InvokeDynamicInsnNode]
              callGraph.closureInstantiations(newIndy) = ClosureInstantiation(closureInit.lambdaMetaFactoryCall.copy(indy = newIndy), callsiteMethod, callsiteClass)

            case None =>
          }

        case _ =>
      }
      // Remove the elided invocation from the call graph
      callGraph.callsites.remove(callsiteInstruction)

      // Inlining a method body can render some code unreachable, see example above (in runInliner).
      unreachableCodeEliminated -= callsiteMethod

      callsiteMethod.maxLocals += returnType.getSize + callee.maxLocals
      callsiteMethod.maxStack = math.max(callsiteMethod.maxStack, callee.maxStack + callsiteStackHeight)

      None
    }
  }

  /**
   * Check whether an inling can be performed. Parmeters are described in method [[inline]].
   * @return `Some(message)` if inlining cannot be performed, `None` otherwise
   */
  def canInline(callsiteInstruction: MethodInsnNode, callsiteStackHeight: Int, callsiteMethod: MethodNode, callsiteClass: ClassBType,
                callee: MethodNode, calleeDeclarationClass: ClassBType): Option[CannotInlineWarning] = {

    def calleeDesc = s"${callee.name} of type ${callee.desc} in ${calleeDeclarationClass.internalName}"
    def methodMismatch = s"Wrong method node for inlining ${textify(callsiteInstruction)}: $calleeDesc"
    assert(callsiteInstruction.name == callee.name, methodMismatch)
    assert(callsiteInstruction.desc == callee.desc, methodMismatch)
    assert(!isConstructor(callee), s"Constructors cannot be inlined: $calleeDesc")
    assert(!BytecodeUtils.isAbstractMethod(callee), s"Callee is abstract: $calleeDesc")
    assert(callsiteMethod.instructions.contains(callsiteInstruction), s"Callsite ${textify(callsiteInstruction)} is not an instruction of $calleeDesc")

    // When an exception is thrown, the stack is cleared before jumping to the handler. When
    // inlining a method that catches an exception, all values that were on the stack before the
    // call (in addition to the arguments) would be cleared (SI-6157). So we don't inline methods
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
      Some(ResultingMethodTooLarge(
        calleeDeclarationClass.internalName, callee.name, callee.desc,
        callsiteClass.internalName, callsiteMethod.name, callsiteMethod.desc))
    } else if (isSynchronizedMethod(callee)) {
      // Could be done by locking on the receiver, wrapping the inlined code in a try and unlocking
      // in finally. But it's probably not worth the effort, scala never emits synchronized methods.
      Some(SynchronizedMethod(calleeDeclarationClass.internalName, callee.name, callee.desc))
    } else if (isStrictfpMethod(callsiteMethod) != isStrictfpMethod(callee)) {
      Some(StrictfpMismatch(
        calleeDeclarationClass.internalName, callee.name, callee.desc,
        callsiteClass.internalName, callsiteMethod.name, callsiteMethod.desc))
    } else if (!callee.tryCatchBlocks.isEmpty && stackHasNonParameters) {
      Some(MethodWithHandlerCalledOnNonEmptyStack(
        calleeDeclarationClass.internalName, callee.name, callee.desc,
        callsiteClass.internalName, callsiteMethod.name, callsiteMethod.desc))
    } else findIllegalAccess(callee.instructions, calleeDeclarationClass, callsiteClass) map {
      case (illegalAccessIns, None) =>
        IllegalAccessInstruction(
          calleeDeclarationClass.internalName, callee.name, callee.desc,
          callsiteClass.internalName, illegalAccessIns)

      case (illegalAccessIns, Some(warning)) =>
        IllegalAccessCheckFailed(
          calleeDeclarationClass.internalName, callee.name, callee.desc,
          callsiteClass.internalName, illegalAccessIns, warning)
    }
  }

  /**
   * Check if a type is accessible to some class, as defined in JVMS 5.4.4.
   *  (A1) C is public
   *  (A2) C and D are members of the same run-time package
   */
  def classIsAccessible(accessed: BType, from: ClassBType): Either[OptimizerWarning, Boolean] = (accessed: @unchecked) match {
    // TODO: A2 requires "same run-time package", which seems to be package + classloader (JMVS 5.3.). is the below ok?
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
    // TODO: B3 requires "same run-time package", which seems to be package + classloader (JMVS 5.3.). is the below ok?
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
   * Returns the first instruction in the `instructions` list that would cause a
   * [[java.lang.IllegalAccessError]] when inlined into the `destinationClass`.
   *
   * If validity of some instruction could not be checked because an error occurred, the instruction
   * is returned together with a warning message that describes the problem.
   */
  def findIllegalAccess(instructions: InsnList, calleeDeclarationClass: ClassBType, destinationClass: ClassBType): Option[(AbstractInsnNode, Option[OptimizerWarning])] = {
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
    @tailrec def find: Option[(AbstractInsnNode, Option[OptimizerWarning])] = {
      if (!it.hasNext) None // all instructions are legal
      else {
        val i = it.next()
        isLegal(i) match {
          case Left(warning) => Some((i, Some(warning))) // checking isLegal for i failed
          case Right(false)  => Some((i, None))          // an illegal instruction was found
          case _             => find
        }
      }
    }
    find
  }
}
