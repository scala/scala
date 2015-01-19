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
import scala.collection.convert.decorateAsScala._
import scala.collection.convert.decorateAsJava._
import AsmUtils._
import BytecodeUtils._
import OptimizerReporting._
import scala.tools.asm.tree.analysis._
import collection.mutable

class Inliner[BT <: BTypes](val btypes: BT) {
  import btypes._
  import callGraph._

  def runInliner(): Unit = {
    for (request <- collectAndOrderInlineRequests) {
      val Some(callee) = request.callee
      inline(request.callsiteInstruction, request.callsiteStackHeight, request.callsiteMethod, request.callsiteClass,
        callee.callee, callee.calleeDeclarationClass,
        receiverKnownNotNull = false, keepLineNumbers = false)
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
    callsites.iterator.filter({
      case (_, callsite) => callsite.callee match {
        case Some(Callee(callee, _, safeToInline, Some(annotatedInline), _)) =>
          // TODO: fix inlining from traits.
          // For trait methods the callee is abstract: "trait T { @inline final def f = 1}".
          // A callsite (t: T).f is `safeToInline` (effectivelyFinal is true), but the callee is the
          // abstract method in the interface.
          !isAbstractMethod(callee) && safeToInline && annotatedInline
        case _ => false
      }
      case _ => false
    }).map(_._2).toList
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
             receiverKnownNotNull: Boolean, keepLineNumbers: Boolean): Option[String] = {
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

      // label for the exit of the inlined functions. xRETURNs are rplaced by GOTOs to this label.
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
      val analyzer = new BasicAnalyzer(callee, calleeDeclarationClass.internalName)

      for (originalReturn <- callee.instructions.iterator().asScala if isReturn(originalReturn)) {
        val frame = analyzer.frameAt(originalReturn)
        var stackHeight = frame.getStackSize

        val inlinedReturn = instructionMap(originalReturn)
        val returnReplacement = new InsnList

        def drop(slot: Int) = returnReplacement add getPop(frame.peekDown(slot).getSize)

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

      // Add all invocation instructions that were inlined to the call graph
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
                callsiteStackHeight = callsiteStackHeight + originalCallsite.callsiteStackHeight
              )

            case None =>
          }

        case _ =>
      }
      // Remove the elided invocation from the call graph
      callGraph.callsites.remove(callsiteInstruction)

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
                callee: MethodNode, calleeDeclarationClass: ClassBType): Option[String] = {

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

    if (isSynchronizedMethod(callee)) {
      // Could be done by locking on the receiver, wrapping the inlined code in a try and unlocking
      // in finally. But it's probably not worth the effort, scala never emits synchronized methods.
      Some(s"Method ${methodSignature(calleeDeclarationClass.internalName, callee)} is not inlined because it is synchronized")
    } else if (!callee.tryCatchBlocks.isEmpty && stackHasNonParameters) {
      Some(
        s"""The operand stack at the callsite in ${methodSignature(callsiteClass.internalName, callsiteMethod)} contains more values than the
           |arguments expected by the callee ${methodSignature(calleeDeclarationClass.internalName, callee)}. These values would be discarded
           |when entering an exception handler declared in the inlined method.""".stripMargin
      )
    } else findIllegalAccess(callee.instructions, callsiteClass) map {
      case illegalAccessIns =>
        s"""The callee ${methodSignature(calleeDeclarationClass.internalName, callee)} contains the instruction ${AsmUtils.textify(illegalAccessIns)}
           |that would cause an IllegalAccessError when inlined into class ${callsiteClass.internalName}""".stripMargin
    }
  }

  def findIllegalAccess(instructions: InsnList, destinationClass: ClassBType): Option[AbstractInsnNode] = {

    /**
     * Check if a type is accessible to some class, as defined in JVMS 5.4.4.
     *  (A1) C is public
     *  (A2) C and D are members of the same run-time package
     */
    def classIsAccessible(accessed: BType, from: ClassBType = destinationClass): Boolean = (accessed: @unchecked) match {
      // TODO: A2 requires "same run-time package", which seems to be package + classloader (JMVS 5.3.). is the below ok?
      case c: ClassBType     => c.isPublic || c.packageInternalName == from.packageInternalName
      case a: ArrayBType     => classIsAccessible(a.elementType, from)
      case _: PrimitiveBType => true
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
     * JVMS 5.4.4 summary: A field or method R is accessible to a class D (destinationClass) iff
     *  (B1) R is public
     *  (B2) R is protected, declared in C (memberDeclClass) and D is a subclass of C.
     *       If R is not static, R must contain a symbolic reference to a class T (memberRefClass),
     *       such that T is either a subclass of D, a superclass of D, or D itself.
     *  (B3) R is either protected or has default access and declared by a class in the same
     *       run-time package as D.
     *  (B4) R is private and is declared in D.
     */
    def memberIsAccessible(memberFlags: Int, memberDeclClass: ClassBType, memberRefClass: ClassBType): Boolean = {
      // TODO: B3 requires "same run-time package", which seems to be package + classloader (JMVS 5.3.). is the below ok?
      def samePackageAsDestination = memberDeclClass.packageInternalName == destinationClass.packageInternalName

      val key = (ACC_PUBLIC | ACC_PROTECTED | ACC_PRIVATE) & memberFlags
      key match {
        case ACC_PUBLIC     =>       // B1
          true

        case ACC_PROTECTED  =>       // B2
          val condB2 = destinationClass.isSubtypeOf(memberDeclClass) && {
            val isStatic = (ACC_STATIC & memberFlags) != 0
            isStatic || memberRefClass.isSubtypeOf(destinationClass) || destinationClass.isSubtypeOf(memberRefClass)
          }
          condB2 || samePackageAsDestination // B3 (protected)

        case 0 =>                            // B3 (default access)
          samePackageAsDestination

        case ACC_PRIVATE    =>       // B4
          memberDeclClass == destinationClass
      }
    }

    def isLegal(instruction: AbstractInsnNode): Boolean = instruction match {
      case ti: TypeInsnNode  =>
        // NEW, ANEWARRAY, CHECKCAST or INSTANCEOF. For these instructions, the reference
        // "must be a symbolic reference to a class, array, or interface type" (JVMS 6), so
        // it can be an internal name, or a full array descriptor.
        classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(ti.desc))

      case ma: MultiANewArrayInsnNode =>
        // "a symbolic reference to a class, array, or interface type"
        classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(ma.desc))

      case fi: FieldInsnNode =>
        val fieldRefClass = classBTypeFromParsedClassfile(fi.owner)
        val (fieldNode, fieldDeclClass) = byteCodeRepository.fieldNode(fieldRefClass.internalName, fi.name, fi.desc).get
        memberIsAccessible(fieldNode.access, classBTypeFromParsedClassfile(fieldDeclClass), fieldRefClass)

      case mi: MethodInsnNode =>
        if (mi.owner.charAt(0) == '[') true // array methods are accessible
        else {
          val methodRefClass = classBTypeFromParsedClassfile(mi.owner)
          val (methodNode, methodDeclClass) = byteCodeRepository.methodNode(methodRefClass.internalName, mi.name, mi.desc).get
          memberIsAccessible(methodNode.access, classBTypeFromParsedClassfile(methodDeclClass), methodRefClass)
        }

      case ivd: InvokeDynamicInsnNode =>
        // TODO @lry check necessary conditions to inline an indy, instead of giving up
        false

      case ci: LdcInsnNode => ci.cst match {
        case t: asm.Type => classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(t.getInternalName))
        case _           => true
      }

      case _ => true
    }

    instructions.iterator.asScala.find(!isLegal(_))
  }
}
