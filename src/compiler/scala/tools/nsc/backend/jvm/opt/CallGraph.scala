/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.collection.immutable.IntMap
import scala.reflect.internal.util.{NoPosition, Position}
import scala.tools.asm.{Handle, Opcodes, Type}
import scala.tools.asm.tree._
import scala.collection.{concurrent, mutable}
import scala.collection.JavaConverters._
import scala.tools.nsc.backend.jvm.BTypes.{InternalName, MethodInlineInfo}
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.nsc.backend.jvm.analysis._
import BytecodeUtils._

class CallGraph[BT <: BTypes](val btypes: BT) {
  import btypes._
  import backendUtils._

  /**
   * The call graph contains the callsites in the program being compiled.
   *
   * Indexing the call graph by the containing MethodNode and the invocation MethodInsnNode allows
   * finding callsites efficiently. For example, an inlining heuristic might want to know all
   * callsites withing a callee method.
   *
   * Note that the call graph is not guaranteed to be complete: callsites may be missing. In
   * particular, if a method is very large, all of its callsites might not be in the hash map.
   * The reason is that adding a method to the call graph requires running an ASM analyzer, which
   * can be too slow.
   *
   * Note that call graph entries (Callsite instances) keep a reference to the invocation
   * MethodInsnNode, which keeps all AbstractInsnNodes of the method reachable. Adding classes
   * from the classpath to the call graph (in addition to classes being compiled) may prevent
   * method instruction nodes from being GCd. The ByteCodeRepository has a fixed size cache for
   * parsed ClassNodes - keeping all ClassNodes alive consumed too much memory.
   * The call graph is less problematic because only methods being called are kept alive, not entire
   * classes. But we should keep an eye on this.
   */
  val callsites: mutable.Map[MethodNode, Map[MethodInsnNode, Callsite]] = recordPerRunCache(concurrent.TrieMap.empty withDefaultValue Map.empty)

  /**
   * Closure instantiations in the program being compiled.
   *
   * Indexing closure instantiations by the containing MethodNode is beneficial for the closure
   * optimizer: finding callsites to re-write requires running a producers-consumers analysis on
   * the method. Here the closure instantiations are already grouped by method.
   */
  val closureInstantiations: mutable.Map[MethodNode, Map[InvokeDynamicInsnNode, ClosureInstantiation]] = recordPerRunCache(concurrent.TrieMap.empty withDefaultValue Map.empty)

  def removeCallsite(invocation: MethodInsnNode, methodNode: MethodNode): Option[Callsite] = {
    val methodCallsites = callsites(methodNode)
    val newCallsites = methodCallsites - invocation
    if (newCallsites.isEmpty) callsites.remove(methodNode)
    else callsites(methodNode) = newCallsites
    methodCallsites.get(invocation)
  }

  def addCallsite(callsite: Callsite): Unit = {
    val methodCallsites = callsites(callsite.callsiteMethod)
    callsites(callsite.callsiteMethod) = methodCallsites + (callsite.callsiteInstruction -> callsite)
  }

  def containsCallsite(callsite: Callsite): Boolean = callsites(callsite.callsiteMethod) contains callsite.callsiteInstruction
  def findCallSite(method: MethodNode, call: MethodInsnNode): Option[Callsite] = callsites.getOrElse(method, Map.empty).get(call)

  def removeClosureInstantiation(indy: InvokeDynamicInsnNode, methodNode: MethodNode): Option[ClosureInstantiation] = {
    val methodClosureInits = closureInstantiations(methodNode)
    val newClosureInits = methodClosureInits - indy
    if (newClosureInits.isEmpty) closureInstantiations.remove(methodNode)
    else closureInstantiations(methodNode) = newClosureInits
    methodClosureInits.get(indy)
  }

  def addClosureInstantiation(closureInit: ClosureInstantiation) = {
    val methodClosureInits = closureInstantiations(closureInit.ownerMethod)
    closureInstantiations(closureInit.ownerMethod) = methodClosureInits + (closureInit.lambdaMetaFactoryCall.indy -> closureInit)
  }

  def addClass(classNode: ClassNode): Unit = {
    val classType = classBTypeFromClassNode(classNode)
    classNode.methods.asScala.foreach(addMethod(_, classType))
  }

  def addIfMissing(methodNode: MethodNode, definingClass: ClassBType): Unit = {
    if (!callsites.contains(methodNode)) addMethod(methodNode, definingClass)
  }

  def addMethod(methodNode: MethodNode, definingClass: ClassBType): Unit = {
    if (!BytecodeUtils.isAbstractMethod(methodNode) && !BytecodeUtils.isNativeMethod(methodNode)) {
      // TODO: run dataflow analyses to make the call graph more precise
      //  - producers to get forwarded parameters (ForwardedParam)
      //  - typeAnalysis for more precise argument types, more precise callee

      // For now we run a NullnessAnalyzer. It is used to determine if the receiver of an instance
      // call is known to be not-null, in which case we don't have to emit a null check when inlining.
      // It is also used to get the stack height at the call site.

      val analyzer = {
        if (compilerSettings.optNullnessTracking && AsmAnalyzer.sizeOKForNullness(methodNode)) {
          Some(new AsmAnalyzer(methodNode, definingClass.internalName, new NullnessAnalyzer(btypes, methodNode)))
        } else if (AsmAnalyzer.sizeOKForBasicValue(methodNode)) {
          Some(new AsmAnalyzer(methodNode, definingClass.internalName))
        } else None
      }

      // if the method is too large to run an analyzer, it is not added to the call graph
      if (analyzer.nonEmpty) {
        val Some(a) = analyzer
        def receiverNotNullByAnalysis(call: MethodInsnNode, numArgs: Int) = a.analyzer match {
          case nullnessAnalyzer: NullnessAnalyzer =>
            val frame = nullnessAnalyzer.frameAt(call, methodNode)
            frame.getStack(frame.getStackSize - 1 - numArgs) eq NotNullValue
          case _ => false
        }

        var methodCallsites = Map.empty[MethodInsnNode, Callsite]
        var methodClosureInstantiations = Map.empty[InvokeDynamicInsnNode, ClosureInstantiation]

        // lazy so it is only computed if actually used by computeArgInfos
        lazy val prodCons = new ProdConsAnalyzer(methodNode, definingClass.internalName)

        methodNode.instructions.iterator.asScala foreach {
          case call: MethodInsnNode if a.frameAt(call) != null => // skips over unreachable code
            val callee: Either[OptimizerWarning, Callee] = for {
              (method, declarationClass)                   <- byteCodeRepository.methodNode(call.owner, call.name, call.desc): Either[OptimizerWarning, (MethodNode, InternalName)]
              (declarationClassNode, calleeSourceFilePath) <- byteCodeRepository.classNodeAndSourceFilePath(declarationClass): Either[OptimizerWarning, (ClassNode, Option[String])]
            } yield {
              val declarationClassBType = classBTypeFromClassNode(declarationClassNode)
              val info = analyzeCallsite(method, declarationClassBType, call, calleeSourceFilePath)
              import info._
              Callee(
                callee = method,
                calleeDeclarationClass = declarationClassBType,
                safeToInline = safeToInline,
                sourceFilePath = sourceFilePath,
                annotatedInline = annotatedInline,
                annotatedNoInline = annotatedNoInline,
                samParamTypes = info.samParamTypes,
                calleeInfoWarning = warning)
            }

            val argInfos = computeArgInfos(callee, call, prodCons)

            val receiverNotNull = call.getOpcode == Opcodes.INVOKESTATIC || {
              val numArgs = Type.getArgumentTypes(call.desc).length
              receiverNotNullByAnalysis(call, numArgs)
            }

            methodCallsites += call -> Callsite(
              callsiteInstruction = call,
              callsiteMethod = methodNode,
              callsiteClass = definingClass,
              callee = callee,
              argInfos = argInfos,
              callsiteStackHeight = a.frameAt(call).getStackSize,
              receiverKnownNotNull = receiverNotNull,
              callsitePosition = callsitePositions.getOrElse(call, NoPosition),
              annotatedInline = inlineAnnotatedCallsites(call),
              annotatedNoInline = noInlineAnnotatedCallsites(call)
            )

          case LambdaMetaFactoryCall(indy, samMethodType, implMethod, instantiatedMethodType) if a.frameAt(indy) != null =>
            val lmf = LambdaMetaFactoryCall(indy, samMethodType, implMethod, instantiatedMethodType)
            val capturedArgInfos = computeCapturedArgInfos(lmf, prodCons)
            methodClosureInstantiations += indy -> ClosureInstantiation(
              lmf,
              methodNode,
              definingClass,
              capturedArgInfos)

          case _ =>
        }

        callsites(methodNode) = methodCallsites
        closureInstantiations(methodNode) = methodClosureInstantiations
      }
    }
  }

  def computeArgInfos(callee: Either[OptimizerWarning, Callee], callsiteInsn: MethodInsnNode, prodCons: => ProdConsAnalyzer): IntMap[ArgInfo] = {
    if (callee.isLeft) IntMap.empty
    else {
      lazy val numArgs = Type.getArgumentTypes(callsiteInsn.desc).length + (if (callsiteInsn.getOpcode == Opcodes.INVOKESTATIC) 0 else 1)
      argInfosForSams(callee.get.samParamTypes, callsiteInsn, numArgs, prodCons)
    }
  }

  def computeCapturedArgInfos(lmf: LambdaMetaFactoryCall, prodCons: => ProdConsAnalyzer): IntMap[ArgInfo] = {
    val capturedSams = capturedSamTypes(lmf)
    val numCaptures = Type.getArgumentTypes(lmf.indy.desc).length
    argInfosForSams(capturedSams, lmf.indy, numCaptures, prodCons)
  }

  private def argInfosForSams(sams: IntMap[ClassBType], consumerInsn: AbstractInsnNode, numConsumed: => Int, prodCons: => ProdConsAnalyzer): IntMap[ArgInfo] = {
    // TODO: use type analysis instead of ProdCons - should be more efficient
    // some random thoughts:
    //  - assign special types to parameters and indy-lambda-functions to track them
    //  - upcast should not change type flow analysis: don't lose information.
    //  - can we do something about factory calls? Foo(x) for case class foo gives a Foo.
    //    inline the factory? analysis across method boundary?

    // assign to a lazy val to prevent repeated evaluation of the by-name arg
    lazy val prodConsI = prodCons
    lazy val firstConsumedSlot = {
      val consumerFrame = prodConsI.frameAt(consumerInsn)
      consumerFrame.stackTop - numConsumed + 1
    }
    sams flatMap {
      case (index, _) =>
        val prods = prodConsI.initialProducersForValueAt(consumerInsn, firstConsumedSlot + index)
        if (prods.size != 1) None
        else {
          val argInfo = prods.head match {
            case LambdaMetaFactoryCall(_, _, _, _) => Some(FunctionLiteral)
            case ParameterProducer(local)          => Some(ForwardedParam(local))
            case _                                 => None
          }
          argInfo.map((index, _))
        }
    }
  }

  def samParamTypes(methodNode: MethodNode, receiverType: ClassBType): IntMap[ClassBType] = {
    val paramTypes = {
      val params = Type.getMethodType(methodNode.desc).getArgumentTypes.map(t => bTypeForDescriptorOrInternalNameFromClassfile(t.getDescriptor))
      val isStatic = BytecodeUtils.isStaticMethod(methodNode)
      if (isStatic) params else receiverType +: params
    }
    samTypes(paramTypes)
  }

  def capturedSamTypes(lmf: LambdaMetaFactoryCall): IntMap[ClassBType] = {
    val capturedTypes = Type.getArgumentTypes(lmf.indy.desc).map(t => bTypeForDescriptorOrInternalNameFromClassfile(t.getDescriptor))
    samTypes(capturedTypes)
  }

  private def samTypes(types: Array[BType]): IntMap[ClassBType] = {
    var res = IntMap.empty[ClassBType]
    for (i <- types.indices) {
      types(i) match {
        case c: ClassBType =>
          if (c.info.get.inlineInfo.sam.isDefined) res = res.updated(i, c)

        case _ =>
      }
    }
    res
  }

  /**
   * Just a named tuple used as return type of `analyzeCallsite`.
   */
  private case class CallsiteInfo(safeToInline: Boolean, sourceFilePath: Option[String],
                                  annotatedInline: Boolean, annotatedNoInline: Boolean,
                                  samParamTypes: IntMap[ClassBType],
                                  warning: Option[CalleeInfoWarning])

  /**
   * Analyze a callsite and gather meta-data that can be used for inlining decisions.
   */
  private def analyzeCallsite(calleeMethodNode: MethodNode, calleeDeclarationClassBType: ClassBType, call: MethodInsnNode, calleeSourceFilePath: Option[String]): CallsiteInfo = {
    val methodSignature = calleeMethodNode.name + calleeMethodNode.desc

    try {
      // The inlineInfo.methodInfos of a ClassBType holds an InlineInfo for each method *declared*
      // within a class (not for inherited methods). Since we already have the  classBType of the
      // callee, we only check there for the methodInlineInfo, we should find it there.
      calleeDeclarationClassBType.info.orThrow.inlineInfo.methodInfos.get(methodSignature) match {
        case Some(methodInlineInfo) =>
          val isAbstract = BytecodeUtils.isAbstractMethod(calleeMethodNode)

          val receiverType = classBTypeFromParsedClassfile(call.owner)
          // (1) A non-final method can be safe to inline if the receiver type is a final subclass. Example:
          //   class A { @inline def f = 1 }; object B extends A; B.f  // can be inlined
          //
          // TODO: (1) doesn't cover the following example:
          //   trait TravLike { def map = ... }
          //   sealed trait List extends TravLike { ... } // assume map is not overridden
          //   final case class :: / final case object Nil
          //   (l: List).map // can be inlined
          // we need to know that
          //   - the recevier is sealed
          //   - what are the children of the receiver
          //   - all children are final
          //   - none of the children overrides map
          //
          // TODO: type analysis can render more calls statically resolved. Example:
          //   new A.f  // can be inlined, the receiver type is known to be exactly A.
          val isStaticallyResolved: Boolean = {
            isNonVirtualCall(call) || // SD-86: super calls (invokespecial) can be inlined
            methodInlineInfo.effectivelyFinal ||
              receiverType.info.orThrow.inlineInfo.isEffectivelyFinal // (1)
          }

          val warning = calleeDeclarationClassBType.info.orThrow.inlineInfo.warning.map(
            MethodInlineInfoIncomplete(calleeDeclarationClassBType.internalName, calleeMethodNode.name, calleeMethodNode.desc, _))

          // (1) For invocations of final trait methods, the callee isStaticallyResolved but also
          //     abstract. Such a callee is not safe to inline - it needs to be re-written to the
          //     static impl method first (safeToRewrite).
          CallsiteInfo(
            safeToInline      =
              inlinerHeuristics.canInlineFromSource(calleeSourceFilePath) &&
                isStaticallyResolved &&  // (1)
                !isAbstract &&
                !BytecodeUtils.isConstructor(calleeMethodNode) &&
                !BytecodeUtils.isNativeMethod(calleeMethodNode) &&
                !BytecodeUtils.hasCallerSensitiveAnnotation(calleeMethodNode),
            sourceFilePath      = calleeSourceFilePath,
            annotatedInline     = methodInlineInfo.annotatedInline,
            annotatedNoInline   = methodInlineInfo.annotatedNoInline,
            samParamTypes       = samParamTypes(calleeMethodNode, receiverType),
            warning             = warning)

        case None =>
          val warning = MethodInlineInfoMissing(calleeDeclarationClassBType.internalName, calleeMethodNode.name, calleeMethodNode.desc, calleeDeclarationClassBType.info.orThrow.inlineInfo.warning)
          CallsiteInfo(false, None, false, false, IntMap.empty, Some(warning))
      }
    } catch {
      case Invalid(noInfo: NoClassBTypeInfo) =>
        val warning = MethodInlineInfoError(calleeDeclarationClassBType.internalName, calleeMethodNode.name, calleeMethodNode.desc, noInfo)
        CallsiteInfo(false, None, false, false, IntMap.empty, Some(warning))
    }
  }

    /**
   * A callsite in the call graph.
   *
   * @param callsiteInstruction The invocation instruction
   * @param callsiteMethod      The method containing the callsite
   * @param callsiteClass       The class containing the callsite
   * @param callee              The callee, as it appears in the invocation instruction. For virtual
   *                            calls, an override of the callee might be invoked. Also, the callee
   *                            can be abstract. Contains a warning message if the callee MethodNode
   *                            cannot be found in the bytecode repository.
   * @param argInfos            Information about the invocation receiver and arguments
   * @param callsiteStackHeight The stack height at the callsite, required by the inliner
   * @param callsitePosition    The source position of the callsite, used for inliner warnings.
   */
  final case class Callsite(callsiteInstruction: MethodInsnNode, callsiteMethod: MethodNode, callsiteClass: ClassBType,
                            callee: Either[OptimizerWarning, Callee], argInfos: IntMap[ArgInfo],
                            callsiteStackHeight: Int, receiverKnownNotNull: Boolean, callsitePosition: Position,
                            annotatedInline: Boolean, annotatedNoInline: Boolean) {
    /**
     * Contains callsites that were created during inlining by cloning this callsite. Used to find
     * corresponding callsites when inlining post-inline requests.
     */
    val inlinedClones = mutable.Set.empty[ClonedCallsite]

    override def toString =
      "Invocation of" +
        s" ${callee.map(_.calleeDeclarationClass.internalName).getOrElse("?")}.${callsiteInstruction.name + callsiteInstruction.desc}" +
        s"@${callsiteMethod.instructions.indexOf(callsiteInstruction)}" +
        s" in ${callsiteClass.internalName}.${callsiteMethod.name}${callsiteMethod.desc}"
  }

  final case class ClonedCallsite(callsite: Callsite, clonedWhenInlining: Callsite)

  /**
   * Information about invocation arguments, obtained through data flow analysis of the callsite method.
   */
  sealed trait ArgInfo
  case object FunctionLiteral extends ArgInfo
  final case class ForwardedParam(index: Int) extends ArgInfo
  //  final case class ArgTypeInfo(argType: BType, isPrecise: Boolean, knownNotNull: Boolean) extends ArgInfo
  // can be extended, e.g., with constant types

  /**
   * A callee in the call graph.
   *
   * @param callee                 The callee, as it appears in the invocation instruction. For
   *                               virtual calls, an override of the callee might be invoked. Also,
   *                               the callee can be abstract.
   * @param calleeDeclarationClass The class in which the callee is declared
   * @param safeToInline           True if the callee can be safely inlined: it cannot be overridden,
   *                               and the inliner settings (project / global) allow inlining it.
   * @param annotatedInline        True if the callee is annotated @inline
   * @param annotatedNoInline      True if the callee is annotated @noinline
   * @param samParamTypes          A map from parameter positions to SAM parameter types
   * @param calleeInfoWarning      An inliner warning if some information was not available while
   *                               gathering the information about this callee.
   */
  final case class Callee(callee: MethodNode, calleeDeclarationClass: btypes.ClassBType,
                          safeToInline: Boolean, sourceFilePath: Option[String],
                          annotatedInline: Boolean, annotatedNoInline: Boolean,
                          samParamTypes: IntMap[btypes.ClassBType],
                          calleeInfoWarning: Option[CalleeInfoWarning]) {
    override def toString = s"Callee($calleeDeclarationClass.${callee.name})"
  }

  /**
   * Metadata about a closure instantiation, stored in the call graph
   *
   * @param lambdaMetaFactoryCall the InvokeDynamic instruction
   * @param ownerMethod           the method where the closure is allocated
   * @param ownerClass            the class containing the above method
   * @param capturedArgInfos      information about captured arguments. Used for updating the call
   *                              graph when re-writing a closure invocation to the body method.
   */
  final case class ClosureInstantiation(lambdaMetaFactoryCall: LambdaMetaFactoryCall, ownerMethod: MethodNode, ownerClass: ClassBType, capturedArgInfos: IntMap[ArgInfo]) {
    /**
     * Contains closure instantiations that were created during inlining by cloning this instantiation.
     */
    val inlinedClones = mutable.Set.empty[ClosureInstantiation]
    override def toString = s"ClosureInstantiation($lambdaMetaFactoryCall, ${ownerMethod.name + ownerMethod.desc}, $ownerClass)"
  }
  final case class LambdaMetaFactoryCall(indy: InvokeDynamicInsnNode, samMethodType: Type, implMethod: Handle, instantiatedMethodType: Type)

  object LambdaMetaFactoryCall {
    def unapply(insn: AbstractInsnNode): Option[(InvokeDynamicInsnNode, Type, Handle, Type)] = insn match {
      case indy: InvokeDynamicInsnNode if indy.bsm == coreBTypes.lambdaMetaFactoryMetafactoryHandle || indy.bsm == coreBTypes.lambdaMetaFactoryAltMetafactoryHandle =>
        indy.bsmArgs match {
          case Array(samMethodType: Type, implMethod: Handle, instantiatedMethodType: Type, _@_*) =>
            // LambdaMetaFactory performs a number of automatic adaptations when invoking the lambda
            // implementation method (casting, boxing, unboxing, and primitive widening, see Javadoc).
            //
            // The closure optimizer supports only one of those adaptations: it will cast arguments
            // to the correct type when re-writing a closure call to the body method. Example:
            //
            //   val fun: String => String = l => l
            //   val l = List("")
            //   fun(l.head)
            //
            // The samMethodType of Function1 is `(Object)Object`, while the instantiatedMethodType
            // is `(String)String`. The return type of `List.head` is `Object`.
            //
            // The implMethod has the signature `C$anonfun(String)String`.
            //
            // At the closure callsite, we have an `INVOKEINTERFACE Function1.apply (Object)Object`,
            // so the object returned by `List.head` can be directly passed into the call (no cast).
            //
            // The closure object will cast the object to String before passing it to the implMethod.
            //
            // When re-writing the closure callsite to the implMethod, we have to insert a cast.
            //
            // The check below ensures that
            //   (1) the implMethod type has the expected signature (captured types plus argument types
            //       from instantiatedMethodType)
            //   (2) the receiver of the implMethod matches the first captured type
            //   (3) all parameters that are not the same in samMethodType and instantiatedMethodType
            //       are reference types, so that we can insert casts to perform the same adaptation
            //       that the closure object would.

            val isStatic                   = implMethod.getTag == Opcodes.H_INVOKESTATIC
            val indyParamTypes             = Type.getArgumentTypes(indy.desc)
            val instantiatedMethodArgTypes = instantiatedMethodType.getArgumentTypes
            val expectedImplMethodType     = {
              val paramTypes = (if (isStatic) indyParamTypes else indyParamTypes.tail) ++ instantiatedMethodArgTypes
              Type.getMethodType(instantiatedMethodType.getReturnType, paramTypes: _*)
            }

            val isIndyLambda = (
                 Type.getType(implMethod.getDesc) == expectedImplMethodType              // (1)
              && (isStatic || implMethod.getOwner == indyParamTypes(0).getInternalName)  // (2)
              && samMethodType.getArgumentTypes.corresponds(instantiatedMethodArgTypes)((samArgType, instArgType) =>
                   samArgType == instArgType || isReference(samArgType) && isReference(instArgType)) // (3)
            )

            if (isIndyLambda) Some((indy, samMethodType, implMethod, instantiatedMethodType))
            else None

          case _ => None
        }
      case _ => None
    }
  }
}
