/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.reflect.internal.util.{NoPosition, Position}
import scala.tools.asm.{Opcodes, Type}
import scala.tools.asm.tree._
import scala.collection.convert.decorateAsScala._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.nsc.backend.jvm.analysis.{NotNull, NullnessAnalyzer}
import ByteCodeRepository.{Source, CompilationUnit}
import BytecodeUtils._

class CallGraph[BT <: BTypes](val btypes: BT) {
  import btypes._

  val callsites: collection.concurrent.Map[MethodInsnNode, Callsite] = recordPerRunCache(collection.concurrent.TrieMap.empty[MethodInsnNode, Callsite])

  def addClass(classNode: ClassNode): Unit = {
    for (m <- classNode.methods.asScala; callsite <- analyzeCallsites(m, classBTypeFromClassNode(classNode)))
      callsites(callsite.callsiteInstruction) = callsite
  }

  def analyzeCallsites(methodNode: MethodNode, definingClass: ClassBType): List[Callsite] = {

    case class CallsiteInfo(safeToInline: Boolean, safeToRewrite: Boolean,
                            annotatedInline: Boolean, annotatedNoInline: Boolean,
                            warning: Option[CalleeInfoWarning])

    /**
     * Analyze a callsite and gather meta-data that can be used for inlining decisions.
     */
    def analyzeCallsite(calleeMethodNode: MethodNode, calleeDeclarationClassBType: ClassBType, receiverTypeInternalName: InternalName, calleeSource: Source): CallsiteInfo = {
      val methodSignature = calleeMethodNode.name + calleeMethodNode.desc

      try {
        // The inlineInfo.methodInfos of a ClassBType holds an InlineInfo for each method *declared*
        // within a class (not for inherited methods). Since we already have the  classBType of the
        // callee, we only check there for the methodInlineInfo, we should find it there.
        calleeDeclarationClassBType.info.orThrow.inlineInfo.methodInfos.get(methodSignature) match {
          case Some(methodInlineInfo) =>
            val canInlineFromSource = compilerSettings.YoptInlineGlobal || calleeSource == CompilationUnit

            val isAbstract = BytecodeUtils.isAbstractMethod(calleeMethodNode)

            // (1) A non-final method can be safe to inline if the receiver type is a final subclass. Example:
            //   class A { @inline def f = 1 }; object B extends A; B.f  // can be inlined
            //
            // TODO: type analysis can render more calls statically resolved. Example:
            //   new A.f  // can be inlined, the receiver type is known to be exactly A.
            val isStaticallyResolved: Boolean = {
              methodInlineInfo.effectivelyFinal ||
                classBTypeFromParsedClassfile(receiverTypeInternalName).info.orThrow.inlineInfo.isEffectivelyFinal // (1)
            }

            val isRewritableTraitCall = isStaticallyResolved && methodInlineInfo.traitMethodWithStaticImplementation

            val warning = calleeDeclarationClassBType.info.orThrow.inlineInfo.warning.map(
              MethodInlineInfoIncomplete(calleeDeclarationClassBType.internalName, calleeMethodNode.name, calleeMethodNode.desc, _))

            // (1) For invocations of final trait methods, the callee isStaticallyResolved but also
            //     abstract. Such a callee is not safe to inline - it needs to be re-written to the
            //     static impl method first (safeToRewrite).
            // (2) Final trait methods can be rewritten from the interface to the static implementation
            //     method to enable inlining.
            CallsiteInfo(
              safeToInline      =
                canInlineFromSource &&
                isStaticallyResolved &&  // (1)
                !isAbstract &&
                !BytecodeUtils.isConstructor(calleeMethodNode) &&
                !BytecodeUtils.isNativeMethod(calleeMethodNode),
              safeToRewrite     = canInlineFromSource && isRewritableTraitCall, // (2)
              annotatedInline   = methodInlineInfo.annotatedInline,
              annotatedNoInline = methodInlineInfo.annotatedNoInline,
              warning           = warning)

          case None =>
            val warning = MethodInlineInfoMissing(calleeDeclarationClassBType.internalName, calleeMethodNode.name, calleeMethodNode.desc, calleeDeclarationClassBType.info.orThrow.inlineInfo.warning)
            CallsiteInfo(false, false, false, false, Some(warning))
        }
      } catch {
        case Invalid(noInfo: NoClassBTypeInfo) =>
          val warning = MethodInlineInfoError(calleeDeclarationClassBType.internalName, calleeMethodNode.name, calleeMethodNode.desc, noInfo)
          CallsiteInfo(false, false, false, false, Some(warning))
      }
    }

    // TODO: run dataflow analyses to make the call graph more precise
    //  - producers to get forwarded parameters (ForwardedParam)
    //  - typeAnalysis for more precise argument types, more precise callee

    // For now we run a NullnessAnalyzer. It is used to determine if the receiver of an instance
    // call is known to be not-null, in which case we don't have to emit a null check when inlining.
    // It is also used to get the stack height at the call site.
    localOpt.minimalRemoveUnreachableCode(methodNode, definingClass.internalName)
    val analyzer = new NullnessAnalyzer
    analyzer.analyze(definingClass.internalName, methodNode)

    methodNode.instructions.iterator.asScala.collect({
      case call: MethodInsnNode =>
        val callee: Either[OptimizerWarning, Callee] = for {
          (method, declarationClass)     <- byteCodeRepository.methodNode(call.owner, call.name, call.desc): Either[OptimizerWarning, (MethodNode, InternalName)]
          (declarationClassNode, source) <- byteCodeRepository.classNodeAndSource(declarationClass): Either[OptimizerWarning, (ClassNode, Source)]
          declarationClassBType          =  classBTypeFromClassNode(declarationClassNode)
        } yield {
          val CallsiteInfo(safeToInline, safeToRewrite, annotatedInline, annotatedNoInline, warning) = analyzeCallsite(method, declarationClassBType, call.owner, source)
          Callee(
            callee = method,
            calleeDeclarationClass = declarationClassBType,
            safeToInline = safeToInline,
            safeToRewrite = safeToRewrite,
            annotatedInline = annotatedInline,
            annotatedNoInline = annotatedNoInline,
            calleeInfoWarning = warning)
        }

        val argInfos = if (callee.isLeft) Nil else {
          // TODO: for now it's Nil, because we don't run any data flow analysis
          // there's no point in using the parameter types, that doesn't add any information.
          // NOTE: need to run the same analyses after inlining, to re-compute the argInfos for the
          // new duplicated callsites, see Inliner.inline
          Nil
        }

        val receiverNotNull = call.getOpcode == Opcodes.INVOKESTATIC || {
          val numArgs = Type.getArgumentTypes(call.desc).length
          val frame = analyzer.frameAt(call, methodNode)
          frame.getStack(frame.getStackSize - 1 - numArgs).nullness == NotNull
        }

        Callsite(
          callsiteInstruction = call,
          callsiteMethod = methodNode,
          callsiteClass = definingClass,
          callee = callee,
          argInfos = argInfos,
          callsiteStackHeight = analyzer.frameAt(call, methodNode).getStackSize,
          receiverKnownNotNull = receiverNotNull,
          callsitePosition = callsitePositions.getOrElse(call, NoPosition)
        )
    }).toList
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
                            callee: Either[OptimizerWarning, Callee], argInfos: List[ArgInfo],
                            callsiteStackHeight: Int, receiverKnownNotNull: Boolean, callsitePosition: Position) {
    override def toString =
      "Invocation of" +
        s" ${callee.map(_.calleeDeclarationClass.internalName).getOrElse("?")}.${callsiteInstruction.name + callsiteInstruction.desc}" +
        s"@${callsiteMethod.instructions.indexOf(callsiteInstruction)}" +
        s" in ${callsiteClass.internalName}.${callsiteMethod.name}"
  }

  /**
   * Information about invocation arguments, obtained through data flow analysis of the callsite method.
   */
  sealed trait ArgInfo
  final case class ArgTypeInfo(argType: BType, isPrecise: Boolean, knownNotNull: Boolean) extends ArgInfo
  final case class ForwardedParam(index: Int) extends ArgInfo
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
   * @param safeToRewrite          True if the callee the interface method of a concrete trait method
   *                               that can be safely re-written to the static implementation method.
   * @param annotatedInline        True if the callee is annotated @inline
   * @param annotatedNoInline      True if the callee is annotated @noinline
   * @param calleeInfoWarning      An inliner warning if some information was not available while
   *                               gathering the information about this callee.
   */
  final case class Callee(callee: MethodNode, calleeDeclarationClass: ClassBType,
                          safeToInline: Boolean, safeToRewrite: Boolean,
                          annotatedInline: Boolean, annotatedNoInline: Boolean,
                          calleeInfoWarning: Option[CalleeInfoWarning]) {
    assert(!(safeToInline && safeToRewrite), s"A callee of ${callee.name} can be either safeToInline or safeToRewrite, but not both.")
  }
}
