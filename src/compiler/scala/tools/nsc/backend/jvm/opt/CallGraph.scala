/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.tools.asm.tree._
import scala.collection.convert.decorateAsScala._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils.BasicAnalyzer

class CallGraph[BT <: BTypes](val btypes: BT) {
  import btypes._

  val callsites: collection.concurrent.Map[MethodInsnNode, Callsite] = recordPerRunCache(collection.concurrent.TrieMap.empty[MethodInsnNode, Callsite])

  def addClass(classNode: ClassNode): Unit = {
    for (m <- classNode.methods.asScala; callsite <- analyzeCallsites(m, classBTypeFromClassNode(classNode)))
      callsites(callsite.callsiteInstruction) = callsite
  }

  def analyzeCallsites(methodNode: MethodNode, definingClass: ClassBType): List[Callsite] = {
    // TODO: run dataflow analyses to make the call graph more precise
    //  - producers to get forwarded parameters (ForwardedParam)
    //  - typeAnalysis for more precise argument types, more precise callee
    //  - nullAnalysis to skip emitting the receiver-null-check when inlining

    // TODO: for now we run a basic analyzer to get the stack height at the call site.
    // once we run a more elaborate analyzer (types, nullness), we can get the stack height out of there.
    val analyzer = new BasicAnalyzer(methodNode, definingClass.internalName)

    methodNode.instructions.iterator.asScala.collect({
      case call: MethodInsnNode =>
        // TODO: log an inliner warning if the callee method cannot be found in the code repo? eg it's not on the classpath.
        val callee = byteCodeRepository.methodNode(call.owner, call.name, call.desc) map {
          case (method, declarationClass) =>
            val (declarationClassNode, source) = byteCodeRepository.classNodeAndSource(declarationClass)
            val declarationClassBType          = classBTypeFromClassNode(declarationClassNode)
            val methodSignature                = method.name + method.desc
            val (safeToInline, annotatedInline, annotatedNoInline) = declarationClassBType.info.inlineInfos.get(methodSignature) match {
              case Some(inlineInfo) =>
                val canInlineFromSource = inlineGlobalEnabled || source == ByteCodeRepository.CompilationUnit
                // TODO: for now, we consider a callee safeToInline only if it's final
                // type analysis can render more calls safeToInline (e.g. when the precise receiver type is known)
                (canInlineFromSource && inlineInfo.effectivelyFinal, Some(inlineInfo.annotatedInline), Some(inlineInfo.annotatedNoInline))
              case None =>
                (false, None, None)
            }
            Callee(
              callee = method,
              calleeDeclarationClass = declarationClassBType,
              safeToInline = safeToInline,
              annotatedInline = annotatedInline,
              annotatedNoInline = annotatedNoInline
            )
        }

        val argInfos = if (callee.isEmpty) Nil else {
          // TODO: for now it's Nil, because we don't run any data flow analysis
          // there's no point in using the parameter types, that doesn't add any information.
          // NOTE: need to run the same analyses after inlining, to re-compute the argInfos for the
          // new duplicated callsites, see Inliner.inline
          Nil
        }

        Callsite(
          callsiteInstruction = call,
          callsiteMethod = methodNode,
          callsiteClass = definingClass,
          callee = callee,
          argInfos = argInfos,
          callsiteStackHeight = analyzer.frameAt(call).getStackSize
        )
    }).toList
  }

  /**
   * A callsite in the call graph.
   * @param callsiteInstruction The invocation instruction
   * @param callsiteMethod      The method containing the callsite
   * @param callsiteClass       The class containing the callsite
   * @param callee              The callee. For virtual calls, an override of the callee might be invoked.
   * @param argInfos            Information about the invocation receiver and arguments
   * @param callsiteStackHeight The stack height at the callsite, required by the inliner
   */
  final case class Callsite(callsiteInstruction: MethodInsnNode, callsiteMethod: MethodNode, callsiteClass: ClassBType,
                            callee: Option[Callee], argInfos: List[ArgInfo],
                            callsiteStackHeight: Int) {
    override def toString = s"Invocation of ${callsiteInstruction.name + callsiteInstruction.desc}@${callsiteMethod.instructions.indexOf(callsiteInstruction)} in ${callsiteMethod.name}"
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
   * @param callee                 The called method. For virtual calls, an override may actually be invoked.
   * @param calleeDeclarationClass The class in which the callee is declared
   * @param safeToInline           True if the callee can be safely inlined: it cannot be overridden,
   *                               and the inliner settings (project / global) allow inlining it.
   * @param annotatedInline        Defined if it is known whether the callee is annotated @inline
   * @param annotatedNoInline      Defined if it is known whether the callee is annotated @noinline
   */
  final case class Callee(callee: MethodNode, calleeDeclarationClass: ClassBType,
                          safeToInline: Boolean,
                          annotatedInline: Option[Boolean], annotatedNoInline: Option[Boolean])
}
