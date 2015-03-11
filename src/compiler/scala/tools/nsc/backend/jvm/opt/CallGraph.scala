/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.tools.asm.tree._
import scala.collection.convert.decorateAsScala._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils.BasicAnalyzer
import ByteCodeRepository.{Source, CompilationUnit}

class CallGraph[BT <: BTypes](val btypes: BT) {
  import btypes._

  val callsites: collection.concurrent.Map[MethodInsnNode, Callsite] = recordPerRunCache(collection.concurrent.TrieMap.empty[MethodInsnNode, Callsite])

  def addClass(classNode: ClassNode): Unit = {
    for (m <- classNode.methods.asScala; callsite <- analyzeCallsites(m, classBTypeFromClassNode(classNode)))
      callsites(callsite.callsiteInstruction) = callsite
  }

  def analyzeCallsites(methodNode: MethodNode, definingClass: ClassBType): List[Callsite] = {

    /**
     * Analyze a callsite and gather meta-data that can be used for inlining decisions.
     *
     * @return Three booleans indicating whether
     *           1. the callsite can be safely inlined
     *           2. the callee is annotated `@inline`
     *           3. the callee is annotated `@noinline`
     */
    def analyzeCallsite(calleeMethodNode: MethodNode, calleeDeclarationClassBType: ClassBType, receiverTypeInternalName: InternalName, calleeSource: Source): (Boolean, Boolean, Boolean) = {
      val methodSignature = calleeMethodNode.name + calleeMethodNode.desc

      // The inlineInfo.methodInfos of a ClassBType holds an InlineInfo for each method *declared*
      // within a class (not for inherited methods). Since we already have the  classBType of the
      // callee, we only check there for the methodInlineInfo, we should find it there.
      calleeDeclarationClassBType.info.inlineInfo.methodInfos.find(_._1 == methodSignature) match {
        case Some((_, methodInlineInfo)) =>
          val canInlineFromSource = inlineGlobalEnabled || calleeSource == CompilationUnit
          // A non-final method can be inline if the receiver type is a final subclass. Example:
          //   class A { @inline def f = 1 }; object B extends A; B.f // can be inlined
          def isStaticallyResolved: Boolean = {
            // TODO: type analysis can render more calls statically resolved
            // Example: `new A.f` can be inlined, the receiver type is known to be exactly A.
            methodInlineInfo.effectivelyFinal || {
              // TODO: inline warning when the receiver class cannot be found on the classpath
              classBTypeFromParsedClassfile(receiverTypeInternalName).exists(_.info.inlineInfo.isEffectivelyFinal)
            }
          }

          (canInlineFromSource && isStaticallyResolved, methodInlineInfo.annotatedInline, methodInlineInfo.annotatedNoInline)

        case None =>
          // TODO: issue inliner warning
          (false, false, false)
      }
    }

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
        val callee = byteCodeRepository.methodNode(call.owner, call.name, call.desc) flatMap {
          case (method, declarationClass) =>
            // TODO: log inliner warning if callee decl class cannot be found?
            byteCodeRepository.classNodeAndSource(declarationClass) map {
              case (declarationClassNode, source) =>
                val declarationClassBType = classBTypeFromClassNode(declarationClassNode)
                val (safeToInline, annotatedInline, annotatedNoInline) = analyzeCallsite(method, declarationClassBType, call.owner, source)
                Callee(
                  callee = method,
                  calleeDeclarationClass = declarationClassBType,
                  safeToInline = safeToInline,
                  annotatedInline = annotatedInline,
                  annotatedNoInline = annotatedNoInline
                )
            }
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
   *
   * @param callsiteInstruction The invocation instruction
   * @param callsiteMethod      The method containing the callsite
   * @param callsiteClass       The class containing the callsite
   * @param callee              The callee, as it appears in the invocation instruction. For virtual
   *                            calls, an override of the callee might be invoked. Also, the callee
   *                            can be abstract. `None` if the callee MethodNode cannot be found in
   *                            the bytecode repository.
   * @param argInfos            Information about the invocation receiver and arguments
   * @param callsiteStackHeight The stack height at the callsite, required by the inliner
   */
  final case class Callsite(callsiteInstruction: MethodInsnNode, callsiteMethod: MethodNode, callsiteClass: ClassBType,
                            callee: Option[Callee], argInfos: List[ArgInfo],
                            callsiteStackHeight: Int) {
    override def toString = s"Invocation of ${callee.map(_.calleeDeclarationClass.internalName).getOrElse("?")}.${callsiteInstruction.name + callsiteInstruction.desc}@${callsiteMethod.instructions.indexOf(callsiteInstruction)} in ${callsiteClass.internalName}.${callsiteMethod.name}"
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
   * @param annotatedInline        True if the callee is annotated @inline
   * @param annotatedNoInline      True if the callee is annotated @noinline
   */
  final case class Callee(callee: MethodNode, calleeDeclarationClass: ClassBType,
                          safeToInline: Boolean,
                          annotatedInline: Boolean, annotatedNoInline: Boolean)
}
