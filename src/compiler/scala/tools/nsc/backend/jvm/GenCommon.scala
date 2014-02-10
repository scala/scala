/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Jason Zaugg
 */

package scala.tools.nsc
package backend.jvm
import scala.tools.nsc.symtab._

trait GenCommon extends BytecodeWriters {
  import global._
  import definitions.{ AnnotationRetentionAttr, AnnotationRetentionPolicyAttr, ClassfileAnnotationClass }

  // -----------------------------------------------------------------------------------------
  // Static forwarders (related to mirror classes but also present in
  // a plain class lacking companion module, for details see `isCandidateForForwarders`).
  // -----------------------------------------------------------------------------------------
  val ExcludedForwarderFlags = {
    import Flags._
    // Should include DEFERRED but this breaks findMember.
    (SPECIALIZED | LIFTED | PROTECTED | STATIC | EXPANDEDNAME | BridgeAndPrivateFlags | MACRO)
  }

  lazy val AnnotationRetentionPolicyModule = AnnotationRetentionPolicyAttr.companionModule
  lazy val AnnotationRetentionPolicySourceValue = AnnotationRetentionPolicyModule.tpe.member(TermName("SOURCE"))
  lazy val AnnotationRetentionPolicyClassValue = AnnotationRetentionPolicyModule.tpe.member(TermName("CLASS"))
  lazy val AnnotationRetentionPolicyRuntimeValue = AnnotationRetentionPolicyModule.tpe.member(TermName("RUNTIME"))

  /**
    * Whether an annotation should be emitted as a Java annotation
    * .initialize: if 'annot' is read from pickle, atp might be un-initialized
    */
  def shouldEmitAnnotation(annot: AnnotationInfo) =
    annot.symbol.initialize.isJavaDefined &&
      annot.matches(ClassfileAnnotationClass) &&
      retentionPolicyOf(annot) != AnnotationRetentionPolicySourceValue &&
      annot.args.isEmpty

  def isRuntimeVisible(annot: AnnotationInfo): Boolean =
    annot.atp.getAnnotation(AnnotationRetentionAttr)
      .exists(_.assocs.contains((nme.value -> LiteralAnnotArg(Constant(AnnotationRetentionPolicyRuntimeValue)))))

  def retentionPolicyOf(annot: AnnotationInfo): Symbol =
    annot.atp.getAnnotation(AnnotationRetentionAttr).map(_.assocs).map(assoc =>
      assoc.collectFirst {
        case (`nme`.value, LiteralAnnotArg(Constant(value: Symbol))) => value
      }.getOrElse(AnnotationRetentionPolicyClassValue)).getOrElse(AnnotationRetentionPolicyClassValue)
}
