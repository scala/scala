package scala.tools.nsc
package backend.jvm

import scala.tools.asm.tree.{AbstractInsnNode, MethodNode}
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.reflect.internal.util.Position
import scala.tools.nsc.settings.ScalaSettings
import scala.util.control.ControlThrowable

/**
 * Interface for emitting inline warnings. The interface is required because the implementation
 * depends on Global, which is not available in BTypes (only in BTypesFromSymbols).
 */
sealed abstract class BackendReporting {
  def inlinerWarning(pos: Position, message: String): Unit
}

final class BackendReportingImpl(val global: Global) extends BackendReporting {
  import global._

  def inlinerWarning(pos: Position, message: String): Unit = {
    currentRun.reporting.inlinerWarning(pos, message)
  }
}

/**
 * Utilities for error reporting.
 *
 * Defines some utility methods to make error reporting with Either easier.
 */
object BackendReporting {
  def methodSignature(classInternalName: InternalName, name: String, desc: String) = {
    classInternalName + "::" + name + desc
  }

  def methodSignature(classInternalName: InternalName, method: MethodNode): String = {
    methodSignature(classInternalName, method.name, method.desc)
  }

  def assertionError(message: String): Nothing = throw new AssertionError(message)

  implicit class RightBiasedEither[A, B](val v: Either[A, B]) extends AnyVal {
    def withFilter(f: B => Boolean)(implicit empty: A): Either[A, B] = v match {
      case Left(_)  => v
      case Right(e) => if (f(e)) v else Left(empty) // scalaz.\/ requires an implicit Monoid m to get m.empty
    }

    /** Get the value, fail with an assertion if this is an error. */
    def get: B = {
      assert(v.isRight, v.left.get)
      v.right.get
    }

    /**
     * Get the right value of an `Either` by throwing a potential error message. Can simplify the
     * implementation of methods that act on multiple `Either` instances. Instead of flat-mapping,
     * the first error can be collected as
     *
     *     tryEither {
     *       eitherOne.orThrow .... eitherTwo.orThrow ... eitherThree.orThrow
     *     }
     */
    def orThrow: B = v match {
      case Left(m)  => throw Invalid(m)
      case Right(t) => t
    }
  }

  case class Invalid[A](e: A) extends ControlThrowable

  /**
   * See documentation of orThrow above.
   */
  def tryEither[A, B](op: => Either[A, B]): Either[A, B] = try { op } catch { case Invalid(e) => Left(e.asInstanceOf[A]) }

  sealed trait OptimizerWarning {
    def emitWarning(settings: ScalaSettings): Boolean
  }

  // Method withFilter in RightBiasedEither requires an implicit empty value. Taking the value here
  // in scope allows for-comprehensions that desugar into withFilter calls (for example when using a
  // tuple de-constructor).
  implicit object emptyOptimizerWarning extends OptimizerWarning {
    def emitWarning(settings: ScalaSettings): Boolean = false
  }

  sealed trait MissingBytecodeWarning extends OptimizerWarning {
    override def toString = this match {
      case ClassNotFound(internalName, definedInJavaSource) =>
        s"The classfile for $internalName could not be found on the compilation classpath." + {
          if (definedInJavaSource) "\nThe class is defined in a Java source file that is being compiled (mixed compilation), therefore no bytecode is available."
          else ""
        }

      case MethodNotFound(name, descriptor, ownerInternalName, missingClass) =>
        val missingClassWarning = missingClass match {
          case None => ""
          case Some(c) =>
            if (c.definedInJavaSource) s"\nNote that class ${c.internalName} is defined in a Java source (mixed compilation), no bytecode is available."
            else s"\nNote that class ${c.internalName} could not be found on the classpath."
        }
        s"The method $name$descriptor could not be found in the class $ownerInternalName or any of its parents." + missingClassWarning

      case FieldNotFound(name, descriptor, ownerInternalName, missingClass) =>
        s"The field node $name$descriptor could not be found because the classfile $ownerInternalName cannot be found on the classpath." +
          missingClass.map(c => s" Reason:\n$c").getOrElse("")
    }

    def emitWarning(settings: ScalaSettings): Boolean = this match {
      case ClassNotFound(_, javaDefined) =>
        if (javaDefined) settings.optWarningNoInlineMixed
        else settings.optWarningNoInlineMissingBytecode

      case m @ MethodNotFound(_, _, _, missing) =>
        if (m.isArrayMethod) false
        else settings.optWarningNoInlineMissingBytecode || missing.exists(_.emitWarning(settings))

      case FieldNotFound(_, _, _, missing) =>
        settings.optWarningNoInlineMissingBytecode || missing.exists(_.emitWarning(settings))
    }
  }

  case class ClassNotFound(internalName: InternalName, definedInJavaSource: Boolean) extends MissingBytecodeWarning
  case class MethodNotFound(name: String, descriptor: String, ownerInternalNameOrArrayDescriptor: InternalName, missingClass: Option[ClassNotFound]) extends MissingBytecodeWarning {
    def isArrayMethod = ownerInternalNameOrArrayDescriptor.charAt(0) == '['
  }
  case class FieldNotFound(name: String, descriptor: String, ownerInternalName: InternalName, missingClass: Option[ClassNotFound]) extends MissingBytecodeWarning

  sealed trait NoClassBTypeInfo extends OptimizerWarning {
    override def toString = this match {
      case NoClassBTypeInfoMissingBytecode(cause) =>
        cause.toString

      case NoClassBTypeInfoClassSymbolInfoFailedSI9111(classFullName) =>
        s"Failed to get the type of class symbol $classFullName due to SI-9111."
    }

    def emitWarning(settings: ScalaSettings): Boolean = this match {
      case NoClassBTypeInfoMissingBytecode(cause)         => cause.emitWarning(settings)
      case NoClassBTypeInfoClassSymbolInfoFailedSI9111(_) => settings.optWarningNoInlineMissingBytecode
    }
  }

  case class NoClassBTypeInfoMissingBytecode(cause: MissingBytecodeWarning) extends NoClassBTypeInfo
  case class NoClassBTypeInfoClassSymbolInfoFailedSI9111(classFullName: String) extends NoClassBTypeInfo

  /**
   * Used in the CallGraph for nodes where an issue occurred determining the callee information.
   */
  sealed trait CalleeInfoWarning extends OptimizerWarning {
    def declarationClass: InternalName
    def name: String
    def descriptor: String

    def warningMessageSignature = BackendReporting.methodSignature(declarationClass, name, descriptor)

    override def toString = this match {
      case MethodInlineInfoIncomplete(_, _, _, cause) =>
        s"The inline information for $warningMessageSignature may be incomplete:\n" + cause

      case MethodInlineInfoMissing(_, _, _, cause) =>
        s"No inline information for method $warningMessageSignature could be found." +
          cause.map(" Possible reason:\n" + _).getOrElse("")

      case MethodInlineInfoError(_, _, _, cause) =>
        s"Error while computing the inline information for method $warningMessageSignature:\n" + cause
    }

    def emitWarning(settings: ScalaSettings): Boolean = this match {
      case MethodInlineInfoIncomplete(_, _, _, cause)               => cause.emitWarning(settings)

      case MethodInlineInfoMissing(_, _, _, Some(cause))            => cause.emitWarning(settings)
      case MethodInlineInfoMissing(_, _, _, None)                   => settings.optWarningNoInlineMissingBytecode

      case MethodInlineInfoError(_, _, _, cause)                    => cause.emitWarning(settings)
    }
  }

  case class MethodInlineInfoIncomplete(declarationClass: InternalName, name: String, descriptor: String, cause: ClassInlineInfoWarning) extends CalleeInfoWarning
  case class MethodInlineInfoMissing(declarationClass: InternalName, name: String, descriptor: String, cause: Option[ClassInlineInfoWarning]) extends CalleeInfoWarning
  case class MethodInlineInfoError(declarationClass: InternalName, name: String, descriptor: String, cause: NoClassBTypeInfo) extends CalleeInfoWarning

  sealed trait CannotInlineWarning extends OptimizerWarning {
    def calleeDeclarationClass: InternalName
    def name: String
    def descriptor: String

    def calleeMethodSig = BackendReporting.methodSignature(calleeDeclarationClass, name, descriptor)

    override def toString = this match {
      case IllegalAccessInstruction(_, _, _, callsiteClass, instruction) =>
        s"The callee $calleeMethodSig contains the instruction ${AsmUtils.textify(instruction)}" +
          s"\nthat would cause an IllegalAccessError when inlined into class $callsiteClass."

      case IllegalAccessCheckFailed(_, _, _, callsiteClass, instruction, cause) =>
        s"Failed to check if $calleeMethodSig can be safely inlined to $callsiteClass without causing an IllegalAccessError. Checking instruction ${AsmUtils.textify(instruction)} failed:\n" + cause

      case MethodWithHandlerCalledOnNonEmptyStack(_, _, _, callsiteClass, callsiteName, callsiteDesc) =>
        s"""The operand stack at the callsite in ${BackendReporting.methodSignature(callsiteClass, callsiteName, callsiteDesc)} contains more values than the
           |arguments expected by the callee $calleeMethodSig. These values would be discarded
           |when entering an exception handler declared in the inlined method.""".stripMargin

      case SynchronizedMethod(_, _, _) =>
        s"Method $calleeMethodSig cannot be inlined because it is synchronized."

      case StrictfpMismatch(_, _, _, callsiteClass, callsiteName, callsiteDesc) =>
        s"""The callsite method ${BackendReporting.methodSignature(callsiteClass, callsiteName, callsiteDesc)}
           |does not have the same strictfp mode as the callee $calleeMethodSig.
         """.stripMargin

      case ResultingMethodTooLarge(_, _, _, callsiteClass, callsiteName, callsiteDesc) =>
        s"""The size of the callsite method ${BackendReporting.methodSignature(callsiteClass, callsiteName, callsiteDesc)}
           |would exceed the JVM method size limit after inlining $calleeMethodSig.
         """.stripMargin
    }

    def emitWarning(settings: ScalaSettings): Boolean = this match {
      case _: IllegalAccessInstruction | _: MethodWithHandlerCalledOnNonEmptyStack | _: SynchronizedMethod | _: StrictfpMismatch | _: ResultingMethodTooLarge =>
        settings.optWarnings.contains(settings.optWarningsChoices.anyInlineFailed)

      case IllegalAccessCheckFailed(_, _, _, _, _, cause) =>
        cause.emitWarning(settings)
    }
  }
  case class IllegalAccessInstruction(calleeDeclarationClass: InternalName, name: String, descriptor: String,
                                      callsiteClass: InternalName, instruction: AbstractInsnNode) extends CannotInlineWarning
  case class IllegalAccessCheckFailed(calleeDeclarationClass: InternalName, name: String, descriptor: String,
                                      callsiteClass: InternalName, instruction: AbstractInsnNode, cause: OptimizerWarning) extends CannotInlineWarning
  case class MethodWithHandlerCalledOnNonEmptyStack(calleeDeclarationClass: InternalName, name: String, descriptor: String,
                                                    callsiteClass: InternalName, callsiteName: String, callsiteDesc: String) extends CannotInlineWarning
  case class SynchronizedMethod(calleeDeclarationClass: InternalName, name: String, descriptor: String) extends CannotInlineWarning
  case class StrictfpMismatch(calleeDeclarationClass: InternalName, name: String, descriptor: String,
                              callsiteClass: InternalName, callsiteName: String, callsiteDesc: String) extends CannotInlineWarning
  case class ResultingMethodTooLarge(calleeDeclarationClass: InternalName, name: String, descriptor: String,
                                     callsiteClass: InternalName, callsiteName: String, callsiteDesc: String) extends CannotInlineWarning

  // TODO: this should be a subtype of CannotInlineWarning
  // but at the place where it's created (in findIllegalAccess) we don't have the necessary data (calleeName, calleeDescriptor).
  case object UnknownInvokeDynamicInstruction extends OptimizerWarning {
    override def toString = "The callee contains an InvokeDynamic instruction with an unknown bootstrap method (not a LambdaMetaFactory)."
    def emitWarning(settings: ScalaSettings): Boolean = settings.optWarnings.contains(settings.optWarningsChoices.anyInlineFailed)
  }

  /**
   * Used in `rewriteClosureApplyInvocations` when a closure apply callsite cannot be rewritten
   * to the closure body method.
   */
  sealed trait RewriteClosureApplyToClosureBodyFailed extends OptimizerWarning {
    def pos: Position

    override def emitWarning(settings: ScalaSettings): Boolean = this match {
      case RewriteClosureAccessCheckFailed(_, cause) => cause.emitWarning(settings)
      case RewriteClosureIllegalAccess(_, _)         => settings.optWarnings.contains(settings.optWarningsChoices.anyInlineFailed)
    }

    override def toString: String = this match {
      case RewriteClosureAccessCheckFailed(_, cause) =>
        s"Failed to rewrite the closure invocation to its implementation method:\n" + cause
      case RewriteClosureIllegalAccess(_, callsiteClass) =>
        s"The closure body invocation cannot be rewritten because the target method is not accessible in class $callsiteClass."
    }
  }
  case class RewriteClosureAccessCheckFailed(pos: Position, cause: OptimizerWarning) extends RewriteClosureApplyToClosureBodyFailed
  case class RewriteClosureIllegalAccess(pos: Position, callsiteClass: InternalName) extends RewriteClosureApplyToClosureBodyFailed

  /**
   * Used in the InlineInfo of a ClassBType, when some issue occurred obtaining the inline information.
   */
  sealed trait ClassInlineInfoWarning extends OptimizerWarning {
    override def toString = this match {
      case NoInlineInfoAttribute(internalName) =>
        s"The Scala classfile $internalName does not have a ScalaInlineInfo attribute."

      case ClassSymbolInfoFailureSI9111(classFullName) =>
        s"Failed to get the type of a method of class symbol $classFullName due to SI-9111."

      case ClassNotFoundWhenBuildingInlineInfoFromSymbol(missingClass) =>
        s"Failed to build the inline information: $missingClass"

      case UnknownScalaInlineInfoVersion(internalName, version) =>
        s"Cannot read ScalaInlineInfo version $version in classfile $internalName. Use a more recent compiler."
    }

    def emitWarning(settings: ScalaSettings): Boolean = this match {
      case NoInlineInfoAttribute(_)                             => settings.optWarningNoInlineMissingScalaInlineInfoAttr
      case ClassNotFoundWhenBuildingInlineInfoFromSymbol(cause) => cause.emitWarning(settings)
      case ClassSymbolInfoFailureSI9111(_)                      => settings.optWarningNoInlineMissingBytecode
      case UnknownScalaInlineInfoVersion(_, _)                  => settings.optWarningNoInlineMissingScalaInlineInfoAttr
    }
  }

  case class NoInlineInfoAttribute(internalName: InternalName) extends ClassInlineInfoWarning
  case class ClassSymbolInfoFailureSI9111(classFullName: String) extends ClassInlineInfoWarning
  case class ClassNotFoundWhenBuildingInlineInfoFromSymbol(missingClass: ClassNotFound) extends ClassInlineInfoWarning
  case class UnknownScalaInlineInfoVersion(internalName: InternalName, version: Int) extends ClassInlineInfoWarning
}
