package scala.reflect.internal

abstract class PerRunSettings {

  val isScala211: Boolean
  val isScala212: Boolean
  val isScala213: Boolean

  //optimise settings
  val optNone: Boolean
  val optUnreachableCode: Boolean
  val optSimplifyJumps: Boolean
  val optCompactLocals: Boolean
  val optCopyPropagation: Boolean
  val optRedundantCasts: Boolean
  val optBoxUnbox: Boolean
  val optNullnessTracking: Boolean
  val optClosureInvocations: Boolean
  val optInlineProject: Boolean
  val optInlineGlobal: Boolean
  val optInlinerEnabled: Boolean
  val optBuildCallGraph: Boolean
  val optAddToBytecodeRepository: Boolean


  val YoptTrace_isSetByUser : Boolean
  val YoptTrace : String

  val optWarningsSummaryOnly : Boolean

  val optWarningEmitAtInlineFailed : Boolean

  val optWarningNoInlineMixed                      : Boolean
  val optWarningNoInlineMissingBytecode            : Boolean
  val optWarningNoInlineMissingScalaInlineInfoAttr : Boolean
  val optWarningAnyInlineFailed                    : Boolean

  val Xexperimental : Boolean

  //---
  val YoptLogInline : String
  val YoptLogInline_isSetByUser : Boolean
  val YoptInlineHeuristics : String

  val debug : Boolean

}
class SimplePerRunSettings extends PerRunSettings {
  override val isScala211: Boolean = true
  override val isScala212: Boolean = true
  override val isScala213: Boolean = true
  override val optNone: Boolean = true
  override val optUnreachableCode: Boolean = true
  override val optSimplifyJumps: Boolean = true
  override val optCompactLocals: Boolean = true
  override val optCopyPropagation: Boolean = true
  override val optRedundantCasts: Boolean = true
  override val optBoxUnbox: Boolean = true
  override val optNullnessTracking: Boolean = true
  override val optClosureInvocations: Boolean = true
  override val optInlineProject: Boolean = true
  override val optInlineGlobal: Boolean = true
  override val optInlinerEnabled: Boolean = true
  override val optBuildCallGraph: Boolean = true
  override val optAddToBytecodeRepository: Boolean = true
  override val YoptTrace_isSetByUser: Boolean = false
  override val YoptTrace: String = ""
  override val optWarningsSummaryOnly: Boolean = true
  override val optWarningEmitAtInlineFailed: Boolean = true
  override val optWarningNoInlineMixed: Boolean = true
  override val optWarningNoInlineMissingBytecode: Boolean = true
  override val optWarningNoInlineMissingScalaInlineInfoAttr: Boolean = true
  override val optWarningAnyInlineFailed                    = true
  //---
  override val YoptLogInline = ""
  override val YoptLogInline_isSetByUser = false
  override val YoptInlineHeuristics = "???"
  override val debug = false
  override val Xexperimental = false
}
