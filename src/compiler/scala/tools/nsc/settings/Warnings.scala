/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package settings

/** Settings influencing the printing of warnings.
 */
trait Warnings {
  self: MutableSettings =>

  // Warning semantics.
  val fatalWarnings = BooleanSetting("-Xfatal-warnings", "Fail the compilation if there are any warnings.")

  // These warnings are all so noisy as to be useless in their
  // present form, but have the potential to offer useful info.
  protected def allWarnings = lintWarnings ++ List(
    warnDeadCode,
    warnSelectNullable,
    warnValueDiscard,
    warnNumericWiden
  )
  // These warnings should be pretty quiet unless you're doing
  // something inadvisable.
  protected def lintWarnings = List(
    warnInaccessible,
    warnNullaryOverride,
    warnNullaryUnit,
    warnAdaptedArgs,
    warnInferAny
  )

  // Warning groups.
  val lint = (
    BooleanSetting("-Xlint", "Enable recommended additional warnings.")
    withPostSetHook (_ => lintWarnings foreach (_.value = true))
  )

  /*val warnEverything = */ (
    BooleanSetting("-Ywarn-all", "Enable all -Y warnings.")
    withPostSetHook { _ =>
      lint.value = true
      allWarnings foreach (_.value = true)
    }
  )

  // Individual warnings.
  val warnSelectNullable   = BooleanSetting   ("-Xcheck-null", "Warn upon selection of nullable reference.")
  val warnAdaptedArgs      = BooleanSetting   ("-Ywarn-adapted-args", "Warn if an argument list is modified to match the receiver.")
  val warnDeadCode         = BooleanSetting   ("-Ywarn-dead-code", "Warn when dead code is identified.")
  val warnValueDiscard     = BooleanSetting   ("-Ywarn-value-discard", "Warn when non-Unit expression results are unused.")
  val warnNumericWiden     = BooleanSetting   ("-Ywarn-numeric-widen", "Warn when numerics are widened.")
  val warnNullaryUnit      = BooleanSetting   ("-Ywarn-nullary-unit", "Warn when nullary methods return Unit.")
  val warnInaccessible     = BooleanSetting   ("-Ywarn-inaccessible", "Warn about inaccessible types in method signatures.")
  val warnNullaryOverride  = BooleanSetting   ("-Ywarn-nullary-override",
    "Warn when non-nullary overrides nullary, e.g. `def foo()` over `def foo`.")
  val warnInferAny         = BooleanSetting   ("-Ywarn-infer-any", "Warn when a type argument is inferred to be `Any`.")

  // Backward compatibility.
  @deprecated("Use fatalWarnings", "2.11.0") def Xwarnfatal      = fatalWarnings         // used by sbt
  @deprecated("Use warnSelectNullable", "2.11.0") def Xchecknull = warnSelectNullable    // used by ide
  @deprecated("Use warnDeadCode", "2.11.0") def Ywarndeadcode    = warnDeadCode          // used by ide
}
