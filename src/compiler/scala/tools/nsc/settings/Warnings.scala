/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package settings

import language.existentials

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
    warnInferAny,
    // warnUnused       SI-7712, SI-7707 warnUnused not quite ready for prime-time
    // warnUnusedImport currently considered too noisy for general use
    warnMissingInterpolator
  )

  private lazy val warnSelectNullable = BooleanSetting("-Xcheck-null", "This option is obsolete and does nothing.")

  // Individual warnings.
  val warnAdaptedArgs      = BooleanSetting   ("-Ywarn-adapted-args", "Warn if an argument list is modified to match the receiver.")
  val warnDeadCode         = BooleanSetting   ("-Ywarn-dead-code", "Warn when dead code is identified.")
  val warnValueDiscard     = BooleanSetting   ("-Ywarn-value-discard", "Warn when non-Unit expression results are unused.")
  val warnNumericWiden     = BooleanSetting   ("-Ywarn-numeric-widen", "Warn when numerics are widened.")
  val warnNullaryUnit      = BooleanSetting   ("-Ywarn-nullary-unit", "Warn when nullary methods return Unit.")
  val warnInaccessible     = BooleanSetting   ("-Ywarn-inaccessible", "Warn about inaccessible types in method signatures.")
  val warnNullaryOverride  = BooleanSetting   ("-Ywarn-nullary-override", "Warn when non-nullary overrides nullary, e.g. `def foo()` over `def foo`.")
  val warnInferAny         = BooleanSetting   ("-Ywarn-infer-any", "Warn when a type argument is inferred to be `Any`.")
  val warnUnused           = BooleanSetting   ("-Ywarn-unused", "Warn when local and private vals, vars, defs, and types are are unused")
  val warnUnusedImport     = BooleanSetting   ("-Ywarn-unused-import", "Warn when imports are unused")

  // Lint warnings that are not -Y
  val warnMissingInterpolator = new BooleanSetting("warn-missing-interpolator", "Warn when a string literal appears to be missing an interpolator id.")

  // Warning groups.
  val lint                 = {
    // Boolean setting for testing if lint is on; not "added" to option processing
    val xlint = new BooleanSetting("-Xlint", "Enable recommended additional warnings.")
    def lintables = (lintWarnings map (_.name stripPrefix "-Y")).sorted
    def isAnon(b: BooleanSetting) = !(b.name startsWith "-")
    def setPolitely(b: BooleanSetting, v: Boolean) = if (!b.isSetByUser) b.value = v
    def set(w: String, v: Boolean) = lintWarnings find (_.name.stripPrefix("-Y") == w) foreach (b => setPolitely(b, v))
    val Neg = "-"
    def propagate(ss: List[String]): Unit = ss match {
      case w :: rest => if (w startsWith Neg) set(w stripPrefix Neg, false) else set(w, true) ; propagate(rest)
      case Nil       => ()
    }
    // enable lint and the group, honoring previous -Y settings
    def enableAll(): Unit = {
      xlint.value = true
      for (s <- lintWarnings) setPolitely(s, true)
    }
    // The command option
    MultiChoiceSetting("-Xlint", "warning", "Enable recommended additional warnings", choices = lintables, default = enableAll) withPostSetHook { x =>
      propagate(x.value)      // enabling the selections (on each append to value)
      xlint.value = true      // only enables lint, not the group
      for (b <- lintWarnings if isAnon(b) && !b.isSetByUser) b.value = true   // init anonymous settings (but not if disabled)
    }
    xlint
  }

  // Backward compatibility.
  @deprecated("Use fatalWarnings", "2.11.0") def Xwarnfatal            = fatalWarnings      // used by sbt
  @deprecated("This option is being removed", "2.11.0") def Xchecknull = warnSelectNullable // used by ide
  @deprecated("Use warnDeadCode", "2.11.0") def Ywarndeadcode          = warnDeadCode       // used by ide
}
