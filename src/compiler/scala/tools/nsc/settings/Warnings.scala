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
    // warnValueOverrides
    warnMissingInterpolator,
    warnDocDetached,
    warnPrivateShadow,
    warnPolyImplicitOverload,
    warnOptionImplicit,
    warnDelayedInit,
    warnByNameRightAssociative,
    warnPackageObjectClasses,
    warnUnsoundMatch
  )

  private lazy val warnSelectNullable = BooleanSetting("-Xcheck-null", "This option is obsolete and does nothing.")

  // Individual warnings.
  val warnDeadCode         = BooleanSetting("-Ywarn-dead-code",
                             "Warn when dead code is identified.")
  val warnValueDiscard     = BooleanSetting("-Ywarn-value-discard",
                             "Warn when non-Unit expression results are unused.")
  val warnNumericWiden     = BooleanSetting("-Ywarn-numeric-widen",
                             "Warn when numerics are widened.")
  val warnUnused           = BooleanSetting("-Ywarn-unused",
                             "Warn when local and private vals, vars, defs, and types are are unused")
  val warnUnusedImport     = BooleanSetting("-Ywarn-unused-import",
                             "Warn when imports are unused")

  // Lint warnings that are not -Y, created with new instead of autoregistering factory method
  private def lintflag(name: String, text: String) = new BooleanSetting(name, text)

  val warnAdaptedArgs          = lintflag("adapted-args",
                                 "Warn if an argument list is modified to match the receiver.")
  val warnNullaryUnit          = lintflag("nullary-unit",
                                 "Warn when nullary methods return Unit.")
  val warnInaccessible         = lintflag("inaccessible",
                                "Warn about inaccessible types in method signatures.")
  val warnNullaryOverride      = lintflag("nullary-override",
                                "Warn when non-nullary `def f()' overrides nullary `def f'.")
  val warnInferAny             = lintflag("infer-any",
                                "Warn when a type argument is inferred to be `Any`.")
  val warnMissingInterpolator  = lintflag("missing-interpolator",
                                "A string literal appears to be missing an interpolator id.")
  val warnDocDetached          = lintflag("doc-detached",
                                "A ScalaDoc comment appears to be detached from its element.")
  val warnPrivateShadow        = lintflag("private-shadow",
                                "A private field (or class parameter) shadows a superclass field.")
  val warnPolyImplicitOverload = lintflag("poly-implicit-overload",
                                "Parameterized overloaded implicit methods are not visible as view bounds")
  val warnOptionImplicit       = lintflag("option-implicit",
                                "Option.apply used implicit view.")
  val warnDelayedInit          = lintflag("delayedinit-select",
                                "Selecting member of DelayedInit")
  val warnByNameRightAssociative = lintflag("by-name-right-associative",
                                "By-name parameter of right associative operator")
  val warnPackageObjectClasses = lintflag("package-object-classes",
                                "Class or object defined in package object")
  val warnUnsoundMatch         = lintflag("unsound-match",
                                "Pattern match may not be typesafe")

  // Lint warnings that are not enabled yet
  val warnValueOverrides = lintflag("value-overrides", "Generated value class method overrides an implementation")

  // Warning groups.
  val lint                 = {
    // Boolean setting for testing if lint is on; not "added" to option processing
    val xlint = lintflag("-Xlint", "Enable recommended additional warnings.")
    val yprefix = "-Ywarn-"
    def lintables = (lintWarnings map (_.name stripPrefix yprefix)).sorted
    def isAnon(b: BooleanSetting) = !(b.name startsWith "-")
    def setPolitely(b: BooleanSetting, v: Boolean) = if (!b.isSetByUser) b.value = v
    def set(w: String, v: Boolean) = lintWarnings find (s => (s.name stripPrefix yprefix) == w) foreach (b => setPolitely(b, v))
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

  // Lint warnings that are currently -Y, but deprecated in that usage
  // Alas, the -Yarg must have a doppelgaenger that is not deprecated
  @deprecated("Use the Xlint flag", since="2.11.2")
  val YwarnAdaptedArgs      = BooleanSetting("-Ywarn-adapted-args",
                             "Warn if an argument list is modified to match the receiver.") withDeprecationMessage
                             "Enable -Xlint:adapted-args" enabling List(warnAdaptedArgs)
  @deprecated("Use the Xlint flag", since="2.11.2")
  val YwarnNullaryUnit      = BooleanSetting("-Ywarn-nullary-unit",
                             "Warn when nullary methods return Unit.") withDeprecationMessage
                             "Enable -Xlint:nullary-unit" enabling List(warnNullaryUnit)
  @deprecated("Use the Xlint flag", since="2.11.2")
  val YwarnInaccessible     = BooleanSetting("-Ywarn-inaccessible",
                             "Warn about inaccessible types in method signatures.") withDeprecationMessage
                             "Enable -Xlint:inaccessible" enabling List(warnInaccessible)
  @deprecated("Use the Xlint flag", since="2.11.2")
  val YwarnNullaryOverride  = BooleanSetting("-Ywarn-nullary-override",
                             "Warn when non-nullary `def f()' overrides nullary `def f'.") withDeprecationMessage
                             "Enable -Xlint:nullary-override" enabling List(warnNullaryOverride)
  @deprecated("Use the Xlint flag", since="2.11.2")
  val YwarnInferAny         = BooleanSetting("-Ywarn-infer-any",
                             "Warn when a type argument is inferred to be `Any`.") withDeprecationMessage
                             "Enable -Xlint:infer-any" enabling List(warnInferAny)

  // Backward compatibility.
  @deprecated("Use fatalWarnings", "2.11.0") def Xwarnfatal            = fatalWarnings      // used by sbt
  @deprecated("This option is being removed", "2.11.0") def Xchecknull = warnSelectNullable // used by ide
  @deprecated("Use warnDeadCode", "2.11.0") def Ywarndeadcode          = warnDeadCode       // used by ide
}
