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

  // These additional warnings are all so noisy as to be useless in their
  // present form, but have the potential to offer useful info.
  protected def allWarnings = lintWarnings ++ List(
    warnDeadCode,
    warnValueDiscard,
    warnNumericWiden,
    warnUnused,           // SI-7712, SI-7707 warnUnused not quite ready for prime-time
    warnUnusedImport,     // currently considered too noisy for general use
    warnValueOverrides    // currently turned off as experimental
  )
  // These warnings should be pretty quiet unless you're doing
  // something inadvisable.
  protected def lintWarnings = List(
    warnInaccessible,
    warnNullaryOverride,
    warnNullaryUnit,
    warnAdaptedArgs,
    warnInferAny,
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

  // Individual warnings. They can be set with -Ywarn.
  private def nonlintflag(name: String, text: String): BooleanSetting = BooleanSetting(name, text)

  val warnDeadCode         = nonlintflag("-Ywarn-dead-code",
                             "Warn when dead code is identified.")
  val warnValueDiscard     = nonlintflag("-Ywarn-value-discard",
                             "Warn when non-Unit expression results are unused.")
  val warnNumericWiden     = nonlintflag("-Ywarn-numeric-widen",
                             "Warn when numerics are widened.")
  val warnUnused           = nonlintflag("-Ywarn-unused",
                             "Warn when local and private vals, vars, defs, and types are are unused")
  val warnUnusedImport     = nonlintflag("-Ywarn-unused-import",
                             "Warn when imports are unused")

  // Lint warnings that have no -Y avatar, created with new instead of the autoregistering factory method.
  // They evaluate true if set to true or else are unset but -Xlint is true
  private def lintflag(name: String, text: String): BooleanSetting =
    new BooleanSetting(name, text) {
      override def value = if (isSetByUser) super.value else xlint
    }

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

  // Experimental lint warnings that are turned off, but which could be turned on programmatically.
  // These warnings are said to blind those who dare enable them.
  // They are not activated by -Xlint and can't be enabled on the command line.
  val warnValueOverrides = {
    val flag = lintflag("value-overrides", "Generated value class method overrides an implementation")
    flag.value = false
    flag
  }

  // The Xlint warning group.
  private val xlint = new BooleanSetting("-Zunused", "True if -Xlint or -Xlint:_")
  // On -Xlint or -Xlint:_, set xlint, otherwise set the lint warning unless already set true
  val lint = {
    val description = "Enable or disable specific warnings"
    val choices     = (lintWarnings map (_.name)).sorted
    MultiChoiceSetting(
      name    = "-Xlint",
      helpArg = "warning",
      descr   = description,
      choices = choices,
      default = Some(() => xlint.value = true)
    ) { s =>
      def helpline(n: String) = lintWarnings.find(_.name == n).map(w => f"  ${w.name}%-25s ${w.helpDescription}%n")
      choices flatMap (helpline(_)) mkString (f"$description:%n", "", f"%n")
    } withPostSetHook { x =>
      val Neg = "-"
      def setPolitely(b: BooleanSetting, v: Boolean) = if (!b.isSetByUser || !b) b.value = v
      def set(w: String, v: Boolean) = lintWarnings find (_.name == w) foreach (setPolitely(_, v))
      def propagate(ss: List[String]): Unit = ss match {
        case w :: rest => if (w startsWith Neg) set(w stripPrefix Neg, false) else set(w, true) ; propagate(rest)
        case Nil       => ()
      }
      propagate(x.value)
    }
  }

  // Lint warnings that are currently -Y, but deprecated in that usage
  @deprecated("Use warnAdaptedArgs", since="2.11.2")
  val YwarnAdaptedArgs      = BooleanSetting("-Ywarn-adapted-args",
                             "Warn if an argument list is modified to match the receiver.") enabling List(warnAdaptedArgs)
                             //withDeprecationMessage "Enable -Xlint:adapted-args"
  @deprecated("Use warnNullaryUnit", since="2.11.2")
  val YwarnNullaryUnit      = BooleanSetting("-Ywarn-nullary-unit",
                             "Warn when nullary methods return Unit.") enabling List(warnNullaryUnit)
                             //withDeprecationMessage "Enable -Xlint:nullary-unit"
  @deprecated("Use warnInaccessible", since="2.11.2")
  val YwarnInaccessible     = BooleanSetting("-Ywarn-inaccessible",
                             "Warn about inaccessible types in method signatures.") enabling List(warnInaccessible)
                             //withDeprecationMessage "Enable -Xlint:inaccessible"
  @deprecated("Use warnNullaryOverride", since="2.11.2")
  val YwarnNullaryOverride  = BooleanSetting("-Ywarn-nullary-override",
                             "Warn when non-nullary `def f()' overrides nullary `def f'.") enabling List(warnNullaryOverride)
                             //withDeprecationMessage "Enable -Xlint:nullary-override"
  @deprecated("Use warnInferAny", since="2.11.2")
  val YwarnInferAny         = BooleanSetting("-Ywarn-infer-any",
                             "Warn when a type argument is inferred to be `Any`.") enabling List(warnInferAny)
                             //withDeprecationMessage "Enable -Xlint:infer-any"

  private lazy val warnSelectNullable = BooleanSetting("-Xcheck-null", "This option is obsolete and does nothing.")

  // Backward compatibility.
  @deprecated("Use fatalWarnings", "2.11.0") def Xwarnfatal            = fatalWarnings      // used by sbt
  @deprecated("This option is being removed", "2.11.0") def Xchecknull = warnSelectNullable // used by ide
  @deprecated("Use warnDeadCode", "2.11.0") def Ywarndeadcode          = warnDeadCode       // used by ide
}
