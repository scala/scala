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

  // Non-lint warnings

  val warnDeadCode         = BooleanSetting("-Ywarn-dead-code", "Warn when dead code is identified.")
  val warnValueDiscard     = BooleanSetting("-Ywarn-value-discard", "Warn when non-Unit expression results are unused.")
  val warnNumericWiden     = BooleanSetting("-Ywarn-numeric-widen", "Warn when numerics are widened.")
  // SI-7712, SI-7707 warnUnused not quite ready for prime-time
  val warnUnused           = BooleanSetting("-Ywarn-unused", "Warn when local and private vals, vars, defs, and types are unused.")
  // currently considered too noisy for general use
  val warnUnusedImport     = BooleanSetting("-Ywarn-unused-import", "Warn when imports are unused.")

  // Experimental lint warnings that are turned off, but which could be turned on programmatically.
  // They are not activated by -Xlint and can't be enabled on the command line because they are not
  // created using the standard factory methods.

  val warnValueOverrides = {
    val flag = new BooleanSetting("value-overrides", "Generated value class method overrides an implementation.")
    flag.value = false
    flag
  }

  // Lint warnings

  object LintWarnings extends MultiChoiceEnumeration {
    class LintWarning(name: String, help: String, val yAliased: Boolean) extends Choice(name, help)
    def LintWarning(name: String, help: String, yAliased: Boolean = false) = new LintWarning(name, help, yAliased)

    val AdaptedArgs            = LintWarning("adapted-args",              "Warn if an argument list is modified to match the receiver.",               true)
    val NullaryUnit            = LintWarning("nullary-unit",              "Warn when nullary methods return Unit.",                                    true)
    val Inaccessible           = LintWarning("inaccessible",              "Warn about inaccessible types in method signatures.",                       true)
    val NullaryOverride        = LintWarning("nullary-override",          "Warn when non-nullary `def f()' overrides nullary `def f'.",                true)
    val InferAny               = LintWarning("infer-any",                 "Warn when a type argument is inferred to be `Any`.",                        true)
    val MissingInterpolator    = LintWarning("missing-interpolator",      "A string literal appears to be missing an interpolator id.")
    val DocDetached            = LintWarning("doc-detached",              "A Scaladoc comment appears to be detached from its element.")
    val PrivateShadow          = LintWarning("private-shadow",            "A private field (or class parameter) shadows a superclass field.")
    val TypeParameterShadow    = LintWarning("type-parameter-shadow",     "A local type parameter shadows a type already in scope.")
    val PolyImplicitOverload   = LintWarning("poly-implicit-overload",    "Parameterized overloaded implicit methods are not visible as view bounds.")
    val OptionImplicit         = LintWarning("option-implicit",           "Option.apply used implicit view.")
    val DelayedInitSelect      = LintWarning("delayedinit-select",        "Selecting member of DelayedInit.")
    val ByNameRightAssociative = LintWarning("by-name-right-associative", "By-name parameter of right associative operator.")
    val PackageObjectClasses   = LintWarning("package-object-classes",    "Class or object defined in package object.")
    val UnsoundMatch           = LintWarning("unsound-match",             "Pattern match may not be typesafe.")
    val StarsAlign             = LintWarning("stars-align",               "Pattern sequence wildcard must align with sequence component.")
    val Constant               = LintWarning("constant",                  "Evaluation of a constant arithmetic expression results in an error.")

    def allLintWarnings = values.toSeq.asInstanceOf[Seq[LintWarning]]
  }
  import LintWarnings._

  def warnAdaptedArgs            = lint contains AdaptedArgs
  def warnNullaryUnit            = lint contains NullaryUnit
  def warnInaccessible           = lint contains Inaccessible
  def warnNullaryOverride        = lint contains NullaryOverride
  def warnInferAny               = lint contains InferAny
  def warnMissingInterpolator    = lint contains MissingInterpolator
  def warnDocDetached            = lint contains DocDetached
  def warnPrivateShadow          = lint contains PrivateShadow
  def warnTypeParameterShadow    = lint contains TypeParameterShadow
  def warnPolyImplicitOverload   = lint contains PolyImplicitOverload
  def warnOptionImplicit         = lint contains OptionImplicit
  def warnDelayedInit            = lint contains DelayedInitSelect
  def warnByNameRightAssociative = lint contains ByNameRightAssociative
  def warnPackageObjectClasses   = lint contains PackageObjectClasses
  def warnUnsoundMatch           = lint contains UnsoundMatch
  def warnStarsAlign             = lint contains StarsAlign
  def warnConstant               = lint contains Constant

  // Lint warnings that are currently -Y, but deprecated in that usage
  @deprecated("Use warnAdaptedArgs", since="2.11.2")
  def YwarnAdaptedArgs = warnAdaptedArgs
  @deprecated("Use warnNullaryUnit", since="2.11.2")
  def YwarnNullaryUnit = warnNullaryUnit
  @deprecated("Use warnInaccessible", since="2.11.2")
  def YwarnInaccessible = warnInaccessible
  @deprecated("Use warnNullaryOverride", since="2.11.2")
  def YwarnNullaryOverride = warnNullaryOverride
  @deprecated("Use warnInferAny", since="2.11.2")
  def YwarnInferAny = warnInferAny

  // The Xlint warning group.
  val lint = MultiChoiceSetting(
    name    = "-Xlint",
    helpArg = "warning",
    descr   = "Enable or disable specific warnings",
    domain  = LintWarnings,
    default = Some(List("_")))

  allLintWarnings foreach {
    case w if w.yAliased =>
      BooleanSetting(s"-Ywarn-${w.name}", {w.help}) withPostSetHook { s =>
        lint.add(if (s) w.name else s"-${w.name}")
      } // withDeprecationMessage s"Enable -Xlint:${c._1}"
    case _ =>
  }

  private lazy val warnSelectNullable = BooleanSetting("-Xcheck-null", "This option is obsolete and does nothing.")

  // Backward compatibility.
  @deprecated("Use fatalWarnings", "2.11.0") def Xwarnfatal            = fatalWarnings      // used by sbt
  @deprecated("This option is being removed", "2.11.0") def Xchecknull = warnSelectNullable // used by ide
  @deprecated("Use warnDeadCode", "2.11.0") def Ywarndeadcode          = warnDeadCode       // used by ide
}
