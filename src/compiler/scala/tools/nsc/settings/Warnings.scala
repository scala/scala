/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package nsc
package settings

/** Settings influencing the printing of warnings.
 */
trait Warnings {
  self: MutableSettings =>

  val Whelp         = BooleanSetting("-W", "Print a synopsis of warning options.")

  // Warning semantics.
  val fatalWarnings = BooleanSetting("-Werror", "Fail the compilation if there are any warnings.") withAbbreviation "-Xfatal-warnings"

  // Non-lint warnings. -- TODO turn into MultiChoiceEnumeration
  val warnMacros           = ChoiceSetting(
    name    = "-Wmacros",
    helpArg = "mode",
    descr   = "Enable lint warnings on macro expansions.",
    choices = List("none", "before", "after", "both"),
    default = "before",
    choicesHelp = List(
      "Do not inspect expansions or their original trees when generating unused symbol warnings.",
      "Only inspect unexpanded user-written code for unused symbols.",
      "Only inspect expanded trees when generating unused symbol warnings.",
      "Inspect both user-written code and expanded trees when generating unused symbol warnings."
    )
  ) withAbbreviation "-Ywarn-macros"
  val warnDeadCode         = BooleanSetting("-Wdead-code", "Warn when dead code is identified.") withAbbreviation "-Ywarn-dead-code"
  val warnValueDiscard     = BooleanSetting("-Wvalue-discard", "Warn when non-Unit expression results are unused.") withAbbreviation "-Ywarn-value-discard"
  val warnNumericWiden     = BooleanSetting("-Wnumeric-widen", "Warn when numerics are widened.") withAbbreviation "-Ywarn-numeric-widen"

  object UnusedWarnings extends MultiChoiceEnumeration {
    val Imports   = Choice("imports",   "Warn if an import selector is not referenced.")
    val PatVars   = Choice("patvars",   "Warn if a variable bound in a pattern is unused.")
    val Privates  = Choice("privates",  "Warn if a private member is unused.")
    val Locals    = Choice("locals",    "Warn if a local definition is unused.")
    val Explicits = Choice("explicits", "Warn if an explicit parameter is unused.")
    val Implicits = Choice("implicits", "Warn if an implicit parameter is unused.")
    val Params    = Choice("params",    "Enable -Ywarn-unused:explicits,implicits.", expandsTo = List(Explicits, Implicits))
    val Linted    = Choice("linted",    "-Xlint:unused.", expandsTo = List(Imports, Privates, Locals, Implicits))
  }

  // The -Ywarn-unused warning group.
  val warnUnused = MultiChoiceSetting(
    name    = "-Wunused",
    helpArg = "warning",
    descr   = "Enable or disable specific `unused' warnings",
    domain  = UnusedWarnings,
    default = Some(List("_"))
  ) withAbbreviation "-Ywarn-unused"

  def warnUnusedImport    = warnUnused contains UnusedWarnings.Imports
  def warnUnusedPatVars   = warnUnused contains UnusedWarnings.PatVars
  def warnUnusedPrivates  = warnUnused contains UnusedWarnings.Privates
  def warnUnusedLocals    = warnUnused contains UnusedWarnings.Locals
  def warnUnusedParams    = warnUnusedExplicits || warnUnusedImplicits
  def warnUnusedExplicits = warnUnused contains UnusedWarnings.Explicits
  def warnUnusedImplicits = warnUnused contains UnusedWarnings.Implicits

  val warnExtraImplicit   = BooleanSetting("-Wextra-implicit", "Warn when more than one implicit parameter section is defined.") withAbbreviation "-Ywarn-extra-implicit"

  val warnSelfImplicit    = BooleanSetting("-Wself-implicit", "Warn when an implicit resolves to an enclosing self-definition.") withAbbreviation "-Ywarn-self-implicit"

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
    class LintWarning(name: String, help: String) extends Choice(name, help)
    def LintWarning(name: String, help: String) = new LintWarning(name, help)

    val AdaptedArgs            = LintWarning("adapted-args",              "Warn if an argument list is modified to match the receiver.")
    val NullaryUnit            = LintWarning("nullary-unit",              "Warn when nullary methods return Unit.")
    val Inaccessible           = LintWarning("inaccessible",              "Warn about inaccessible types in method signatures.")
    val NullaryOverride        = LintWarning("nullary-override",          "Warn when non-nullary `def f()' overrides nullary `def f'.")
    val InferAny               = LintWarning("infer-any",                 "Warn when a type argument is inferred to be `Any`.")
    val MissingInterpolator    = LintWarning("missing-interpolator",      "A string literal appears to be missing an interpolator id.")
    val DocDetached            = LintWarning("doc-detached",              "A Scaladoc comment appears to be detached from its element.")
    val PrivateShadow          = LintWarning("private-shadow",            "A private field (or class parameter) shadows a superclass field.")
    val TypeParameterShadow    = LintWarning("type-parameter-shadow",     "A local type parameter shadows a type already in scope.")
    val PolyImplicitOverload   = LintWarning("poly-implicit-overload",    "Parameterized overloaded implicit methods are not visible as view bounds.")
    val OptionImplicit         = LintWarning("option-implicit",           "Option.apply used implicit view.")
    val DelayedInitSelect      = LintWarning("delayedinit-select",        "Selecting member of DelayedInit.")
    val PackageObjectClasses   = LintWarning("package-object-classes",    "Class or object defined in package object.")
    val StarsAlign             = LintWarning("stars-align",               "Pattern sequence wildcard must align with sequence component.")
    val Constant               = LintWarning("constant",                  "Evaluation of a constant arithmetic expression results in an error.")
    val Unused                 = LintWarning("unused",                    "Enable -Ywarn-unused:imports,privates,locals,implicits.")
    val NonlocalReturn         = LintWarning("nonlocal-return",           "A return statement used an exception for flow control.")
    val ImplicitNotFound       = LintWarning("implicit-not-found",        "Check @implicitNotFound and @implicitAmbiguous messages.")
    val Serial                 = LintWarning("serial",                    "@SerialVersionUID on traits and non-serializable classes.")

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
  def warnPackageObjectClasses   = lint contains PackageObjectClasses
  def warnStarsAlign             = lint contains StarsAlign
  def warnConstant               = lint contains Constant
  def lintUnused                 = lint contains Unused
  def warnNonlocalReturn         = lint contains NonlocalReturn
  def lintImplicitNotFound       = lint contains ImplicitNotFound
  def warnSerialization          = lint contains Serial

  // The Xlint warning group.
  val lint = MultiChoiceSetting(
    name    = "-Xlint",
    helpArg = "warning",
    descr   = "Enable or disable specific warnings",
    domain  = LintWarnings,
    default = Some(List("_"))
  ).withPostSetHook { s =>
    if (s contains Unused) warnUnused.enable(UnusedWarnings.Linted)
    else warnUnused.disable(UnusedWarnings.Linted)
  }

  // Backward compatibility.
  @deprecated("Use fatalWarnings", "2.11.0") def Xwarnfatal            = fatalWarnings      // used by sbt

  private lazy val warnSelectNullable = BooleanSetting("-Xcheck-null", "This option is obsolete and does nothing.")
  @deprecated("This option is being removed", "2.11.0") def Xchecknull = warnSelectNullable // used by ide
  @deprecated("Use warnDeadCode", "2.11.0") def Ywarndeadcode          = warnDeadCode       // used by ide
}
