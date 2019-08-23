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

import scala.tools.nsc.Reporting.WarningCategory

/** Settings influencing the printing of warnings.
 */
trait Warnings {
  self: MutableSettings =>

  val Whelp         = BooleanSetting("-W", "Print a synopsis of warning options.")

  // Warning semantics.
  val fatalWarnings = BooleanSetting("-Werror", "Fail the compilation if there are any warnings.") withAbbreviation "-Xfatal-warnings"

  private val WconfDefault = List("cat=deprecation:ws", "cat=feature:ws", "cat=unchecked:ws", "cat=optimizer:ws")
  // Note: user-defined settings are added on the right, but the value is reversed before
  // it's parsed, so that later defined settings take precedence.
  val Wconf = MultiStringSetting(
    "-Wconf",
    "patterns",
    "Configure reporting of compiler warnings; use `help` for details.",
    helpText = Some(
      s"""Configure compiler warnings.
         |Syntax: -Wconf:<filter>&...&<filter>:<action>,<filter>:<action>,...
         |
         |<filter>
         |  - any message: any
         |  - message categories: cat=deprecation, cat=lint, cat=lint-infer-any
         |  - message content: msg=regex
         |  - site where the warning is triggered: site=my.package.*
         |  - source file name: src=src_managed/*
         |  - origin of deprecation: origin=external.package.*
         |  - since of deprecation: since<*1.24
         |
         |<action>
         |  - error / e
         |  - warning / w
         |  - warning-summary / ws
         |  - info / i
         |  - info-summary / is
         |  - silent / s
         |
         |The default configuration is:
         |  -Wconf:${WconfDefault.mkString(",")}
         |
         |User-defined configurations are added to the left. The leftmost rule matching
         |a warning message defines the action.
         |
         |Examples:
         |  - change every warning into an error: -Wconf any:error
         |  - ignore certain deprecations: -Wconf:cat=deprecation&origin=some.lib.*&since<*2.2:s
         |
         |Full list of message categories:
         |${WarningCategory.all.keys.groupBy(_.split('-').head).toList.sortBy(_._1).map(_._2.toList.sorted.mkString(", ")).mkString(" - ", "\n - ", "")}
         |
         |Note: on the command-line you might need to quote configurations containing `*` or `&`
         |to prevent the shell from expanding it to a list of files in the current directory.""".stripMargin))
  locally { Wconf.tryToSet(WconfDefault) }

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
  val warnOctalLiteral     = BooleanSetting("-Woctal-literal", "Warn on obsolete octal syntax.") withAbbreviation "-Ywarn-octal-literal"

  object UnusedWarnings extends MultiChoiceEnumeration {
    val Imports   = Choice("imports",   "Warn if an import selector is not referenced.")
    val PatVars   = Choice("patvars",   "Warn if a variable bound in a pattern is unused.")
    val Privates  = Choice("privates",  "Warn if a private member is unused.")
    val Locals    = Choice("locals",    "Warn if a local definition is unused.")
    val Explicits = Choice("explicits", "Warn if an explicit parameter is unused.")
    val Implicits = Choice("implicits", "Warn if an implicit parameter is unused.")
    val Params    = Choice("params",    "Enable -Wunused:explicits,implicits.", expandsTo = List(Explicits, Implicits))
    val Linted    = Choice("linted",    "-Xlint:unused.", expandsTo = List(Imports, Privates, Locals, Implicits))
  }

  // The -Ywarn-unused warning group.
  val warnUnused = MultiChoiceSetting(
    name    = "-Wunused",
    helpArg = "warning",
    descr   = "Enable or disable specific `unused` warnings",
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
    val Unused                 = LintWarning("unused",                    "Enable -Wunused:imports,privates,locals,implicits.")
    val NonlocalReturn         = LintWarning("nonlocal-return",           "A return statement used an exception for flow control.")
    val ImplicitNotFound       = LintWarning("implicit-not-found",        "Check @implicitNotFound and @implicitAmbiguous messages.")
    val Serial                 = LintWarning("serial",                    "@SerialVersionUID on traits and non-serializable classes.")
    val ValPattern             = LintWarning("valpattern",                "Enable pattern checks in val definitions.")
    val EtaZero                = LintWarning("eta-zero",                  "Warn on eta-expansion (rather than auto-application) of zero-ary method.")
    val EtaSam                 = LintWarning("eta-sam",                   "Warn on eta-expansion to meet a Java-defined functional interface that is not explicitly annotated with @FunctionalInterface.")
    val Deprecation            = LintWarning("deprecation",               "Enable linted deprecations.")

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
  def lintValPatterns            = lint contains ValPattern
  def warnEtaZero                = lint contains EtaZero
  def warnEtaSam                 = lint contains EtaSam
  def lintDeprecation            = lint contains Deprecation

  // The Xlint warning group.
  val lint = MultiChoiceSetting(
    name    = "-Xlint",
    helpArg = "warning",
    descr   = "Enable recommended warnings",
    domain  = LintWarnings,
    default = Some(List("_"))
  ).withPostSetHook { s =>
    if (s contains Unused) warnUnused.enable(UnusedWarnings.Linted)
    else warnUnused.disable(UnusedWarnings.Linted)
    if (s.contains(Deprecation)) deprecation.value = true
  }

  // Backward compatibility.
  @deprecated("Use fatalWarnings", "2.11.0") def Xwarnfatal            = fatalWarnings      // used by sbt

  private lazy val warnSelectNullable = BooleanSetting("-Xcheck-null", "This option is obsolete and does nothing.")
  @deprecated("This option is being removed", "2.11.0") def Xchecknull = warnSelectNullable // used by ide
  @deprecated("Use warnDeadCode", "2.11.0") def Ywarndeadcode          = warnDeadCode       // used by ide
}
