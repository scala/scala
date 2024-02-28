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

import scala.annotation.nowarn
import scala.tools.nsc.Reporting.WarningCategory

/** Settings influencing the printing of warnings.
 */
trait Warnings {
  self: MutableSettings =>

  val Whelp         = BooleanSetting("-W", "Print a synopsis of warning options.")

  // Warning semantics.
  val fatalWarnings = BooleanSetting("-Werror", "Fail the compilation if there are any warnings.") withAbbreviation "-Xfatal-warnings"

  private val WconfDefault = List("cat=deprecation:ws", "cat=feature:ws", "cat=optimizer:ws")
  // Note: user-defined settings are added on the right, but the value is reversed before
  // it's parsed, so that later defined settings take precedence.
  val Wconf = MultiStringSetting(
    "-Wconf",
    "patterns",
    "Configure reporting of compiler warnings; use `help` for details.",
    default = WconfDefault,
    helpText = Some(
      s"""Configure compiler warnings.
         |Syntax: -Wconf:<filters>:<action>,<filters>:<action>,...
         |multiple <filters> are combined with &, i.e., <filter>&...&<filter>
         |
         |Note: Run with `-Wconf:any:warning-verbose` to print warnings with their category, site,
         |and (for deprecations) origin and since-version.
         |
         |<filter>
         |  - Any message: any
         |
         |  - Message categories: cat=deprecation, cat=lint, cat=lint-infer-any
         |    The full list of warning categories is shown at the end of this help text.
         |
         |  - Message content: msg=regex
         |    The regex need only match some part of the message, not all of it.
         |
         |  - Site where the warning is triggered: site=my\\.package\\..*
         |    The regex must match the full name (`package.Class.method`) of the warning position.
         |
         |  - Source file name: src=src_managed/.*
         |    If `-rootdir` is specified, the regex must match the canonical path relative to the
         |    root directory. Otherwise, the regex must match the canonical path relative to any
         |    path segment (`b/.*Test.scala` matches `/a/b/XTest.scala` but not `/ab/Test.scala`).
         |    Use unix-style paths, separated by `/`.
         |
         |  - Origin of deprecation: origin=external\\.package\\..*
         |    The regex must match the full name (`package.Class.method`) of the deprecated entity.
         |
         |  - Since of deprecation: since<1.24
         |    Valid operators: <, =, >, valid versions: N, N.M, N.M.P. Compares against the first
         |    version of the form N, N.M or N.M.P found in the `since` parameter of the deprecation,
         |    for example `1.2.3` in `@deprecated("", "some lib 1.2.3-foo")`.
         |
         |<action>
         |  - error / e
         |  - warning / w
         |  - warning-summary / ws (summary with the number of warnings, like for deprecations)
         |  - warning-verbose / wv (show applicable filters with each warning)
         |  - info / i             (infos are not counted as warnings and don't affect `-Werror`)
         |  - info-summary / is
         |  - info-verbose / iv
         |  - silent / s
         |
         |The default configuration is:
         |  -Wconf:${WconfDefault.mkString(",")}
         |
         |User-defined configurations are added to the left. The leftmost rule matching
         |a warning message defines the action.
         |
         |Examples:
         |  - change every warning into an error: -Wconf:any:error
         |  - silence certain deprecations: -Wconf:origin=some\\.lib\\..*&since>2.2:s
         |
         |Full list of message categories:
         |${WarningCategory.all.keys.groupBy(_.split('-').head).toList.sortBy(_._1).map(_._2.toList.sorted.mkString(", ")).mkString(" - ", "\n - ", "")}
         |
         |To suppress warnings locally, use the `scala.annotation.nowarn` annotation.
         |
         |Note: on the command-line you might need to quote configurations containing `*` or `&`
         |to prevent the shell from expanding patterns.""".stripMargin),
    prepend = true)

  // Non-lint warnings.
  val warnMacros           = ChoiceSetting(
    name    = "-Wmacros",
    helpArg = "mode",
    descr   = "Enable lint warnings on macro expansions.",
    choices = List("none", "before", "after", "both", "default"),
    default = "default",
    choicesHelp = List(
      "Do not inspect expansions or their original trees when generating unused symbol warnings.",
      "Only inspect unexpanded user-written code for unused symbols.",
      "Only inspect expanded trees when generating unused symbol warnings.",
      "Inspect both user-written code and expanded trees when generating unused symbol warnings.",
      "Only inspect unexpanded user-written code for unused symbols but include usages in expansions.",
    )
  ) withAbbreviation "-Ywarn-macros"
  val warnDeadCode         = BooleanSetting("-Wdead-code", "Warn when dead code is identified.") withAbbreviation "-Ywarn-dead-code"
  val warnNonUnitIf        = BooleanSetting("-Wnonunit-if", "Warn when if statements are non-Unit expressions, enabled by -Wnonunit-statement.")
  import scala.language.existentials
  val warnNonUnitStatement = BooleanSetting("-Wnonunit-statement", "Warn when block statements are non-Unit expressions.")
    .enablingIfNotSetByUser(warnNonUnitIf :: Nil)
  val warnValueDiscard     = BooleanSetting("-Wvalue-discard", "Warn when non-Unit expression results are unused.") withAbbreviation "-Ywarn-value-discard"
  val warnNumericWiden     = BooleanSetting("-Wnumeric-widen", "Warn when numerics are widened.") withAbbreviation "-Ywarn-numeric-widen"
  val warnOctalLiteral     = BooleanSetting("-Woctal-literal", "Warn on obsolete octal syntax.") withAbbreviation "-Ywarn-octal-literal"
  val warnNamedLiteral     = BooleanSetting("-Wunnamed-boolean-literal", "Warn about unnamed boolean literals if there is more than one or defaults are used, unless parameter has @deprecatedName.")
  val warnNamedBoolean     = BooleanSetting("-Wunnamed-boolean-literal-strict", "Warn about all unnamed boolean literals, unless parameter has @deprecatedName or the method has a single leading boolean parameter.").enabling(warnNamedLiteral :: Nil)

  object PerformanceWarnings extends MultiChoiceEnumeration {
    val Captured       = Choice("captured",        "Modification of var in closure causes boxing.")
    val NonlocalReturn = Choice("nonlocal-return", "A return statement used an exception for flow control.")
  }
  val warnPerformance = MultiChoiceSetting(
    name    = "-Wperformance",
    helpArg = "warning",
    descr   = "Enable or disable specific lints for performance",
    domain  = PerformanceWarnings,
    default = Some(List("_"))
  )
  def warnCaptured       = warnPerformance.contains(PerformanceWarnings.Captured)
  def warnNonlocalReturn = warnPerformance.contains(PerformanceWarnings.NonlocalReturn)

  object UnusedWarnings extends MultiChoiceEnumeration {
    val Imports   = Choice("imports",   "Warn if an import selector is not referenced.")
    val PatVars   = Choice("patvars",   "Warn if a variable bound in a pattern is unused.")
    val Privates  = Choice("privates",  "Warn if a private member is unused.")
    val Locals    = Choice("locals",    "Warn if a local definition is unused.")
    val Explicits = Choice("explicits", "Warn if an explicit parameter is unused.")
    val Implicits = Choice("implicits", "Warn if an implicit parameter is unused.")
    val Synthetics = Choice("synthetics", "Warn if a synthetic implicit parameter (context bound) is unused.")
    val Nowarn    = Choice("nowarn",    "Warn if a @nowarn annotation does not suppress any warnings.")
    val Params    = Choice("params",    "Enable -Wunused:explicits,implicits,synthetics.", expandsTo = List(Explicits, Implicits, Synthetics))
    val Linted    = Choice("linted",    "-Xlint:unused.", expandsTo = List(Imports, Privates, Locals, Implicits, Nowarn))
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
  def warnUnusedParams    = warnUnusedExplicits || warnUnusedImplicits || warnUnusedSynthetics
  def warnUnusedExplicits = warnUnused contains UnusedWarnings.Explicits
  def warnUnusedImplicits = warnUnused contains UnusedWarnings.Implicits
  def warnUnusedSynthetics = warnUnused contains UnusedWarnings.Synthetics
  def warnUnusedNowarn    = warnUnused contains UnusedWarnings.Nowarn

  val warnExtraImplicit   = BooleanSetting("-Wextra-implicit", "Warn when more than one implicit parameter section is defined.") withAbbreviation "-Ywarn-extra-implicit"

  @deprecated("Use lintImplicitRecursion", since="2.13.3")
  val warnSelfImplicit    = BooleanSetting("-Wself-implicit", "An implicit resolves to an enclosing definition.") withAbbreviation "-Ywarn-self-implicit" withDeprecationMessage "Use -Xlint:implicit-recursion"

  // Experimental lint warnings that are turned off, but which could be turned on programmatically.
  // They are not activated by -Xlint and can't be enabled on the command line because they are not
  // created using the standard factory methods.

  val warnValueOverrides = new BooleanSetting("value-overrides", "Generated value class method overrides an implementation.", default = false)

  // Lint warnings

  object LintWarnings extends MultiChoiceEnumeration {
    class LintWarning(name: String, help: String) extends Choice(name, help)
    def LintWarning(name: String, help: String) = new LintWarning(name, help)

    val AdaptedArgs            = LintWarning("adapted-args",              "An argument list was modified to match the receiver.")
    val NullaryUnit            = LintWarning("nullary-unit",              "`def f: Unit` looks like an accessor; add parens to look side-effecting.")
    val Inaccessible           = LintWarning("inaccessible",              "Warn about inaccessible types in method signatures.")
    val InferAny               = LintWarning("infer-any",                 "A type argument was inferred as Any.")
    val MissingInterpolator    = LintWarning("missing-interpolator",      "A string literal appears to be missing an interpolator id.")
    val DocDetached            = LintWarning("doc-detached",              "When running scaladoc, warn if a doc comment is discarded.")
    val PrivateShadow          = LintWarning("private-shadow",            "A private field (or class parameter) shadows a superclass field.")
    val TypeParameterShadow    = LintWarning("type-parameter-shadow",     "A local type parameter shadows a type already in scope.")
    val PolyImplicitOverload   = LintWarning("poly-implicit-overload",    "Parameterized overloaded implicit methods are not visible as view bounds.")
    val OptionImplicit         = LintWarning("option-implicit",           "Option.apply used an implicit view.")
    val DelayedInitSelect      = LintWarning("delayedinit-select",        "Selecting member of DelayedInit.")
    val PackageObjectClasses   = LintWarning("package-object-classes",    "Class or object defined in package object.")
    val StarsAlign             = LintWarning("stars-align",               "In a pattern, a sequence wildcard `_*` should match all of a repeated parameter.")
    val StrictUnsealedPatMat   = LintWarning("strict-unsealed-patmat",    "Pattern match on an unsealed class without a catch-all.")
    val Constant               = LintWarning("constant",                  "Evaluation of a constant arithmetic expression resulted in an error.")
    val Unused                 = LintWarning("unused",                    "Enable -Wunused:imports,privates,locals,implicits,nowarn.")
    val NonlocalReturn         = LintWarning("nonlocal-return",           "A return statement used an exception for flow control.")
    val ImplicitNotFound       = LintWarning("implicit-not-found",        "Check @implicitNotFound and @implicitAmbiguous messages.")
    val Serial                 = LintWarning("serial",                    "@SerialVersionUID on traits and non-serializable classes.")
    val ValPattern             = LintWarning("valpattern",                "Enable pattern checks in val definitions.")
    val EtaZero                = LintWarning("eta-zero",                  "Usage `f` of parameterless `def f()` resulted in eta-expansion, not empty application `f()`.")
    val EtaSam                 = LintWarning("eta-sam",                   "A method reference was eta-expanded but the expected SAM type was not annotated @FunctionalInterface.")
    val Deprecation            = LintWarning("deprecation",               "Enable -deprecation and also check @deprecated annotations.")
    val ByNameImplicit         = LintWarning("byname-implicit",           "Block adapted by implicit with by-name parameter.")
    val RecurseWithDefault     = LintWarning("recurse-with-default",      "Recursive call used default argument.")
    val UnitSpecialization     = LintWarning("unit-special",              "Warn for specialization of Unit in parameter position.")
    val MultiargInfix          = LintWarning("multiarg-infix",            "Infix operator was defined or used with multiarg operand.")
    val ImplicitRecursion      = LintWarning("implicit-recursion",        "Implicit resolves to an enclosing definition.")
    val UniversalMethods       = LintWarning("universal-methods",         "Require arg to is/asInstanceOf. No Unit receiver.")
    val NumericMethods         = LintWarning("numeric-methods",           "Dubious usages, such as `42.isNaN`.")
    val ArgDiscard             = LintWarning("arg-discard",               "-Wvalue-discard for adapted arguments.")
    val IntDivToFloat          = LintWarning("int-div-to-float",          "Warn when an integer division is converted (widened) to floating point: `(someInt / 2): Double`.")
    val PatternShadow          = LintWarning("pattern-shadow",            "Pattern variable id is also a term in scope.")
    val CloneableObject        = LintWarning("cloneable",                 "Modules (objects) should not be Cloneable.")

    def allLintWarnings = values.toSeq.asInstanceOf[Seq[LintWarning]]
  }
  import LintWarnings._

  def warnAdaptedArgs            = lint contains AdaptedArgs
  def warnNullaryUnit            = lint contains NullaryUnit
  def warnInaccessible           = lint contains Inaccessible
  def warnInferAny               = lint contains InferAny
  def warnMissingInterpolator    = lint contains MissingInterpolator
  def warnDocDetached            = lint contains DocDetached
  def warnPrivateShadow          = lint contains PrivateShadow
  def warnTypeParameterShadow    = lint contains TypeParameterShadow
  def warnPolyImplicitOverload   = lint contains PolyImplicitOverload
  def warnOptionImplicit         = lint contains OptionImplicit
  def warnDelayedInit            = lint contains DelayedInitSelect
  def warnPackageObjectClasses   = lint contains PackageObjectClasses
  def warnStrictUnsealedPatMat   = lint contains StrictUnsealedPatMat
  def warnStarsAlign             = lint contains StarsAlign
  def warnConstant               = lint contains Constant
  def lintUnused                 = lint contains Unused
  def lintImplicitNotFound       = lint contains ImplicitNotFound
  def warnSerialization          = lint contains Serial
  def lintValPatterns            = lint contains ValPattern
  def warnEtaZero                = lint contains EtaZero
  def warnEtaSam                 = lint contains EtaSam
  def lintDeprecation            = lint contains Deprecation
  def warnByNameImplicit         = lint contains ByNameImplicit
  def warnRecurseWithDefault     = lint contains RecurseWithDefault
  def unitSpecialization         = lint contains UnitSpecialization
  def multiargInfix              = lint contains MultiargInfix
  def lintImplicitRecursion      = lint.contains(ImplicitRecursion) || (warnSelfImplicit.value: @nowarn("cat=deprecation"))
  def lintUniversalMethods       = lint.contains(UniversalMethods)
  def lintNumericMethods         = lint.contains(NumericMethods)
  def lintArgDiscard             = lint.contains(ArgDiscard)
  def lintIntDivToFloat          = lint.contains(IntDivToFloat)
  def warnPatternShadow          = lint.contains(PatternShadow)
  def warnCloneableObject        = lint.contains(CloneableObject)

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
    if (s.contains(Deprecation) && deprecation.isDefault) deprecation.value = true
    if (s.contains(NonlocalReturn)) warnPerformance.enable(PerformanceWarnings.NonlocalReturn)
    else warnPerformance.disable(PerformanceWarnings.NonlocalReturn)
  }

  // Backward compatibility.
  @deprecated("Use fatalWarnings", "2.11.0") def Xwarnfatal            = fatalWarnings      // used by sbt

  private lazy val warnSelectNullable = BooleanSetting("-Xcheck-null", "This option is obsolete and does nothing.")
  @deprecated("This option is being removed", "2.11.0") def Xchecknull = warnSelectNullable // used by ide
  @deprecated("Use warnDeadCode", "2.11.0") def Ywarndeadcode          = warnDeadCode       // used by ide
}
