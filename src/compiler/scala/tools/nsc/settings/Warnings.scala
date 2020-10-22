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

  // Warning semantics.
  val fatalWarnings = BooleanSetting("-Xfatal-warnings", "Fail the compilation if there are any warnings.")

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
         |  - warning-verbose / wv (show warning category and site)
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

  // Non-lint warnings. -- TODO turn into MultiChoiceEnumeration
  val warnMacros           = ChoiceSetting(
    name    = "-Ywarn-macros",
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
  )
  val warnDeadCode         = BooleanSetting("-Ywarn-dead-code", "Warn when dead code is identified.")
  val warnValueDiscard     = BooleanSetting("-Ywarn-value-discard", "Warn when non-Unit expression results are unused.")
  val warnNumericWiden     = BooleanSetting("-Ywarn-numeric-widen", "Warn when numerics are widened.")

  object UnusedWarnings extends MultiChoiceEnumeration {
    val Imports   = Choice("imports",   "Warn if an import selector is not referenced.")
    val PatVars   = Choice("patvars",   "Warn if a variable bound in a pattern is unused.")
    val Privates  = Choice("privates",  "Warn if a private member is unused.")
    val Locals    = Choice("locals",    "Warn if a local definition is unused.")
    val Explicits = Choice("explicits", "Warn if an explicit parameter is unused.")
    val Implicits = Choice("implicits", "Warn if an implicit parameter is unused.")
    val Nowarn    = Choice("nowarn",    "Warn if a @nowarn annotation does not suppress any warnings.")
    val Params    = Choice("params",    "Enable -Ywarn-unused:explicits,implicits.", expandsTo = List(Explicits, Implicits))
    val Linted    = Choice("linted",    "-Xlint:unused.", expandsTo = List(Imports, Privates, Locals, Implicits, Nowarn))
  }

  // The -Ywarn-unused warning group.
  val warnUnused = MultiChoiceSetting(
    name    = "-Ywarn-unused",
    helpArg = "warning",
    descr   = "Enable or disable specific `unused' warnings",
    domain  = UnusedWarnings,
    default = Some(List("_"))
  )

  def warnUnusedImport    = warnUnused contains UnusedWarnings.Imports
  def warnUnusedPatVars   = warnUnused contains UnusedWarnings.PatVars
  def warnUnusedPrivates  = warnUnused contains UnusedWarnings.Privates
  def warnUnusedLocals    = warnUnused contains UnusedWarnings.Locals
  def warnUnusedParams    = warnUnusedExplicits || warnUnusedImplicits
  def warnUnusedExplicits = warnUnused contains UnusedWarnings.Explicits
  def warnUnusedImplicits = warnUnused contains UnusedWarnings.Implicits
  def warnUnusedNowarn    = warnUnused contains UnusedWarnings.Nowarn

  BooleanSetting("-Ywarn-unused-import", "Warn when imports are unused.") withPostSetHook { s =>
    warnUnused.add(s"${if (s) "" else "-"}imports")
  } //withDeprecationMessage s"Enable -Ywarn-unused:imports"

  val warnExtraImplicit   = BooleanSetting("-Ywarn-extra-implicit", "Warn when more than one implicit parameter section is defined.")

  val warnSelfImplicit    = BooleanSetting("-Ywarn-self-implicit", "Warn when an implicit resolves to an enclosing self-definition.")

  // Experimental lint warnings that are turned off, but which could be turned on programmatically.
  // They are not activated by -Xlint and can't be enabled on the command line because they are not
  // created using the standard factory methods.

  val warnValueOverrides = new BooleanSetting("value-overrides", "Generated value class method overrides an implementation.", default = false)

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
    val Unused                 = LintWarning("unused",                    "Enable -Ywarn-unused:imports,privates,locals,implicits,nowarn.")
    val Deprecation            = LintWarning("deprecation",               "Enable -deprecation and also check @deprecated annotations.")

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
  def lintUnused                 = lint contains Unused
  def lintDeprecation            = lint contains Deprecation

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
    default = Some(List("_"))
  ).withPostSetHook { s =>
    if (s contains Unused) warnUnused.enable(UnusedWarnings.Linted)
    else warnUnused.disable(UnusedWarnings.Linted)
    if (s.contains(Deprecation) && deprecation.isDefault) deprecation.value = true
  }

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
