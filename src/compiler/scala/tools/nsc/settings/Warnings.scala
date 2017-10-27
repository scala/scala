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
    val Params    = Choice("params",    "Enable -Ywarn-unused:explicits,implicits.", expandsTo = List(Explicits, Implicits))
    val Linted    = Choice("linted",    "-Xlint:unused.", expandsTo = List(Imports, Privates, Locals, Implicits))
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

  BooleanSetting("-Ywarn-unused-import", "Warn when imports are unused.") withPostSetHook { s =>
    warnUnused.add(s"${if (s) "" else "-"}imports")
  } //withDeprecationMessage s"Enable -Ywarn-unused:imports"

  val warnExtraImplicit   = BooleanSetting("-Ywarn-extra-implicit", "Warn when more than one implicit parameter section is defined.")

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
    val Unused                 = LintWarning("unused",                    "Enable -Ywarn-unused:imports,privates,locals,implicits.")

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
