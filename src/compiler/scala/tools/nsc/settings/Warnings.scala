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

  // Warning semantics.
  val fatalWarnings = BooleanSetting("-Xfatal-warnings", "Fail the compilation if there are any warnings.")

  /**
   * -W "Warning" settings
   */
  val warningConfigs  = MultiStringSetting(name="-Wconfig", arg="configs", descr="Configure the severity of using @restricted API.", helpText=Some(
    """Configure the severity of @restricted API usages, using warning config expressions.
      |A warning config expression may be <label>, <label>:<severity>, or <label>:<prefix>:<severity>
      |
      |<label> is a String passed into @restricted.
      |<severity> must be one of "error", "warning", "info", or "none": default: warning.
      |<prefix> denotes the prefix of the restricted API. For example, "foo.*" will denote
      |that this config would apply only to calls under foo package/object.
      |Note: "*" is simply ignored.
      |
      |The following configuration would escalate apiMayChange restriction to an error:
      |    -Wconfig apiMayChange:foo.*:error
      |
    """.stripMargin
  ))
  def restrictionSeverity(label: String, symFullName: String, defaultSeverity: String): String =
    if (wconfigs.isEmpty) defaultSeverity
    else
      wconfigs.find(_.isHit(label, symFullName)) match {
        case Some(x) => x.severity
        case _       => defaultSeverity
      }
  private[scala] final class WarningConfig(val label: String, val prefix0: String, val severity: String) {
    val prefix = prefix0.replaceAllLiterally("*", "")
    def isHit(lbl: String, symFullName: String): Boolean =
      (lbl == this.label) && (prefix.isEmpty || symFullName.startsWith(prefix))
    severity match {
      case "error" | "warning" | "info" | "none" => ()
      case _ => throw new IllegalArgumentException(s"invalid severity '$severity' in warning config")
    }
  }
  private[scala] lazy val wconfigs: List[WarningConfig] = {
    def parse(str: String): WarningConfig = {
      str.split(":").toList match {
        case List(l)       => new WarningConfig(l, "", "warning")
        case List(l, s)    => new WarningConfig(l, "", s)
        case List(l, p, s) => new WarningConfig(l, p, s)
        case _             => throw new IllegalArgumentException(s"invalid warning config: $str")
      }
    }
    warningConfigs.value map { str =>
      parse(str)
    }
  }

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
  val warnOctalLiteral     = BooleanSetting("-Ywarn-octal-literal", "Warn on obsolete octal syntax.")

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
    descr   = "Enable or disable specific `unused` warnings",
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

  val warnExtraImplicit   = BooleanSetting("-Ywarn-extra-implicit", "Warn when more than one implicit parameter section is defined.")

  val warnSelfImplicit    = BooleanSetting("-Ywarn-self-implicit", "Warn when an implicit resolves to an enclosing self-definition.")

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
    val ValPattern             = LintWarning("valpattern",                "Enable pattern checks in val definitions.")
    val EtaZero                = LintWarning("eta-zero",                  "Warn on eta-expansion (rather than auto-application) of zero-ary method.")
    val EtaSam                 = LintWarning("eta-sam",                   "Warn on eta-expansion to meet a Java-defined functional interface that is not explicitly annotated with @FunctionalInterface.")

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
