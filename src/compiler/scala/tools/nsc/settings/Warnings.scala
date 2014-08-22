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

  // Non-lint warnings

  val warnDeadCode         = BooleanSetting("-Ywarn-dead-code", "Warn when dead code is identified.")
  val warnValueDiscard     = BooleanSetting("-Ywarn-value-discard", "Warn when non-Unit expression results are unused.")
  val warnNumericWiden     = BooleanSetting("-Ywarn-numeric-widen", "Warn when numerics are widened.")
  // SI-7712, SI-7707 warnUnused not quite ready for prime-time
  val warnUnused           = BooleanSetting("-Ywarn-unused", "Warn when local and private vals, vars, defs, and types are are unused.")
  // currently considered too noisy for general use
  val warnUnusedImport     = BooleanSetting("-Ywarn-unused-import", "Warn when imports are unused.")

  // Experimental lint warnings that are turned off, but which could be turned on programmatically.
  // These warnings are said to blind those who dare enable them.
  // They are not activated by -Xlint and can't be enabled on the command line.
  val warnValueOverrides = { // currently turned off as experimental. creaded using constructor (new BS), so not available on the command line.
  val flag = new BooleanSetting("value-overrides", "Generated value class method overrides an implementation")
    flag.value = false
    flag
  }

  // Lint warnings

  def warnAdaptedArgs            = lint contains "adapted-args"
  def warnNullaryUnit            = lint contains "nullary-unit"
  def warnInaccessible           = lint contains "inaccessible"
  def warnNullaryOverride        = lint contains "nullary-override"
  def warnInferAny               = lint contains "infer-any"
  def warnMissingInterpolator    = lint contains "missing-interpolator"
  def warnDocDetached            = lint contains "doc-detached"
  def warnPrivateShadow          = lint contains "private-shadow"
  def warnTypeParameterShadow    = lint contains "type-parameter-shadow"
  def warnPolyImplicitOverload   = lint contains "poly-implicit-overload"
  def warnOptionImplicit         = lint contains "option-implicit"
  def warnDelayedInit            = lint contains "delayedinit-select"
  def warnByNameRightAssociative = lint contains "by-name-right-associative"
  def warnPackageObjectClasses   = lint contains "package-object-classes"
  def warnUnsoundMatch           = lint contains "unsound-match"

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
  val lint: MultiChoiceSetting = {
    val description = "Enable or disable specific warnings"

    val choices = List(
      ("adapted-args",              "Warn if an argument list is modified to match the receiver.",               true),
      ("nullary-unit",              "Warn when nullary methods return Unit.",                                    true),
      ("inaccessible",              "Warn about inaccessible types in method signatures.",                       true),
      ("nullary-override",          "Warn when non-nullary `def f()' overrides nullary `def f'.",                true),
      ("infer-any",                 "Warn when a type argument is inferred to be `Any`.",                        true),
      ("missing-interpolator",      "A string literal appears to be missing an interpolator id.",                false),
      ("doc-detached",              "A ScalaDoc comment appears to be detached from its element.",               false),
      ("private-shadow",            "A private field (or class parameter) shadows a superclass field.",          false),
      ("type-parameter-shadow",     "A local type parameter shadows a type already in scope.",                   false),
      ("poly-implicit-overload",    "Parameterized overloaded implicit methods are not visible as view bounds.", false),
      ("option-implicit",           "Option.apply used implicit view.",                                          false),
      ("delayedinit-select",        "Selecting member of DelayedInit",                                           false),
      ("by-name-right-associative", "By-name parameter of right associative operator.",                          false),
      ("package-object-classes",    "Class or object defined in package object.",                                false),
      ("unsound-match",             "Pattern match may not be typesafe.",                                        false)
    ).sorted

    for (c <- choices.filter(_._3)) {
      BooleanSetting("-Ywarn-"+ c._1, c._2) withPostSetHook { s =>
        if (s) lint.add(c._1)
        else lint.add("-" + c._1)
      } // withDeprecationMessage s"Enable -Xlint:${c._1}"
    }

    MultiChoiceSetting(
      name         = "-Xlint",
      helpArg      = "warning",
      descr        = description,
      choices      = choices map (_._1),
      descriptions = choices map (_._2),
      default      = Some(List("_"))
    )
  }

  private lazy val warnSelectNullable = BooleanSetting("-Xcheck-null", "This option is obsolete and does nothing.")

  // Backward compatibility.
  @deprecated("Use fatalWarnings", "2.11.0") def Xwarnfatal            = fatalWarnings      // used by sbt
  @deprecated("This option is being removed", "2.11.0") def Xchecknull = warnSelectNullable // used by ide
  @deprecated("Use warnDeadCode", "2.11.0") def Ywarndeadcode          = warnDeadCode       // used by ide
}
