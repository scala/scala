/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package settings

import annotation.elidable
import scala.tools.util.PathResolver.Defaults
import scala.collection.mutable.HashSet

/** Settings influencing the printing of warnings.
 */
trait Warnings {
  self: MutableSettings =>

  // Warning semantics.
  val fatalWarnings = BooleanSetting("-Xfatal-warnings", "Fail the compilation if there are any warnings.")

  // These warnings are all so noisy as to be useless in their
  // present form, but have the potential to offer useful info.
  protected def allWarnings = lintWarnings ++ List(
    warnSelectNullable,
    warnValueDiscard,
    warnNumericWiden
  )
  // These warnings should be pretty quiet unless you're doing
  // something inadvisable.
  protected def lintWarnings = List(
    warnDeadCode,
    warnInaccessible,
    warnNullaryOverride,
    warnNullaryUnit
  )

  // Warning groups.
  val lint = (
    BooleanSetting("-Xlint", "Enable recommended additional warnings.")
    withPostSetHook (_ => lintWarnings foreach (_.value = true))
  )
  val warnEverything = (
    BooleanSetting("-Ywarn-all", "Enable all -Y warnings.")
    withPostSetHook (_ => lintWarnings foreach (_.value = true))
  )

  // Individual warnings.
  val warnSelectNullable   = BooleanSetting   ("-Xcheck-null", "Warn upon selection of nullable reference.")
  val warnDeadCode         = BooleanSetting   ("-Ywarn-dead-code", "Warn when dead code is identified.")
  val warnValueDiscard     = BooleanSetting   ("-Ywarn-value-discard", "Warn when non-Unit expression results are unused.")
  val warnNumericWiden     = BooleanSetting   ("-Ywarn-numeric-widen", "Warn when numerics are widened.")
  val warnNullaryUnit      = BooleanSetting   ("-Ywarn-nullary-unit", "Warn when nullary methods return Unit.")
  val warnInaccessible     = BooleanSetting   ("-Ywarn-inaccessible", "Warn about inaccessible types in method signatures.")
  val warnNullaryOverride  = BooleanSetting   ("-Ywarn-nullary-override",
    "Warn when non-nullary overrides nullary, e.g. `def foo()` over `def foo`.")
}
