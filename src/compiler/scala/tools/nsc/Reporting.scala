/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL, Typesafe Inc.
 * @author  Adriaan Moors
 */

package scala
package tools
package nsc

import scala.collection.mutable
import scala.reflect.internal.util.StringOps.countElementsAsString

/** Provides delegates to the reporter doing the actual work.
 * PerRunReporting implements per-Run stateful info tracking and reporting
 *
 * TODO: make reporting configurable
 */
trait Reporting extends scala.reflect.internal.Reporting { self: ast.Positions with CompilationUnits with scala.reflect.internal.Symbols =>
  def settings: Settings

  // not deprecated yet, but a method called "error" imported into
  // nearly every trait really must go.  For now using globalError.
  def error(msg: String) = globalError(msg)

  // a new instance of this class is created for every Run (access the current instance via `currentRun.reporting`)
  protected def PerRunReporting = new PerRunReporting
  class PerRunReporting extends PerRunReportingBase {
    /** Collects for certain classes of warnings during this run. */
    private class ConditionalWarning(what: String, doReport: () => Boolean, setting: Settings#Setting) {
      def this(what: String, booleanSetting: Settings#BooleanSetting) {
        this(what, () => booleanSetting, booleanSetting)
      }
      val warnings = mutable.LinkedHashMap[Position, (String, String)]()
      def warn(pos: Position, msg: String, since: String = "") =
        if (doReport()) reporter.warning(pos, msg)
        else if (!(warnings contains pos)) warnings += ((pos, (msg, since)))
      def summarize() =
        if (warnings.nonEmpty && (setting.isDefault || doReport())) {
          val sinceAndAmount = mutable.TreeMap[String, Int]()
          warnings.valuesIterator.foreach { case (_, since) =>
            val value = sinceAndAmount.get(since)
            if (value.isDefined) sinceAndAmount += ((since, value.get + 1))
            else sinceAndAmount += ((since, 1))
          }
          val deprecationSummary = sinceAndAmount.size > 1
          sinceAndAmount.foreach { case (since, numWarnings) =>
            val warningsSince = if (since.nonEmpty) s" (since $since)" else ""
            val warningVerb   = if (numWarnings == 1) "was" else "were"
            val warningCount  = countElementsAsString(numWarnings, s"$what warning")
            val rerun         = if (deprecationSummary) "" else reporter.rerunWithDetails(setting, setting.name)
            reporter.warning(NoPosition, s"there ${warningVerb} ${warningCount}${warningsSince}${rerun}")
          }
          if (deprecationSummary) {
            val numWarnings   = warnings.size
            val warningVerb   = if (numWarnings == 1) "was" else "were"
            val warningCount  = countElementsAsString(numWarnings, s"$what warning")
            val rerun         = reporter.rerunWithDetails(setting, setting.name)
            reporter.warning(NoPosition, s"there ${warningVerb} ${warningCount} in total${rerun}")
          }
        }
    }

    // This change broke sbt; I gave it the thrilling name of uncheckedWarnings0 so
    // as to recover uncheckedWarnings for its ever-fragile compiler interface.
    private val _deprecationWarnings    = new ConditionalWarning("deprecation", settings.deprecation)
    private val _uncheckedWarnings      = new ConditionalWarning("unchecked", settings.unchecked)
    private val _featureWarnings        = new ConditionalWarning("feature", settings.feature)
    private val _inlinerWarnings        = new ConditionalWarning("inliner", () => !settings.optWarningsSummaryOnly, settings.optWarnings)
    private val _allConditionalWarnings = List(_deprecationWarnings, _uncheckedWarnings, _featureWarnings, _inlinerWarnings)

    // TODO: remove in favor of the overload that takes a Symbol, give that argument a default (NoSymbol)
    def deprecationWarning(pos: Position, msg: String, since: String): Unit = _deprecationWarnings.warn(pos, msg, since)
    def uncheckedWarning(pos: Position, msg: String): Unit   = _uncheckedWarnings.warn(pos, msg)
    def featureWarning(pos: Position, msg: String): Unit     = _featureWarnings.warn(pos, msg)
    def inlinerWarning(pos: Position, msg: String): Unit     = _inlinerWarnings.warn(pos, msg)

    def deprecationWarnings = _deprecationWarnings.warnings.toList
    def uncheckedWarnings   = _uncheckedWarnings.warnings.toList
    def featureWarnings     = _featureWarnings.warnings.toList
    def inlinerWarnings     = _inlinerWarnings.warnings.toList

    def allConditionalWarnings = _allConditionalWarnings flatMap (_.warnings)

    // behold! the symbol that caused the deprecation warning (may not be deprecated itself)
    def deprecationWarning(pos: Position, sym: Symbol, msg: String, since: String): Unit = _deprecationWarnings.warn(pos, msg, since)
    def deprecationWarning(pos: Position, sym: Symbol): Unit = {
      val version = sym.deprecationVersion.getOrElse("")
      val since   = if (version.isEmpty) version else s" (since $version)"
      val message = sym.deprecationMessage match { case Some(msg) => s": $msg"        case _ => "" }
      deprecationWarning(pos, sym, s"$sym${sym.locationString} is deprecated$since$message", version)
    }

    private[this] var reportedFeature = Set[Symbol]()
    def featureWarning(pos: Position, featureName: String, featureDesc: String, featureTrait: Symbol, construct: => String = "", required: Boolean): Unit = {
      val req     = if (required) "needs to" else "should"
      val fqname  = "scala.language." + featureName
      val explain = (
        if (reportedFeature contains featureTrait) "" else
        s"""|
            |This can be achieved by adding the import clause 'import $fqname'
            |or by setting the compiler option -language:$featureName.
            |See the Scaladoc for value $fqname for a discussion
            |why the feature $req be explicitly enabled.""".stripMargin
      )
      reportedFeature += featureTrait

      val msg = s"$featureDesc $req be enabled\nby making the implicit value $fqname visible.$explain" replace ("#", construct)
      if (required) reporter.error(pos, msg)
      else featureWarning(pos, msg)
    }

    /** Has any macro expansion used a fallback during this run? */
    var seenMacroExpansionsFallingBack = false

    def summarizeErrors(): Unit = if (!reporter.hasErrors) {
      _allConditionalWarnings foreach (_.summarize())

      if (seenMacroExpansionsFallingBack)
        reporter.warning(NoPosition, "some macros could not be expanded and code fell back to overridden methods;"+
                "\nrecompiling with generated classfiles on the classpath might help.")

      // todo: migrationWarnings

      if (settings.fatalWarnings && reporter.hasWarnings)
        reporter.error(NoPosition, "No warnings can be incurred under -Xfatal-warnings.")
    }
  }
}
