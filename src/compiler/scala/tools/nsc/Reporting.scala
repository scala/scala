/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL, Typesafe Inc.
 * @author  Adriaan Moors
 */

package scala
package tools
package nsc

import scala.collection.{ mutable, immutable }
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
    private class ConditionalWarning(what: String, option: Settings#BooleanSetting)(reRunFlag: String = option.name) {
      val warnings = mutable.LinkedHashMap[Position, String]()
      def warn(pos: Position, msg: String) =
        if (option) reporter.warning(pos, msg)
        else if (!(warnings contains pos)) warnings += ((pos, msg))
      def summarize() =
        if (warnings.nonEmpty && (option.isDefault || option)) {
          val numWarnings  = warnings.size
          val warningVerb  = if (numWarnings == 1) "was" else "were"
          val warningCount = countElementsAsString(numWarnings, s"$what warning")

          reporter.warning(NoPosition, s"there $warningVerb $warningCount; re-run with $reRunFlag for details")
        }
    }

    // This change broke sbt; I gave it the thrilling name of uncheckedWarnings0 so
    // as to recover uncheckedWarnings for its ever-fragile compiler interface.
    private val _deprecationWarnings    = new ConditionalWarning("deprecation", settings.deprecation)()
    private val _uncheckedWarnings      = new ConditionalWarning("unchecked", settings.unchecked)()
    private val _featureWarnings        = new ConditionalWarning("feature", settings.feature)()
    private val _inlinerWarnings        = new ConditionalWarning("inliner", settings.YinlinerWarnings)(if (settings.isBCodeActive) settings.YoptWarnings.name else settings.YinlinerWarnings.name)
    private val _allConditionalWarnings = List(_deprecationWarnings, _uncheckedWarnings, _featureWarnings, _inlinerWarnings)

    // TODO: remove in favor of the overload that takes a Symbol, give that argument a default (NoSymbol)
    def deprecationWarning(pos: Position, msg: String): Unit = _deprecationWarnings.warn(pos, msg)
    def uncheckedWarning(pos: Position, msg: String): Unit   = _uncheckedWarnings.warn(pos, msg)
    def featureWarning(pos: Position, msg: String): Unit     = _featureWarnings.warn(pos, msg)
    def inlinerWarning(pos: Position, msg: String): Unit     = _inlinerWarnings.warn(pos, msg)

    def deprecationWarnings = _deprecationWarnings.warnings.toList
    def uncheckedWarnings   = _uncheckedWarnings.warnings.toList
    def featureWarnings     = _featureWarnings.warnings.toList
    def inlinerWarnings     = _inlinerWarnings.warnings.toList

    def allConditionalWarnings = _allConditionalWarnings flatMap (_.warnings)

    // behold! the symbol that caused the deprecation warning (may not be deprecated itself)
    def deprecationWarning(pos: Position, sym: Symbol, msg: String): Unit = _deprecationWarnings.warn(pos, msg)
    def deprecationWarning(pos: Position, sym: Symbol): Unit = {
      val suffix = sym.deprecationMessage match { case Some(msg) => ": "+ msg case _ => "" }
      deprecationWarning(pos, sym, s"$sym${sym.locationString} is deprecated$suffix")
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
