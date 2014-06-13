/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL, Typesafe Inc.
 * @author  Adriaan Moors
 */

package scala
package tools
package nsc

import reporters.{ Reporter, ConsoleReporter }
import scala.collection.{ mutable, immutable }

/** Provides delegates to the reporter doing the actual work.
 * PerRunReporting implements per-Run stateful info tracking and reporting
 *
 * TODO: make reporting configurable
 */
trait Reporting extends scala.reflect.internal.Reporting { self: ast.Positions with CompilationUnits with scala.reflect.api.Symbols =>
  def settings: Settings

  // == currentRun.reporting
  def currentReporting: PerRunReporting

  def supplementTyperState(errorMessage: String): String

  // not deprecated yet, but a method called "error" imported into
  // nearly every trait really must go.  For now using globalError.
  def error(msg: String) = globalError(msg)

  override def deprecationWarning(pos: Position, msg: String) = currentReporting.deprecationWarning(pos, msg)
  override def supplementErrorMessage(errorMessage: String)   = currentReporting.supplementErrorMessage(errorMessage)

  // a new instance of this class is created for every Run (access the current instance via `currentReporting`)
  class PerRunReporting {
    // NOTE: scala.reflect.macros.Parsers#parse relies on everything related to reporting going through this def...
    // TODO: can we rework this to avoid the indirection/fragility?
    def reporter = Reporting.this.reporter

    /** Collects for certain classes of warnings during this run. */
    private class ConditionalWarning(what: String, option: Settings#BooleanSetting) {
      val warnings = mutable.LinkedHashMap[Position, String]()
      def warn(pos: Position, msg: String) =
        if (option) reporter.warning(pos, msg)
        else if (!(warnings contains pos)) warnings += ((pos, msg))
      def summarize() =
        if (warnings.nonEmpty && (option.isDefault || settings.fatalWarnings)) {
          val numWarnings  = warnings.size
          val warningEvent = // TODO use scala.reflect.internal.util.StringOps.countElementsAsString(numWarnings, s"$what warning")
            if (numWarnings > 1) s"were $numWarnings $what warnings"
            else s"was one $what warning"

          reporter.warning(NoPosition, s"there $warningEvent; re-run with ${option.name} for details")
        }
    }

    // This change broke sbt; I gave it the thrilling name of uncheckedWarnings0 so
    // as to recover uncheckedWarnings for its ever-fragile compiler interface.
    private val _deprecationWarnings    = new ConditionalWarning("deprecation", settings.deprecation)
    private val _uncheckedWarnings      = new ConditionalWarning("unchecked", settings.unchecked)
    private val _featureWarnings        = new ConditionalWarning("feature", settings.feature)
    private val _inlinerWarnings        = new ConditionalWarning("inliner", settings.YinlinerWarnings)
    private val _allConditionalWarnings = List(_deprecationWarnings, _uncheckedWarnings, _featureWarnings, _inlinerWarnings)

    def deprecationWarning(pos: Position, msg: String): Unit = _deprecationWarnings.warn(pos, msg)
    def uncheckedWarning(pos: Position, msg: String): Unit   = _uncheckedWarnings.warn(pos, msg)
    def featureWarning(pos: Position, msg: String): Unit     = _featureWarnings.warn(pos, msg)
    def inlinerWarning(pos: Position, msg: String): Unit     = _inlinerWarnings.warn(pos, msg)

    def deprecationWarnings = _deprecationWarnings.warnings.toList
    def uncheckedWarnings   = _uncheckedWarnings.warnings.toList
    def featureWarnings     = _featureWarnings.warnings.toList
    def inlinerWarnings     = _inlinerWarnings.warnings.toList

    def allConditionalWarnings = _allConditionalWarnings flatMap (_.warnings)

    private[this] var reportedFeature = Set[Symbol]()
    def featureWarning(pos: Position, featureName: String, featureDesc: String, featureTrait: Symbol, construct: => String = "", required: Boolean): Unit = {
      val req     = if (required) "needs to" else "should"
      val fqname  = "scala.language." + featureName
      val explain = (
        if (reportedFeature contains featureTrait) "" else
        s"""|
            |This can be achieved by adding the import clause 'import $fqname'
            |or by setting the compiler option -language:$featureName.
            |See the Scala docs for value $fqname for a discussion
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

    // for repl
    private[this] var incompleteHandler: (Position, String) => Unit = null
    def withIncompleteHandler[T](handler: (Position, String) => Unit)(thunk: => T) = {
      val saved = incompleteHandler
      incompleteHandler = handler
      try thunk
      finally incompleteHandler = saved
    }

    def incompleteHandled = incompleteHandler != null
    def incompleteInputError(pos: Position, msg: String): Unit =
      if (incompleteHandled) incompleteHandler(pos, msg)
      else reporter.error(pos, msg)

    /** Have we already supplemented the error message of a compiler crash? */
    private[this] var supplementedError = false
    def supplementErrorMessage(errorMessage: String): String =
      if (supplementedError) errorMessage
      else {
        supplementedError = true
        supplementTyperState(errorMessage)
      }
  }
}