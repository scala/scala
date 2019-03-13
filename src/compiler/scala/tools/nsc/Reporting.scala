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

package scala
package tools
package nsc

import scala.collection.mutable
import scala.reflect.internal.{Reporting => IReporting, Symbols, SymbolTable}
import scala.reflect.internal.util.StringOps.countElementsAsString
import nsc.settings.{NoScalaVersion, ScalaVersion}

/** Provides delegates to the reporter doing the actual work.
 *  PerRunReporting implements per-Run stateful info tracking and reporting.
 */
//trait Reporting extends IReporting { self: ast.Positions with CompilationUnits with Symbols with SymbolTable =>
trait Reporting extends IReporting { self: Global =>
  import definitions._

  def settings: Settings

  // not deprecated yet, but a method called "error" imported into
  // nearly every trait really must go.  For now using globalError.
  def error(msg: String) = globalError(msg)

  object policy extends Enumeration {
    val Silent, Info, Warn, Error = Value
    // match a sym's full name against a prefix; if isInfractor, match the infractor.
    // For library "shapeless 2.1", the rule can supply the name, version, or both to match.
    case class Philter(pkg: String, escalation: Value, isInfractor: Boolean, label: String, version: ScalaVersion, matcher: ScalaVersion => Boolean) {
      def matches(sym: Symbol, since: String, infractor: Symbol): Boolean = {
        val (lib, v) = splitVersion(since)
        (pkg.isEmpty || {
          val checked = if (isInfractor) infractor else sym
          checked.fullName.startsWith(pkg)
        }) &&
        (label.isEmpty || label == lib) &&
        ((version eq NoScalaVersion) || matcher(v))
      }
    }
    val NoPhilter = Philter("", Silent, false, "", NoScalaVersion, _ => false)
    val config: List[Philter] = {
      // +sym since<1.0 or +'sym to mean warnings with use site context owner sym
      val regex = raw"([+-]{0,2})(?:\s*([\w.']*))?(?:\s*since([<>=])([\w. -]+))?".r
      // extractors aren't allowed to receive nulls, thereby crippling java regex
      object Maybes {
        def unapplySeq(s: String): Option[Seq[String]] =
          regex.unapplySeq(s).map(ss => ss.map { case null => "" case x => x })
      }
      def parse(s: String) =
        s match {
          case Maybes(esc, pkg, op, vers) =>
            val infr = pkg.startsWith("'")
            val name = if (infr) pkg.substring(1) else pkg
            val warn = esc match {
              case "+" => Error case "-" => Info case "--" => Silent case "" => Warn
              case _ => reporter.error(NoPosition, s"bad escalation '$esc'") ; Warn
            }
            val (label, version) = splitVersion(vers)
            Philter(name, warn, infr, label, version,
              op match {
                case "<" => x => version < x
                case ">" => x => version > x
                case "=" => x => version == x
                case ""  => _ => false
                case huh => reporter.error(NoPosition, s"bad operator '$huh'") ; _ => false
              })
          case _ =>
            reporter.error(NoPosition, s"bad policy '$s'")
            NoPhilter
        }
      settings.deprecationPolicy.value.map(parse)
    }
    private def splitVersion(since: String): (String, ScalaVersion) = {
      if (since.contains(' ')) {
        val i = since.lastIndexOf(' ')
        val (a, b) = since.splitAt(i)
        (a.trim, versionOf(b.trim))
      } else
        ("", versionOf(since))
    }
    private def versionOf(vstr: String) = ScalaVersion(vstr, x => reporter.error(NoPosition, s"bad version '$x'"))

    def apply(pos: Position, infractor: Symbol, sym: Symbol, msg: String, since: String, label: String): Value =
      config.find(_.matches(sym, since, infractor)).map(_.escalation).getOrElse(
        if (sym.hasAnnotation(DeprecatedErrorAttr)) Error else Warn
      )
  }

  // a new instance of this class is created for every Run (access the current instance via `currentRun.reporting`)
  protected def PerRunReporting = new PerRunReporting
  class PerRunReporting extends PerRunReportingBase {
    /** Collects for certain classes of warnings during this run. */
    private class ConditionalWarning(what: String, doReport: Boolean, setting: Settings#Setting) {
      def this(what: String, booleanSetting: Settings#BooleanSetting) {
        this(what, booleanSetting.value, booleanSetting)
      }
      val warnings = mutable.LinkedHashMap[Position, (String, String)]()
      def warn(pos: Position, msg: String, since: String = "") =
        if (doReport) reporter.warning(pos, msg)
        else if (!(warnings contains pos)) warnings += ((pos, (msg, since)))
      def summarize() =
        if (!warnings.isEmpty && (setting.isDefault || doReport)) {
          val sinceAndAmount = mutable.TreeMap[String, Int]()
          warnings.valuesIterator.foreach { case (_, since) =>
            val value = sinceAndAmount.get(since)
            if (value.isDefined) sinceAndAmount += ((since, value.get + 1))
            else sinceAndAmount += ((since, 1))
          }
          val deprecationSummary = sinceAndAmount.size > 1
          sinceAndAmount.foreach { case (since, numWarnings) =>
            val warningsSince = if (!since.isEmpty) s" (since $since)" else ""
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
    private val _inlinerWarnings        = new ConditionalWarning("inliner", !settings.optWarningsSummaryOnly, settings.optWarnings)
    private val _allConditionalWarnings = List(_deprecationWarnings, _uncheckedWarnings, _featureWarnings, _inlinerWarnings)

    def uncheckedWarning(pos: Position, msg: String): Unit   = _uncheckedWarnings.warn(pos, msg)
    def featureWarning(pos: Position, msg: String): Unit     = _featureWarnings.warn(pos, msg)
    def inlinerWarning(pos: Position, msg: String): Unit     = _inlinerWarnings.warn(pos, msg)

    def deprecationWarnings = _deprecationWarnings.warnings.toList
    def uncheckedWarnings   = _uncheckedWarnings.warnings.toList
    def featureWarnings     = _featureWarnings.warnings.toList
    def inlinerWarnings     = _inlinerWarnings.warnings.toList

    def allConditionalWarnings = _allConditionalWarnings flatMap (_.warnings)

    // useful for syntax deprecations? There is neither a subject symbol nor an infractor.
    def deprecationWarning(pos: Position, msg: String, since: String): Unit = deprecationWarning(pos, NoSymbol, NoSymbol, msg, since)

    // the given infractor (context owner) used the given symbol deprecatedly
    def deprecationWarning(pos: Position, infractor: Symbol, sym: Symbol): Unit = {
      val version = sym.deprecationVersion.getOrElse("")
      val since   = if (version.isEmpty) version else s" (since $version)"
      val message = sym.deprecationMessage match { case Some(msg) => s": $msg"        case _ => "" }
      val label   = sym.deprecationLabel.getOrElse("deprecated")
      deprecationWarning(pos, infractor, sym, s"$sym${sym.locationString} is deprecated$since$message", version, label)
    }
    // issue, escalate, or silence the given deprecation
    def deprecationWarning(pos: Position, infractor: Symbol, sym: Symbol, msg: String, since: String, label: String = "deprecated"): Unit =
      policy(pos, infractor, sym, msg, since, label) match {
        case policy.Silent =>
        case policy.Info   => reporter.echo(pos, s"$msg (since $since)")
        case policy.Warn   => _deprecationWarnings.warn(pos, msg, since)
        case policy.Error  => reporter.error(pos, s"$msg (since $since)")
      }

    private[this] var reportedFeature = Set[Symbol]()
    def featureWarning(pos: Position, featureName: String, featureDesc: String, featureTrait: Symbol, construct: => String = "", required: Boolean): Unit = {
      val req     = if (required) "needs to" else "should"
      val fqname  = "scala.language." + featureName
      val explain = (
        if (reportedFeature contains featureTrait) "" else
        s"""|
            |----
            |This can be achieved by adding the import clause 'import $fqname'
            |or by setting the compiler option -language:$featureName.
            |See the Scaladoc for value $fqname for a discussion
            |why the feature $req be explicitly enabled.""".stripMargin
      )
      reportedFeature += featureTrait

      val msg = s"$featureDesc $req be enabled\nby making the implicit value $fqname visible.$explain" replace ("#", construct)
      // don't error on postfix in pre-0.13.18 xsbt/Compat.scala
      def isSbtCompat =
        (featureName == "postfixOps" && pos.source.path.endsWith("/xsbt/Compat.scala") && Thread.currentThread.getStackTrace.exists(_.getClassName.startsWith("sbt.")))
      if (required && !isSbtCompat) {
        reporter.error(pos, msg)
      } else featureWarning(pos, msg)
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
