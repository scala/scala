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

import java.util.Locale.ENGLISH
import java.util.regex.PatternSyntaxException

import scala.collection.mutable
import scala.reflect.internal
import scala.reflect.internal.util.StringOps.countElementsAsString
import scala.reflect.internal.util.{Position, SourceFile}
import scala.tools.nsc.Reporting.Version.{NonParseableVersion, ParseableVersion}
import scala.tools.nsc.Reporting._
import scala.util.matching.Regex

/** Provides delegates to the reporter doing the actual work.
 * PerRunReporting implements per-Run stateful info tracking and reporting
 */
trait Reporting extends internal.Reporting { self: ast.Positions with CompilationUnits with internal.Symbols =>
  def settings: Settings

  @deprecated("use `globalError` instead")
  def error(msg: String): Unit = globalError(msg)

  // a new instance of this class is created for every Run (access the current instance via `runReporting`)
  protected def PerRunReporting = new PerRunReporting
  class PerRunReporting extends PerRunReportingBase {
    val rootDirPrefix: String =
      if (settings.rootdir.value.isEmpty) ""
      else Regex.quote(new java.io.File(settings.rootdir.value).getCanonicalPath.replace("\\", "/"))
    lazy val wconf = WConf.parse(settings.Wconf.value, rootDirPrefix) match {
      case Left(msgs) =>
        val multiHelp =
          if (settings.Wconf.value.exists(_.contains(",")))
            """
              |Note: for multiple filters, use `-Wconf:filter1:action1,filter2:action2` (recommended)
              |      or alternatively          `-Wconf filter1:action1 filter2:action2`""".stripMargin
          else ""
        globalError(s"Failed to parse `-Wconf` configuration: ${settings.Wconf.value}\n${msgs.mkString("\n")}$multiHelp")
        WConf(Nil)
      case Right(c) => c
    }

    private val summarizedWarnings: mutable.Map[WarningCategory, mutable.LinkedHashMap[Position, Message]] = mutable.HashMap.empty
    private val summarizedInfos: mutable.Map[WarningCategory, mutable.LinkedHashMap[Position, Message]] = mutable.HashMap.empty

    private var suppressionsComplete = false
    private val suppressions: mutable.LinkedHashMap[SourceFile, mutable.ListBuffer[Suppression]] = mutable.LinkedHashMap.empty
    private val suspendedMessages: mutable.LinkedHashSet[Message] = mutable.LinkedHashSet.empty

    private def isSuppressed(warning: Message): Boolean =
      suppressions.getOrElse(warning.pos.source, Nil).find(_.matches(warning)) match {
        case Some(s) => s.markUsed(); true
        case _ => false
      }

    def addSuppression(sup: Suppression): Unit = {
      val source = sup.annotPos.source
      suppressions.getOrElseUpdate(source, mutable.ListBuffer.empty) += sup
    }

    def suppressionExists(pos: Position): Boolean =
      suppressions.getOrElse(pos.source, Nil).exists(_.annotPos.point == pos.point)

    def warnUnusedSuppressions(): Unit = {
      // if we stop before typer completes (errors in parser, Ystop), report all suspended messages
      suspendedMessages.foreach(issueWarning)
      if (settings.warnUnusedNowarn) {
        val sources = suppressions.keysIterator.toList
        for (source <- sources; sups <- suppressions.remove(source); sup <- sups.reverse) {
          if (!sup.used)
            issueWarning(Message.Plain(sup.annotPos, "@nowarn annotation does not suppress any warnings", WarningCategory.UnusedNowarn))
        }
      }
    }

    def reportSuspendedMessages(): Unit = {
      suppressionsComplete = true
      // sort suppressions. they are not added in any particular order because of lazy type completion
      suppressions.mapValuesInPlace((_, sups) => sups.sortBy(sup => 0 - sup.start))
      suspendedMessages.foreach { m =>
        if (!isSuppressed(m))
          issueWarning(m)
      }
      suspendedMessages.clear()
    }

    private def summaryMap(action: Action, category: WarningCategory) = {
      val sm = (action: @unchecked) match {
        case Action.WarningSummary => summarizedWarnings
        case Action.InfoSummary => summarizedInfos
      }
      sm.getOrElseUpdate(category, mutable.LinkedHashMap.empty)
    }

    private def issueWarning(warning: Message): Unit = issueWarning(warning, wconf.action(warning))

    private def issueWarning(warning: Message, action: Action): Unit = {
      def verboseMessage(category: WarningCategory, msg: String, site: String, origin: String, version: Version): String =
        s"[${category.name} @ $site" +
        (if (origin != "") s" | origin=$origin" else "") +
        (if (version.filterString != "") s" | version=${version.filterString}" else "") +
        s"] $msg"
      def verbose = warning match {
        case Message.Deprecation(_, msg, site, origin, version)     => verboseMessage(warning.category, msg, site, origin, version)
        case Message.Plain(_, msg, category, site, origin, version) => verboseMessage(category, msg, site, origin, version)
      }
      action match {
        case Action.Error          => reporter.error(warning.pos, warning.msg)
        case Action.Warning        => reporter.warning(warning.pos, warning.msg)
        case Action.WarningVerbose => reporter.warning(warning.pos, verbose)
        case Action.Info           => reporter.echo(warning.pos, warning.msg)
        case Action.InfoVerbose    => reporter.echo(warning.pos, verbose)
        case a @ (Action.WarningSummary | Action.InfoSummary) =>
          val m = summaryMap(a, warning.category.summaryCategory)
          if (!m.contains(warning.pos)) m.addOne((warning.pos, warning))
        case Action.Silent =>
      }
    }

    private def checkSuppressedAndIssue(warning: Message): Unit = checkSuppressedAndIssue(warning, wconf.action(warning))

    private def checkSuppressedAndIssue(warning: Message, action: Action): Unit = {
      if (suppressionsComplete) {
        if (!isSuppressed(warning))
          issueWarning(warning, action)
      } else
        suspendedMessages += warning
    }

    private def summarize(action: Action, category: WarningCategory): Unit = {
      def rerunMsg: String = {
        val s: Settings#Setting = category match {
          case WarningCategory.Deprecation => settings.deprecation
          case WarningCategory.Feature => settings.feature
          case WarningCategory.Optimizer => settings.optWarnings
          case WarningCategory.Unchecked => settings.unchecked
          case _ => null
        }
        if (s != null) reporter.rerunWithDetails(s, s.name)
        else s"; change -Wconf for cat=${category.name} to display individual messages"
      }

      val m = summaryMap(action, category)
      if (m.nonEmpty) {
        val sinceAndAmount = mutable.TreeMap[String, Int]()
        m.valuesIterator.foreach { msg =>
          val since = msg match {
            case d: Message.Deprecation => d.since.orig
            case _ => ""
          }
          val value = sinceAndAmount.get(since)
          if (value.isDefined) sinceAndAmount += ((since, value.get + 1))
          else sinceAndAmount += ((since, 1))
        }
        val deprecationSummary = sinceAndAmount.size > 1
        val isWarn = action == Action.WarningSummary
        val messageKind =
          if (isWarn) { if (category == WarningCategory.Deprecation) "" else " warning" }
          else " info message"
        sinceAndAmount.foreach { case (since, numWarnings) =>
          val warningsSince = if (since.nonEmpty) s" (since $since)" else ""
          val warningCount  = countElementsAsString(numWarnings, s"${category.name}$messageKind")
          val rerun         = if (deprecationSummary) "" else rerunMsg
          val msg           = s"$warningCount$warningsSince$rerun"
          if (isWarn) reporter.warning(NoPosition, msg)
          else reporter.echo(NoPosition, msg)
        }
        if (deprecationSummary) {
          val numWarnings   = m.size
          val warningCount  = countElementsAsString(numWarnings, s"${category.name}$messageKind")
          val rerun         = rerunMsg
          val msg           = s"$warningCount in total$rerun"
          if (isWarn) reporter.warning(NoPosition, msg)
          else reporter.echo(NoPosition, msg)
        }
      }
    }

    private def siteName(sym: Symbol) = if (sym.exists) {
      // Similar to fullNameString, but don't jump to enclosing class. Keep full chain of symbols.
      def impl(s: Symbol): String =
        if (s.isRootSymbol || s == NoSymbol) s.nameString
        else if (s.owner.isEffectiveRoot) s.nameString
        else impl(s.effectiveOwner) + "." + s.nameString
      impl(sym)
    } else ""

    def deprecationWarning(pos: Position, msg: String, since: String, site: String, origin: String): Unit =
      checkSuppressedAndIssue(Message.Deprecation(pos, msg, site, origin, Version.fromString(since)))

    def deprecationWarning(pos: Position, origin: Symbol, site: Symbol, msg: String, since: String): Unit =
      deprecationWarning(pos, msg, since, siteName(site), siteName(origin))

    def deprecationWarning(pos: Position, origin: Symbol, site: Symbol): Unit = {
      val version = origin.deprecationVersion.getOrElse("")
      val since   = if (version.isEmpty) version else s" (since $version)"
      val message = origin.deprecationMessage.map(": " + _).getOrElse("")
      deprecationWarning(pos, origin, site, s"$origin${origin.locationString} is deprecated$since$message", version)
    }

    private[this] var reportedFeature = Set[Symbol]()
    // we don't have access to runDefinitions here, so mapping from strings instead of feature symbols
    private val featureCategory: Map[String, WarningCategory.Feature] = {
      import WarningCategory._
      Map(
        ("dynamics", FeatureDynamics),
        ("existentials", FeatureExistentials),
        ("higherKinds", FeatureHigherKinds),
        ("implicitConversions", FeatureImplicitConversions),
        ("postfixOps", FeaturePostfixOps),
        ("reflectiveCalls", FeatureReflectiveCalls),
        ("macros", FeatureMacros)
      ).withDefaultValue(Feature)
    }
    def featureWarning(pos: Position, featureName: String, featureDesc: String, featureTrait: Symbol, construct: => String = "", required: Boolean, site: Symbol): Unit = {
      val req     = if (required) "needs to" else "should"
      val fqname  = "scala.language." + featureName
      val explain = (
        if (reportedFeature contains featureTrait) "" else
          s"""
             |----
             |This can be achieved by adding the import clause 'import $fqname'
             |or by setting the compiler option -language:$featureName.
             |See the Scaladoc for value $fqname for a discussion
             |why the feature $req be explicitly enabled.""".stripMargin
        )
      reportedFeature += featureTrait

      val msg = s"$featureDesc $req be enabled\nby making the implicit value $fqname visible.$explain" replace ("#", construct)
      // maybe pos.source.file.file.getParentFile.getName or Path(source.file.file).parent.name
      def parentFileName(source: internal.util.SourceFile) =
        Option(java.nio.file.Paths.get(source.path).getParent).map(_.getFileName.toString)
      // don't error on postfix in pre-0.13.18 xsbt/Compat.scala
      def isSbtCompat = (featureName == "postfixOps"
        && pos.source.file.name == "Compat.scala"
        && parentFileName(pos.source).getOrElse("") == "xsbt"
        && Thread.currentThread.getStackTrace.exists(_.getClassName.startsWith("sbt."))
      )
      if (required && !isSbtCompat) reporter.error(pos, msg)
      else warning(pos, msg, featureCategory(featureTrait.nameString), site)
    }

    // Used in the optimizer where we don't have no symbols, the site string is created from the class internal name and method name.
    def warning(pos: Position, msg: String, category: WarningCategory, site: String): Unit =
      checkSuppressedAndIssue(Message.Plain(pos, msg, category, site, "", Version.fromString("")))

    // Preferred over the overload above whenever a site symbol is available
    def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol): Unit =
      warning(pos, msg, category, siteName(site))

    // someone is using @apiStatus API
    def handleApiStatus(pos: Position, sym: Symbol, site: Symbol): Unit = {
      val category0 = sym.apiStatusCategory.getOrElse("unspecified")
      val category = WarningCategory.parse(category0)
      val message = sym.apiStatusMessage.getOrElse("")
      val origin = siteName(sym)
      val since = Version.fromString(sym.apiStatusVersion.getOrElse(""))
      val msg = category match {
        case WarningCategory.Deprecation =>
          Message.Deprecation(pos, message, siteName(site), origin, since)
        case _ =>
          val sinceX = sym.apiStatusVersion match {
            case Some(v) => s" ($v)"
            case _       => ""
          }
          Message.Plain(pos, s"$message$sinceX", category, siteName(site), origin, since)
      }
      val defaultAction0 = sym.apiStatusDefaultAction.getOrElse("warning")
      val defaultAction = Action.parse(defaultAction0) match {
        case Right(a) => a
        case Left(_)  => Action.Warning
      }
      checkSuppressedAndIssue(msg, wconf.actionOpt(msg).getOrElse(defaultAction))
    }

    // used by Global.deprecationWarnings, which is used by sbt
    def deprecationWarnings: List[(Position, String)] = summaryMap(Action.WarningSummary, WarningCategory.Deprecation).toList.map(p => (p._1, p._2.msg))
    def uncheckedWarnings: List[(Position, String)]   = summaryMap(Action.WarningSummary, WarningCategory.Unchecked).toList.map(p => (p._1, p._2.msg))

    def allConditionalWarnings: List[(Position, String)] = summarizedWarnings.toList.sortBy(_._1.name).flatMap(_._2.toList.map(p => (p._1, p._2.msg)))

    // useful in REPL because line parsing doesn't entail a new Run
    def clearAllConditionalWarnings(): Unit = {
      summarizedWarnings.clear()
      summarizedInfos.clear()
    }

    /** Has any macro expansion used a fallback during this run? */
    var seenMacroExpansionsFallingBack = false

    def summarizeErrors(): Unit = if (!reporter.hasErrors) {
      for (c <- summarizedWarnings.keys.toList.sortBy(_.name))
        summarize(Action.WarningSummary, c)
      for (c <- summarizedInfos.keys.toList.sortBy(_.name))
        summarize(Action.InfoSummary, c)

      if (seenMacroExpansionsFallingBack)
        warning(NoPosition, "some macros could not be expanded and code fell back to overridden methods;"+
                "\nrecompiling with generated classfiles on the classpath might help.", WarningCategory.Other, site = "")

      // todo: migrationWarnings

      if (settings.fatalWarnings && reporter.hasWarnings)
        reporter.error(NoPosition, "No warnings can be incurred under -Werror.")
    }
  }
}

object Reporting {
  sealed trait Message {
    def pos: Position
    def msg: String
    def category: WarningCategory
    def site: String // sym.FullName of the location where the warning is positioned, may be empty
    def origin: String
  }

  object Message {
    final case class Plain(pos: Position, msg: String, category: WarningCategory, site: String, origin: String, since: Version) extends Message
    object Plain {
      def apply(pos: Position, msg: String, category: WarningCategory): Plain =
        Plain(pos, msg, category, "", "", Version.fromString(""))
    }

    // `site` and `origin` may be empty
    final case class Deprecation(pos: Position, msg: String, site: String, origin: String, since: Version) extends Message {
      def category: WarningCategory = WarningCategory.Deprecation
    }
  }

  sealed trait WarningCategory {
    def name: String = name0
    private[this] lazy val name0: String = {
      val objectName = this.getClass.getName.split('$').last
      WarningCategory.insertDash.replaceAllIn(objectName, "-")
        .stripPrefix("-")
        .stripSuffix("-")
        .toLowerCase(ENGLISH)
    }

    def includes(o: WarningCategory): Boolean = this eq o
    def summaryCategory: WarningCategory = this
  }

  object WarningCategory {
    private val insertDash = "(?=[A-Z][a-z])".r

    val builtIn: mutable.Map[String, WarningCategory] = mutable.Map.empty
    private def add(c: WarningCategory): Unit = builtIn += ((c.name, c))

    object Deprecation extends WarningCategory; add(Deprecation)

    object Unchecked extends WarningCategory; add(Unchecked)

    object Optimizer extends WarningCategory; add(Optimizer)

    object Scaladoc extends WarningCategory; add(Scaladoc)

    object JavaSource extends WarningCategory; add(JavaSource)

    object Unspecified extends WarningCategory; add(Unspecified)

    sealed trait Other extends WarningCategory { override def summaryCategory: WarningCategory = Other }
    object Other extends Other { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Other] }; add(Other)
    object OtherShadowing extends Other; add(OtherShadowing)
    object OtherPureStatement extends Other; add(OtherPureStatement)
    object OtherMigration extends Other; add(OtherMigration)
    object OtherMatchAnalysis extends WarningCategory; add(OtherMatchAnalysis)
    object OtherDebug extends WarningCategory; add(OtherDebug)

    sealed trait WFlag extends WarningCategory { override def summaryCategory: WarningCategory = WFlag }
    object WFlag extends WFlag { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[WFlag] }; add(WFlag)
    object WFlagDeadCode extends WFlag; add(WFlagDeadCode)
    object WFlagExtraImplicit extends WFlag; add(WFlagExtraImplicit)
    object WFlagNumericWiden extends WFlag; add(WFlagNumericWiden)
    object WFlagSelfImplicit extends WFlag; add(WFlagSelfImplicit)
    object WFlagValueDiscard extends WFlag; add(WFlagValueDiscard)

    sealed trait Unused extends WarningCategory { override def summaryCategory: WarningCategory = Unused }
    object Unused extends Unused { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Unused] }; add(Unused)
    object UnusedImports extends Unused; add(UnusedImports)
    object UnusedPatVars extends Unused; add(UnusedPatVars)
    object UnusedPrivates extends Unused; add(UnusedPrivates)
    object UnusedLocals extends Unused; add(UnusedLocals)
    object UnusedParams extends Unused; add(UnusedParams)
    object UnusedNowarn extends Unused; add(UnusedNowarn)

    sealed trait Lint extends WarningCategory { override def summaryCategory: WarningCategory = Lint }
    object Lint extends Lint { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Lint] }; add(Lint)
    object LintAdaptedArgs extends Lint; add(LintAdaptedArgs)
    object LintNullaryUnit extends Lint; add(LintNullaryUnit)
    object LintInaccessible extends Lint; add(LintInaccessible)
    object LintNullaryOverride extends Lint; add(LintNullaryOverride)
    object LintInferAny extends Lint; add(LintInferAny)
    object LintMissingInterpolator extends Lint; add(LintMissingInterpolator)
    object LintDocDetached extends Lint; add(LintDocDetached)
    object LintPrivateShadow extends Lint; add(LintPrivateShadow)
    object LintTypeParameterShadow extends Lint; add(LintTypeParameterShadow)
    object LintPolyImplicitOverload extends Lint; add(LintPolyImplicitOverload)
    object LintOptionImplicit extends Lint; add(LintOptionImplicit)
    object LintDelayedinitSelect extends Lint; add(LintDelayedinitSelect)
    object LintPackageObjectClasses extends Lint; add(LintPackageObjectClasses)
    object LintStarsAlign extends Lint; add(LintStarsAlign)
    object LintConstant extends Lint; add(LintConstant)
    object LintNonlocalReturn extends Lint; add(LintNonlocalReturn)
    object LintImplicitNotFound extends Lint; add(LintImplicitNotFound)
    object LintSerial extends Lint; add(LintSerial)
    object LintEtaZero extends Lint; add(LintEtaZero)
    object LintEtaSam extends Lint; add(LintEtaSam)
    object LintDeprecation extends Lint; add(LintDeprecation)
    object LintBynameImplicit extends Lint; add(LintBynameImplicit)
    object LintRecurseWithDefault extends Lint; add(LintRecurseWithDefault)
    object LintUnitSpecialization extends Lint; add(LintUnitSpecialization)

    sealed trait Feature extends WarningCategory { override def summaryCategory: WarningCategory = Feature }
    object Feature extends Feature { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Feature] }; add(Feature)
    object FeatureDynamics extends Feature; add(FeatureDynamics)
    object FeatureExistentials extends Feature; add(FeatureExistentials)
    object FeatureHigherKinds extends Feature; add(FeatureHigherKinds)
    object FeatureImplicitConversions extends Feature; add(FeatureImplicitConversions)
    object FeaturePostfixOps extends Feature; add(FeaturePostfixOps)
    object FeatureReflectiveCalls extends Feature; add(FeatureReflectiveCalls)
    object FeatureMacros extends Feature; add(FeatureMacros)

    case class Custom private (override val name: String) extends WarningCategory {
      override def includes(o: WarningCategory): Boolean = this == o
    }
    def custom(name: String): Custom = {
      val n = WarningCategory.insertDash.replaceAllIn(name, "-")
        .stripPrefix("-")
        .stripSuffix("-")
        .toLowerCase(ENGLISH)
      Custom(n)
    }

    def parse(category: String): WarningCategory =
      WarningCategory.builtIn.getOrElse(category, WarningCategory.custom(category))
  }

  sealed trait Version {
    def orig: String
    def filterString: String
  }
  object Version {
    final case class NonParseableVersion(orig: String) extends Version {
      def filterString: String = ""
    }

    final case class ParseableVersion(orig: String, maj: Int, min: Option[Int], patch: Option[Int]) extends Version {
      def filterString: String = s"$maj" + min.map(m => s".$m" + patch.map(p => s".$p").getOrElse("")).getOrElse("")
      def greater(other: ParseableVersion): Boolean = {
        maj > other.maj ||
          maj == other.maj && {
            val am = min.getOrElse(0)
            val bm = other.min.getOrElse(0)
            am > bm ||
              am == bm && {
                val ap = patch.getOrElse(0)
                val bp = other.patch.getOrElse(0)
                ap > bp
              }
          }
      }

      def same(other: ParseableVersion): Boolean =
        maj == other.maj &&
          min.getOrElse(0) == other.min.getOrElse(0) &&
          patch.getOrElse(0) == other.patch.getOrElse(0)

      def smaller(other: ParseableVersion): Boolean = {
        maj < other.maj ||
          maj == other.maj && {
            val am = min.getOrElse(0)
            val bm = other.min.getOrElse(0)
            am < bm ||
              am == bm && {
                val ap = patch.getOrElse(0)
                val bp = other.patch.getOrElse(0)
                ap < bp
              }
          }
      }
    }

    val VersionPattern: Regex = """(?:.*?\s+)??(\d+)(?:\.(\d+)(?:\.(\d+))?)?(?:\W.*)?""".r
    def fromString(s: String): Version = s match {
      case VersionPattern(maj, min, pat) =>
        ParseableVersion(s, maj.toInt, Option(min).map(_.toInt), Option(pat).map(_.toInt))
      case _ =>
        NonParseableVersion(s)
    }

    val VersionNumberPattern: Regex = """(\d+)(?:\.(\d+)(?:\.(\d+))?)?""".r
    def fromNumberOnlyString(s: String): Version = s match {
      case VersionNumberPattern(maj, min, pat) =>
        ParseableVersion(s, maj.toInt, Option(min).map(_.toInt), Option(pat).map(_.toInt))
      case _ =>
        NonParseableVersion(s)
    }
  }

  sealed trait MessageFilter {
    def matches(message: Message): Boolean
  }

  object MessageFilter {
    object Any extends MessageFilter {
      def matches(message: Message): Boolean = true
    }

    final case class Category(cat: WarningCategory) extends MessageFilter {
      def matches(message: Message): Boolean = cat.includes(message.category)
    }

    final case class MessagePattern(pattern: Regex) extends MessageFilter {
      def matches(message: Message): Boolean = pattern.findFirstIn(message.msg).nonEmpty
    }

    final case class SitePattern(pattern: Regex) extends MessageFilter {
      def matches(message: Message): Boolean = pattern.matches(message.site)
    }

    final case class SourcePattern(pattern: Regex) extends MessageFilter {
      private[this] val cache = mutable.Map.empty[SourceFile, Boolean]

      def matches(message: Message): Boolean = cache.getOrElseUpdate(message.pos.source, {
        val sourcePath = message.pos.source.file.canonicalPath.replace("\\", "/")
        pattern.findFirstIn(sourcePath).nonEmpty
      })
    }

    final case class OriginPattern(pattern: Regex) extends MessageFilter {
      def matches(message: Message): Boolean = pattern.matches(message.origin)
    }

    final case class SincePattern(comp: Int, version: ParseableVersion) extends MessageFilter {
      private[this] def isMatch(mv: ParseableVersion): Boolean =
        if (comp == -1) mv.smaller(version)
        else if (comp == 0) mv.same(version)
        else mv.greater(version)
      def matches(message: Message): Boolean = message match {
        case Message.Deprecation(_, _, _, _, mv: ParseableVersion) => isMatch(mv)
        case Message.Plain(_, _, _, _, _, mv: ParseableVersion)    => isMatch(mv)
        case _                                                     => false
      }
    }
  }

  sealed trait Action

  object Action {
    object Error extends Action
    object Warning extends Action
    object WarningSummary extends Action
    object WarningVerbose extends Action
    object Info extends Action
    object InfoSummary extends Action
    object InfoVerbose extends Action
    object Silent extends Action

    def parse(s: String): Either[String, Action] = s match {
      case "error" | "e"            => Right(Error)
      case "warning" | "w"          => Right(Warning)
      case "warning-summary" | "ws" => Right(WarningSummary)
      case "warning-verbose" | "wv" => Right(WarningVerbose)
      case "info" | "i"             => Right(Info)
      case "info-summary" | "is"    => Right(InfoSummary)
      case "info-verbose" | "iv"    => Right(InfoVerbose)
      case "silent" | "s"           => Right(Silent)
      case _                        => Left(s"unknonw action: `$s`")
    }
  }

  final case class WConf(filters: List[(List[MessageFilter], Action)]) {
    def action(message: Message): Action = actionOpt(message).getOrElse(Action.Warning)

    def actionOpt(message: Message): Option[Action] = filters.find(_._1.forall(_.matches(message))) map {
      case (_, action) => action
    }
  }

  object WConf {
    import MessageFilter._

    private def regex(s: String) =
      try Right(s.r)
      catch { case e: PatternSyntaxException => Left(s"invalid pattern `$s`: ${e.getMessage}") }

    def parseFilter(s: String, rootDir: String): Either[String, MessageFilter] = {
      if (s == "any") {
        Right(Any)
      } else if (s.startsWith("msg=")) {
        regex(s.substring(4)).map(MessagePattern)
      } else if (s.startsWith("cat=")) {
        val cs = s.substring(4)
        Right(Category(WarningCategory.parse(cs)))
      } else if (s.startsWith("site=")) {
        regex(s.substring(5)).map(SitePattern)
      } else if (s.startsWith("origin=")) {
        regex(s.substring(7)).map(OriginPattern)
      } else if(s.startsWith("since")) {
        def fail = Left(s"invalid since filter: `$s`; required shape: `since<1.2.3`, `since=3.2`, `since>2`")
        if (s.length < 6) fail
        else {
          val v = Version.fromNumberOnlyString(s.substring(6))
          val op = s.charAt(5) match {
            case '<' => -1
            case '=' => 0
            case '>' => 1
            case _ => 99
          }
          (v, op) match {
            case (_: NonParseableVersion, _) => fail
            case (_, 99) => fail
            case (pv: ParseableVersion, o) => Right(SincePattern(o, pv))
          }
        }
      } else if (s.startsWith("src=")) {
        val arg = s.substring(4)
        val pat = new mutable.StringBuilder()
        if (rootDir.nonEmpty) pat += '^' ++= rootDir
        // Also prepend prepend a `/` if rootDir is empty, the pattern has to match
        // the beginning of a path segment
        if (!rootDir.endsWith("/") && !arg.startsWith("/")) pat += '/'
        pat ++= arg
        if (!arg.endsWith("$")) pat += '$'
        regex(pat.toString).map(SourcePattern)
      } else {
        Left(s"unknown filter: $s")
      }
    }

    def parse(setting: List[String], rootDir: String): Either[List[String], WConf] = {
      if (setting.isEmpty) Right(WConf(Nil))
      else {
        val parsedConfs: List[Either[List[String], (List[MessageFilter], Action)]] = setting.map(conf => {
          val parts = conf.split("[&:]") // TODO: don't split on escaped \&
          val (ms, fs) = parts.view.init.map(parseFilter(_, rootDir)).toList.partitionMap(identity)
          if (ms.nonEmpty) Left(ms)
          else if (fs.isEmpty) Left(List("no filters or no action defined"))
          else Action.parse(parts.last) match {
            case Right(a) => Right((fs, a))
            case Left(s)  => Left(List(s))
          }
        })
        val (ms, fs) = parsedConfs.partitionMap(identity)
        if (ms.nonEmpty) Left(ms.flatten)
        else Right(WConf(fs))
      }
    }
  }

  case class Suppression(annotPos: Position, filters: List[MessageFilter], start: Int, end: Int) {
    private[this] var _used = false
    def used: Boolean = _used
    def markUsed(): Unit = { _used = true }


    def matches(message: Message): Boolean = {
      val pos = message.pos
      pos.isDefined && start <= pos.start && pos.end <= end && filters.exists(_.matches(message))
    }
  }
}
