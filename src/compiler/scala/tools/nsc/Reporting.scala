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

import java.util.regex.PatternSyntaxException
import scala.collection.mutable
import scala.reflect.internal
import scala.reflect.internal.util.StringOps.countElementsAsString
import scala.reflect.internal.util.{NoSourceFile, Position, SourceFile}
import scala.tools.nsc.Reporting.Version.{NonParseableVersion, ParseableVersion}
import scala.tools.nsc.Reporting._
import scala.tools.nsc.settings.ScalaVersion
import scala.util.matching.Regex

/** Provides delegates to the reporter doing the actual work.
 * PerRunReporting implements per-Run stateful info tracking and reporting
 */
trait Reporting extends internal.Reporting { self: ast.Positions with CompilationUnits with internal.Symbols =>
  def settings: Settings

  @deprecated("use `globalError` instead", since = "2.13.4")
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

    private val suppressions: mutable.LinkedHashMap[SourceFile, mutable.ListBuffer[Suppression]] = mutable.LinkedHashMap.empty
    private val suppressionsComplete: mutable.Set[SourceFile] = mutable.Set.empty
    private val suspendedMessages: mutable.LinkedHashMap[SourceFile, mutable.LinkedHashSet[Message]] = mutable.LinkedHashMap.empty

    // Used in REPL. The old run is used for parsing. Don't discard its suspended warnings.
    def initFrom(old: PerRunReporting): Unit = {
      suspendedMessages ++= old.suspendedMessages
    }

    private def isSuppressed(warning: Message): Boolean =
      suppressions.getOrElse(warning.pos.source, Nil).find(_.matches(warning)) match {
        case Some(s) => s.markUsed(); true
        case _ => false
      }

    def clearSuppressionsComplete(sourceFile: SourceFile): Unit = suppressionsComplete -= sourceFile

    def addSuppression(sup: Suppression): Unit = {
      val source = sup.annotPos.source
      suppressions.getOrElseUpdate(source, mutable.ListBuffer.empty) += sup
    }

    def suppressionExists(pos: Position): Boolean =
      suppressions.getOrElse(pos.source, Nil).exists(_.annotPos.point == pos.point)

    def runFinished(hasErrors: Boolean): Unit = {
      val warningNowarn = settings.warnUnusedNowarn
      val warningUnused = settings.warnUnusedUnused && ScalaVersion.current >= ScalaVersion("2.13.11")
      def maybeWarn(s: Suppression): Unit =
        if (!s.used)
          if (!s.synthetic) {
            if (warningNowarn)
              issueWarning(Message.Plain(s.annotPos, "@nowarn annotation does not suppress any warnings", WarningCategory.UnusedNowarn, ""))
          }
          else if (warningUnused && s.filters.exists { case MessageFilter.Category(WarningCategory.Unused) => true case _ => false })
            issueWarning(Message.Plain(s.annotPos, "@unused annotation does not suppress any warnings", WarningCategory.UnusedUnused, ""))
      // report suspended messages (in case the run finished before typer)
      suspendedMessages.valuesIterator.foreach(_.foreach(issueWarning))
      // report unused nowarns only if all all phases are done. scaladoc doesn't run all phases.
      if (!hasErrors && (warningNowarn || warningUnused) && !settings.isScaladoc)
        for {
          source <- suppressions.keysIterator.toList
          sups   <- suppressions.remove(source)
          sup    <- sups.reverse
        } maybeWarn(sup)
    }

    def reportSuspendedMessages(unit: CompilationUnit): Unit = {
      // sort suppressions. they are not added in any particular order because of lazy type completion
      for (sups <- suppressions.get(unit.source))
        suppressions(unit.source) = sups.sortBy(sup => 0 - sup.start)
      suppressionsComplete += unit.source
      suspendedMessages.remove(unit.source).foreach(_.foreach(issueIfNotSuppressed))
    }

    private def summaryMap(action: Action, category: WarningCategory) = {
      val sm = (action: @unchecked) match {
        case Action.WarningSummary => summarizedWarnings
        case Action.InfoSummary => summarizedInfos
      }
      sm.getOrElseUpdate(category, mutable.LinkedHashMap.empty)
    }

    private def issueWarning(warning: Message): Unit = {
      def verbose = warning match {
        case Message.Deprecation(_, msg, site, origin, version) => s"[${warning.category.name} @ $site | origin=$origin | version=${version.filterString}] $msg"
        case Message.Origin(_, msg, category, site, origin @ _) => s"[${category.name} @ $site] $msg"  // origin text is obvious at caret
        case Message.Plain(_, msg, category, site) => s"[${category.name} @ $site] $msg"
      }
      wconf.action(warning) match {
        case Action.Error => reporter.error(warning.pos, warning.msg)
        case Action.Warning => reporter.warning(warning.pos, warning.msg)
        case Action.WarningVerbose => reporter.warning(warning.pos, verbose)
        case Action.Info => reporter.echo(warning.pos, warning.msg)
        case Action.InfoVerbose => reporter.echo(warning.pos, verbose)
        case a @ (Action.WarningSummary | Action.InfoSummary) =>
          val m = summaryMap(a, warning.category.summaryCategory)
          if (!m.contains(warning.pos)) m.addOne((warning.pos, warning))
        case Action.Silent =>
      }
    }

    def shouldSuspend(warning: Message): Boolean =
      warning.pos.source != NoSourceFile && !suppressionsComplete(warning.pos.source)

    def issueIfNotSuppressed(warning: Message): Unit =
      if (shouldSuspend(warning))
        suspendedMessages.getOrElseUpdate(warning.pos.source, mutable.LinkedHashSet.empty) += warning
      else {
        if (!isSuppressed(warning))
          issueWarning(warning)
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

    override def deprecationWarning(pos: Position, msg: String, since: String, site: String, origin: String): Unit =
      issueIfNotSuppressed(Message.Deprecation(pos, msg, site, origin, Version.fromString(since)))

    def deprecationWarning(pos: Position, origin: Symbol, site: Symbol, msg: String, since: String): Unit =
      deprecationWarning(pos, msg, since, siteName(site), siteName(origin))

    def deprecationWarning(pos: Position, origin: Symbol, site: Symbol): Unit = {
      val version = origin.deprecationVersion.getOrElse("")
      val since   = if (version.isEmpty) version else s" (since $version)"
      val message = origin.deprecationMessage.map(": " + _).getOrElse("")
      deprecationWarning(pos, origin, site, s"$origin${origin.locationString} is deprecated$since$message", version)
    }

    private[this] var reportedFeature = Set[Symbol]()
    protected def featureReported(featureTrait: Symbol): Unit = reportedFeature += featureTrait

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

      val msg = s"$featureDesc $req be enabled\nby making the implicit value $fqname visible.$explain".replace("#", construct)
      // maybe pos.source.file.file.getParentFile.getName or Path(source.file.file).parent.name
      def parentFileName(source: internal.util.SourceFile) =
        Option(java.nio.file.Paths.get(source.path).getParent).map(_.getFileName.toString)
      // don't error on postfix in pre-0.13.18 xsbt/Compat.scala
      def isSbtCompat = (featureName == "postfixOps"
        && pos.source.file.name == "Compat.scala"
        && parentFileName(pos.source).getOrElse("") == "xsbt"
        && Thread.currentThread.getStackTrace.exists(_.getClassName.startsWith("sbt."))
      )
      // on postfix error, include interesting infix warning
      def isXfix = featureName == "postfixOps" && suspendedMessages.get(pos.source).map(_.exists(w => pos.includes(w.pos))).getOrElse(false)
      if (required && !isSbtCompat) {
        val amended = if (isXfix) s"$msg\n${suspendedMessages(pos.source).filter(pos includes _.pos).map(_.msg).mkString("\n")}" else msg
        reporter.error(pos, amended)
      } else warning(pos, msg, featureCategory(featureTrait.nameString), site)
    }

    // Used in the optimizer where we have no symbols, the site string is created from the class internal name and method name.
    def warning(pos: Position, msg: String, category: WarningCategory, site: String): Unit =
      issueIfNotSuppressed(Message.Plain(pos, msg, category, site))

    // Preferred over the overload above whenever a site symbol is available
    def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol): Unit =
      warning(pos, msg, category, siteName(site))

    // Provide an origin for the warning.
    def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol, origin: String): Unit =
      issueIfNotSuppressed(Message.Origin(pos, msg, category, siteName(site), origin))

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

    // i.e., summarize warnings
    def summarizeErrors(): Unit = if (!reporter.hasErrors && !settings.nowarn.value) {
      for (c <- summarizedWarnings.keys.toList.sortBy(_.name))
        summarize(Action.WarningSummary, c)
      for (c <- summarizedInfos.keys.toList.sortBy(_.name))
        summarize(Action.InfoSummary, c)

      if (seenMacroExpansionsFallingBack)
        warning(NoPosition, "some macros could not be expanded and code fell back to overridden methods;"+
                "\nrecompiling with generated classfiles on the classpath might help.", WarningCategory.Other, site = "")

      // todo: migrationWarnings

      if (settings.fatalWarnings.value && reporter.hasWarnings)
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
  }

  object Message {
    // an ordinary Message has a `category` for filtering and the `site` where it was issued
    final case class Plain(pos: Position, msg: String, category: WarningCategory, site: String) extends Message

    // a Plain message with an `origin` which should not be empty. For example, the origin of an unused import is the fully-qualified selection
    final case class Origin(pos: Position, msg: String, category: WarningCategory, site: String, origin: String) extends Message

    // `site` and `origin` may be empty
    final case class Deprecation(pos: Position, msg: String, site: String, origin: String, since: Version) extends Message {
      def category: WarningCategory = WarningCategory.Deprecation
    }
  }

  sealed trait WarningCategory {
    lazy val name: String = {
      val objectName = this.getClass.getName.split('$').last
      WarningCategory.insertDash.replaceAllIn(objectName, "-")
        .stripPrefix("-")
        .stripSuffix("-")
        .toLowerCase
    }

    def includes(o: WarningCategory): Boolean = this eq o
    def summaryCategory: WarningCategory = this
  }

  object WarningCategory {
    private val insertDash = "(?=[A-Z][a-z])".r

    val all: mutable.Map[String, WarningCategory] = mutable.Map.empty
    private def add(c: WarningCategory): Unit = all += ((c.name, c))

    object Deprecation extends WarningCategory; add(Deprecation)

    object Unchecked extends WarningCategory; add(Unchecked)

    object Optimizer extends WarningCategory; add(Optimizer)

    object Scaladoc extends WarningCategory; add(Scaladoc)

    object JavaSource extends WarningCategory; add(JavaSource)

    sealed trait Other extends WarningCategory { override def summaryCategory: WarningCategory = Other }
    object Other extends Other { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Other] }; add(Other)
    object OtherShadowing extends Other; add(OtherShadowing)
    object OtherPureStatement extends Other; add(OtherPureStatement)
    object OtherMigration extends Other; add(OtherMigration)
    object OtherMatchAnalysis extends Other; add(OtherMatchAnalysis)
    object OtherDebug extends Other; add(OtherDebug)
    object OtherNullaryOverride extends Other; add(OtherNullaryOverride)
    object OtherNonCooperativeEquals extends Other; add(OtherNonCooperativeEquals)
    object OtherImplicitType extends Other; add(OtherImplicitType)

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
    object UnusedUnused extends Unused; add(UnusedUnused)

    sealed trait Lint extends WarningCategory { override def summaryCategory: WarningCategory = Lint }
    object Lint extends Lint { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Lint] }; add(Lint)
    object LintAdaptedArgs extends Lint; add(LintAdaptedArgs)
    object LintNullaryUnit extends Lint; add(LintNullaryUnit)
    object LintInaccessible extends Lint; add(LintInaccessible)
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
    object LintMultiargInfix extends Lint; add(LintMultiargInfix)
    object LintPerformance extends Lint; add(LintPerformance)

    sealed trait Feature extends WarningCategory { override def summaryCategory: WarningCategory = Feature }
    object Feature extends Feature { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Feature] }; add(Feature)
    object FeatureDynamics extends Feature; add(FeatureDynamics)
    object FeatureExistentials extends Feature; add(FeatureExistentials)
    object FeatureHigherKinds extends Feature; add(FeatureHigherKinds)
    object FeatureImplicitConversions extends Feature; add(FeatureImplicitConversions)
    object FeaturePostfixOps extends Feature; add(FeaturePostfixOps)
    object FeatureReflectiveCalls extends Feature; add(FeatureReflectiveCalls)
    object FeatureMacros extends Feature; add(FeatureMacros)
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

    final case class DeprecatedOrigin(pattern: Regex) extends MessageFilter {
      def matches(message: Message): Boolean = message match {
        case m: Message.Deprecation => pattern.matches(m.origin)
        case m: Message.Origin      => pattern.matches(m.origin)
        case _ => false
      }
    }

    final case class DeprecatedSince(comp: Int, version: ParseableVersion) extends MessageFilter {
      def matches(message: Message): Boolean = message match {
        case Message.Deprecation(_, _, _, _, mv: ParseableVersion) =>
          if (comp == -1) mv.smaller(version)
          else if (comp == 0) mv.same(version)
          else mv.greater(version)
        case _ => false
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
  }

  final case class WConf(filters: List[(List[MessageFilter], Action)]) {
    def action(message: Message): Action = filters.find(_._1.forall(_.matches(message))) match {
      case Some((_, action)) => action
      case _ => Action.Warning
    }
  }

  object WConf {
    import Action._
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
        val c = WarningCategory.all.get(cs).map(Category)
        c.toRight(s"Unknown category: `$cs`")
      } else if (s.startsWith("site=")) {
        regex(s.substring(5)).map(SitePattern)
      } else if (s.startsWith("origin=")) {
        regex(s.substring(7)).map(DeprecatedOrigin)
      } else if (s.startsWith("since")) {
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
            case (pv: ParseableVersion, o) => Right(DeprecatedSince(o, pv))
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
      def parseAction(s: String): Either[List[String], Action] = s match {
        case "error" | "e" => Right(Error)
        case "warning" | "w" => Right(Warning)
        case "warning-summary" | "ws" => Right(WarningSummary)
        case "warning-verbose" | "wv" => Right(WarningVerbose)
        case "info" | "i" => Right(Info)
        case "info-summary" | "is" => Right(InfoSummary)
        case "info-verbose" | "iv" => Right(InfoVerbose)
        case "silent" | "s" => Right(Silent)
        case _ => Left(List(s"unknown action: `$s`"))
      }

      if (setting.isEmpty) Right(WConf(Nil))
      else {
        val parsedConfs: List[Either[List[String], (List[MessageFilter], Action)]] = setting.map(conf => {
          val parts = conf.split("[&:]") // TODO: don't split on escaped \&
          val (ms, fs) = parts.view.init.map(parseFilter(_, rootDir)).toList.partitionMap(identity)
          if (ms.nonEmpty) Left(ms)
          else if (fs.isEmpty) Left(List("no filters or no action defined"))
          else parseAction(parts.last).map((fs, _))
        })
        val (ms, fs) = parsedConfs.partitionMap(identity)
        if (ms.nonEmpty) Left(ms.flatten)
        else Right(WConf(fs))
      }
    }
  }

  case class Suppression(annotPos: Position, filters: List[MessageFilter], start: Int, end: Int, synthetic: Boolean = false) {
    private[this] var _used = false
    def used: Boolean = _used
    def markUsed(): Unit = { _used = true }

    def matches(message: Message): Boolean = {
      val pos = message.pos
      pos.isDefined && start <= pos.start && pos.end <= end && filters.forall(_.matches(message))
    }
  }
}
