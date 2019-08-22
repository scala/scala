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
import scala.reflect.internal.util.Position
import scala.reflect.internal.util.StringOps.countElementsAsString
import scala.tools.nsc.Reporting.{Action, Message, Version, WConf, WarningCategory}
import scala.util.matching.Regex

/** Provides delegates to the reporter doing the actual work.
 * PerRunReporting implements per-Run stateful info tracking and reporting
 */
trait Reporting extends scala.reflect.internal.Reporting { self: ast.Positions with CompilationUnits with scala.reflect.internal.Symbols =>
  def settings: Settings

  @deprecated("use `globalError` instead")
  def error(msg: String): Unit = globalError(msg)

  // a new instance of this class is created for every Run (access the current instance via `currentRun.reporting`)
  protected def PerRunReporting = new PerRunReporting
  class PerRunReporting extends PerRunReportingBase {
    lazy val wconf = WConf.parse(settings.Wconf.value.reverse) match {
      case Left(msgs) =>
        globalError(s"Failed to parse `-Wconf` configuration: ${settings.Wconf.value}\n${msgs.mkString("\n")}")
        WConf(Nil)
      case Right(c) => c
    }

    private val summarizedWarnings: mutable.Map[WarningCategory, mutable.LinkedHashMap[Position, Message]] = mutable.HashMap.empty
    private val summarizedInfos: mutable.Map[WarningCategory, mutable.LinkedHashMap[Position, Message]] = mutable.HashMap.empty

    private def summaryMap(action: Action, category: WarningCategory) = {
      val sm = (action: @unchecked) match {
        case Action.WarningSummary => summarizedWarnings
        case Action.InfoSummary => summarizedInfos
      }
      sm.getOrElseUpdate(category, mutable.LinkedHashMap.empty)
    }

    private def issueWarning(warning: Message): Unit = wconf.action(warning) match {
      case Action.Error => reporter.error(warning.pos, warning.msg)
      case Action.Warning => reporter.warning(warning.pos, warning.msg)
      case Action.Info => reporter.echo(warning.pos, warning.msg)
      case a @ (Action.WarningSummary | Action.InfoSummary) =>
        val m = summaryMap(a, warning.category)
        if (!m.contains(warning.pos)) m.addOne((warning.pos, warning))
      case Action.Silent =>
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
        else s"; change -Wconf for ${category.name} to display individual messages"
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
        val messageKind = if (isWarn) "warning" else "info message"
        sinceAndAmount.foreach { case (since, numWarnings) =>
          val warningsSince = if (since.nonEmpty) s" (since $since)" else ""
          val warningVerb   = if (numWarnings == 1) "was" else "were"
          val warningCount  = countElementsAsString(numWarnings, s"${category.name} $messageKind")
          val rerun         = if (deprecationSummary) "" else rerunMsg
          val msg           = s"there ${warningVerb} ${warningCount}${warningsSince}${rerun}"
          if (isWarn) reporter.warning(NoPosition, msg)
          else reporter.echo(NoPosition, msg)
        }
        if (deprecationSummary) {
          val numWarnings   = m.size
          val warningVerb   = if (numWarnings == 1) "was" else "were"
          val warningCount  = countElementsAsString(numWarnings, s"${category.name} $messageKind")
          val rerun         = rerunMsg
          val msg           = s"there ${warningVerb} ${warningCount} in total${rerun}"
          if (isWarn) reporter.warning(NoPosition, msg)
          else reporter.echo(NoPosition, msg)
        }
      }
    }

    private def siteName(s: Symbol) = if (s.exists) s.fullNameString else ""

    def deprecationWarning(pos: Position, msg: String, since: String, site: String, origin: String): Unit =
      issueWarning(Message.Deprecation(pos, msg, site, origin, new Version(since)))

    def deprecationWarning(pos: Position, origin: Symbol, site: Symbol, msg: String, since: String): Unit =
      deprecationWarning(pos, msg, since, siteName(site), siteName(origin))

    def deprecationWarning(pos: Position, origin: Symbol, site: Symbol): Unit = {
      val version = origin.deprecationVersion.getOrElse("")
      val since   = if (version.isEmpty) version else s" (since $version)"
      val message = origin.deprecationMessage.map(": " + _).getOrElse("")
      deprecationWarning(pos, origin, site, s"$origin${origin.locationString} is deprecated$since$message", version)
    }

    def warning(pos: Position, msg: String, category: WarningCategory, site: String): Unit =
      issueWarning(Message.Plain(pos, msg, category, site))

    def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol): Unit =
      warning(pos, msg, category, siteName(site))

    @deprecated("use `warning` instead")
    def uncheckedWarning(pos: Position, msg: String): Unit   = issueWarning(Message.Plain(pos, msg, WarningCategory.Unchecked, ""))
    @deprecated("use `warning` instead")
    def featureWarning(pos: Position, msg: String): Unit     = issueWarning(Message.Plain(pos, msg, WarningCategory.Feature, ""))
    @deprecated("use `warning` instead")
    def inlinerWarning(pos: Position, msg: String): Unit     = issueWarning(Message.Plain(pos, msg, WarningCategory.Optimizer, ""))

    // used by Global.deprecationWarnings, which is used by sbt
    def deprecationWarnings: List[(Position, String)] = summaryMap(Action.WarningSummary, WarningCategory.Deprecation).toList.map(p => (p._1, p._2.msg))
    def uncheckedWarnings: List[(Position, String)]   = summaryMap(Action.WarningSummary, WarningCategory.Unchecked).toList.map(p => (p._1, p._2.msg))

    def allConditionalWarnings: List[(Position, String)] = summarizedWarnings.toList.sortBy(_._1.name).flatMap(_._2.toList.map(p => (p._1, p._2.msg)))

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
      for (c <- summarizedWarnings.keys.toList.sortBy(_.name))
        summarize(Action.WarningSummary, c)
      for (c <- summarizedInfos.keys.toList.sortBy(_.name))
        summarize(Action.InfoSummary, c)

      if (seenMacroExpansionsFallingBack)
        reporter.warning(NoPosition, "some macros could not be expanded and code fell back to overridden methods;"+
                "\nrecompiling with generated classfiles on the classpath might help.")

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
  }

  object Message {
    final case class Plain(pos: Position, msg: String, category: WarningCategory, site: String) extends Message

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
  }

  object WarningCategory {
    private val insertDash = "(?=[A-Z][a-z])".r

    var all: mutable.Map[String, WarningCategory] = mutable.Map.empty
    private def add(c: WarningCategory): Unit = all += ((c.name, c))

    object Unspecific extends WarningCategory; add(Unspecific)

    object Deprecation extends WarningCategory; add(Deprecation)

    object Unchecked extends WarningCategory; add(Unchecked)

    object Optimizer extends WarningCategory; add(Optimizer)

    sealed trait WFlag extends WarningCategory
    object WFlag extends WFlag { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[WFlag] }; add(WFlag)
    object WFlagDeadCode extends WFlag; add(WFlagDeadCode)
    object WFlagExtraImplicit extends WFlag; add(WFlagExtraImplicit)
    object WFlagNumericWiden extends WFlag; add(WFlagNumericWiden)
    object WFlagOctalLiteral extends WFlag; add(WFlagOctalLiteral)
    object WFlagValueDiscard extends WFlag; add(WFlagValueDiscard)

    sealed trait Unused extends WarningCategory
    object Unused extends Unused { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Unused] }; add(Unused)
    object UnusedImports extends Unused; add(UnusedImports)
    object UnusedPatVars extends Unused; add(UnusedPatVars)
    object UnusedPrivates extends Unused; add(UnusedPrivates)
    object UnusedLocals extends Unused; add(UnusedLocals)
    object UnusedParams extends Unused; add(UnusedParams)

    sealed trait Lint extends WarningCategory
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
    object LintValpattern extends Lint; add(LintValpattern)
    object LintEtaZero extends Lint; add(LintEtaZero)
    object LintEtaSam extends Lint; add(LintEtaSam)
    object LintIncompleteDeprecation extends Lint; add(LintIncompleteDeprecation)

    sealed trait Feature extends WarningCategory
    object Feature extends Feature { override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Feature] }; add(Feature)
    object FeatureDynamics extends Feature; add(FeatureDynamics)
    object FeatureExistentials extends Feature; add(FeatureExistentials)
    object FeatureHigherKinds extends Feature; add(FeatureHigherKinds)
    object FeatureImplicitConversions extends Feature; add(FeatureImplicitConversions)
    object FeaturePostfixOps extends Feature; add(FeaturePostfixOps)
    object FeatureReflectiveCalls extends Feature; add(FeatureReflectiveCalls)
    object FeatureMacros extends Feature; add(FeatureMacros)
  }

  // X, X.Y, X.Y.Z, X.Y.Z.FOO-BAR. `rest` is ignored in comparison.
  final case class Version(orig: String, maj: Int, min: Option[Int], patch: Option[Int], rest: Option[String]) {
    def this(orig: String) = this(orig, -1, None, None, None) // TODO

    def greater(other: Version): Boolean = {
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

    def same(other: Version): Boolean =
      maj == other.maj &&
        min.getOrElse(0) == other.min.getOrElse(0) &&
        patch.getOrElse(0) == other.patch.getOrElse(0)

    def smaller(other: Version): Boolean = {
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
      def matches(message: Message): Boolean = pattern.matches(message.msg)
    }

    final case class SitePattern(pattern: Regex) extends MessageFilter {
      def matches(message: Message): Boolean = pattern.matches(message.site)
    }

    final case class SourcePattern(pattern: Regex) extends MessageFilter {
      def matches(message: Message): Boolean = {
        // TODO: message.pos.source.file, relativize, check
        true
      }
    }

    final case class DeprecatedOrigin(pattern: Regex) extends MessageFilter {
      def matches(message: Message): Boolean = message match {
        case m: Message.Deprecation => pattern.matches(m.origin)
        case _ => false
      }
    }

    final case class DeprecatedSince(comp: Int, version: Version) extends MessageFilter {
      def matches(message: Message): Boolean = message match {
        case m: Message.Deprecation =>
          if (comp == -1) m.since.smaller(version)
          else if (comp == 0) m.since.same(version)
          else m.since.greater(version)
        case _ => true
      }
    }
  }

  sealed trait Action

  object Action {
    object Error extends Action
    object Warning extends Action
    object WarningSummary extends Action
    object Info extends Action
    object InfoSummary extends Action
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

    def parse(setting: List[String]): Either[List[String], WConf] = {
      def filter(s: String): Either[String, MessageFilter] = {
        if (s == "any") {
          Right(Any)
        } else if (s.startsWith("msg=")) {
          Right(MessagePattern(s.substring(4).r))
        } else if (s.startsWith("cat=")) {
          val cs = s.substring(4)
          val c = WarningCategory.all.get(cs).map(Category)
          c.toRight(s"Unknown category: `$cs`")
        } else if (s.startsWith("site=")) {
          Right(SitePattern(s.substring(5).r))
        } else if (s.startsWith("origin=")) {
          Right(DeprecatedOrigin(s.substring(7).r))
        } else {
          Left(s"filter not yet implemented: $s")
        }
      }
      def action(s: String): Either[List[String], Action] = s match {
        case "error" | "e" => Right(Error)
        case "warning" | "w" => Right(Warning)
        case "warning-summary" | "ws" => Right(WarningSummary)
        case "info" | "i" => Right(Info)
        case "info-summary" | "is" => Right(InfoSummary)
        case "silent" | "s" => Right(Silent)
        case _ => Left(List(s"unknonw action: `$s`"))
      }

      if (setting.isEmpty) Right(WConf(Nil))
      else {
        val parsedConfs: List[Either[List[String], (List[MessageFilter], Action)]] = setting.map(conf => {
          val parts = conf.split("[&:]")
          val (ms, fs) = parts.view.init.map(filter).toList.partitionMap(identity)
          if (ms.nonEmpty) Left(ms)
          else if (fs.isEmpty) Left(List("no filters defined"))
          else action(parts.last).map((fs, _))
        })
        val (ms, fs) = parsedConfs.partitionMap(identity)
        if (ms.nonEmpty) Left(ms.flatten)
        else Right(WConf(fs))
      }
    }
  }
}
