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

import java.io.IOException
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}
import java.util.regex.PatternSyntaxException
import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.reflect.internal
import scala.reflect.internal.util.StringOps.countElementsAsString
import scala.reflect.internal.util.{CodeAction, NoSourceFile, Position, ReplBatchSourceFile, SourceFile, TextEdit}
import scala.tools.nsc.Reporting.Version.{NonParseableVersion, ParseableVersion}
import scala.tools.nsc.Reporting._
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
      case Right(conf) =>
        // configure cat=scala3-migration if it isn't yet
        val Migration = MessageFilter.Category(WarningCategory.Scala3Migration)
        val boost = (settings.isScala3: @nowarn) && !conf.filters.exists(_._1.exists(_ == Migration))
        if (boost) conf.copy(filters = conf.filters :+ (Migration :: Nil, Action.Error))
        else conf
    }

    private lazy val quickfixFilters = {
      if (settings.quickfix.isSetByUser && settings.quickfix.value.isEmpty) {
        globalError(s"Missing message filter for `-quickfix`; see `-quickfix:help` or use `-quickfix:any` to apply all available quick fixes.")
        Nil
      } else if (settings.quickFixSilent) {
        Nil
      } else {
        val parsed = settings.quickfix.value.map(WConf.parseFilter(_, rootDirPrefix))
        val msgs = parsed.collect { case Left(msg) => msg }
        if (msgs.nonEmpty) {
          globalError(s"Failed to parse `-quickfix` filters: ${settings.quickfix.value.mkString(",")}\n${msgs.mkString("\n")}")
          Nil
        } else parsed.collect { case Right(f) => f }
      }
    }

    private val skipRewriteAction = Set(Action.WarningSummary, Action.InfoSummary, Action.Silent)

    private def registerTextEdit(m: Message): Boolean =
      m.actions.exists(_.edits.nonEmpty) &&
      quickfixFilters.exists(_.matches(m)) && {
        m.actions.foreach(action => textEdits.addAll(action.edits))
        true
      }

    private def registerErrorTextEdit(pos: Position, msg: String, actions: List[CodeAction]): Boolean =
      actions.exists(_.edits.nonEmpty) &&
      quickfixFilters.exists {
        case MessageFilter.Any => true
        case mp: MessageFilter.MessagePattern => mp.check(msg)
        case sp: MessageFilter.SourcePattern => sp.check(pos)
        case _ => false
      } && {
        actions.foreach(action => textEdits.addAll(action.edits))
        true
      }

    private val summarizedWarnings: mutable.Map[WarningCategory, mutable.LinkedHashMap[Position, Message]] = mutable.HashMap.empty
    private val summarizedInfos: mutable.Map[WarningCategory, mutable.LinkedHashMap[Position, Message]] = mutable.HashMap.empty

    /**
     * The REPL creates two SourceFile instances for each line:
     *   - `requestFromLine` -> `parse`
     *   - Class Request has field `unit` initialized with a new SourceFile - note that the content is different (`paddedLine`)
     *
     * ReplBatchSourceFile has a reference to the first source file and we make sure to consistently use this
     * one for the warning suspension / suppression hash maps.
     */
    private def repSrc(s: SourceFile) = s match {
      case r: ReplBatchSourceFile => r.parserSource
      case _ => s
    }

    private val suppressions: mutable.LinkedHashMap[SourceFile, mutable.ListBuffer[Suppression]] = mutable.LinkedHashMap.empty
    private val suppressionsComplete: mutable.Set[SourceFile] = mutable.Set.empty
    private val suspendedMessages: mutable.LinkedHashMap[SourceFile, mutable.LinkedHashSet[Message]] = mutable.LinkedHashMap.empty

    private val textEdits: mutable.Set[TextEdit] = mutable.Set.empty

    // Used in REPL. The old run is used for parsing. Don't discard its suspended warnings.
    def initFrom(old: PerRunReporting): Unit = {
      suspendedMessages ++= old.suspendedMessages
    }

    def clearSuspendedMessages(): Unit = {
      suspendedMessages.clear()
    }

    private def isSuppressed(warning: Message): Boolean =
      suppressions.getOrElse(repSrc(warning.pos.source), Nil).find(_.matches(warning)) match {
        case Some(s) => s.markUsed(); true
        case _ => false
      }

    def clearSuppressionsComplete(sourceFile: SourceFile): Unit = suppressionsComplete -= repSrc(sourceFile)

    def addSuppression(sup: Suppression): Unit = {
      val source = sup.annotPos.source
      suppressions.getOrElseUpdate(repSrc(source), mutable.ListBuffer.empty) += sup
    }

    def suppressionExists(pos: Position): Boolean =
      suppressions.getOrElse(repSrc(pos.source), Nil).exists(_.annotPos.point == pos.point)

    def runFinished(hasErrors: Boolean): Unit = {
      // report suspended messages (in case the run finished before typer)
      suspendedMessages.valuesIterator.foreach(_.foreach(issueWarning))

      // report unused nowarns only if all all phases are done. scaladoc doesn't run all phases.
      if (!hasErrors && settings.warnUnusedNowarn && !settings.isScaladoc)
        for {
          source <- suppressions.keysIterator.toList
          sups   <- suppressions.remove(source)
          sup    <- sups.reverse
        } if (!sup.used && !sup.synthetic) issueWarning(Message.Plain(sup.annotPos, "@nowarn annotation does not suppress any warnings", WarningCategory.UnusedNowarn, "", Nil))

      // apply quick fixes
      quickfix(textEdits)
      textEdits.clear()
    }

    def reportSuspendedMessages(unit: CompilationUnit): Unit = {
      val src = repSrc(unit.source)
      // sort suppressions. they are not added in any particular order because of lazy type completion
      for (sups <- suppressions.get(src))
        suppressions(src) = sups.sortBy(sup => 0 - sup.start)
      suppressionsComplete += src
      suspendedMessages.remove(src).foreach(_.foreach(issueIfNotSuppressed))
    }

    private def summaryMap(action: Action, category: WarningCategory) = {
      val sm = (action: @unchecked) match {
        case Action.WarningSummary => summarizedWarnings
        case Action.InfoSummary => summarizedInfos
      }
      sm.getOrElseUpdate(category, mutable.LinkedHashMap.empty)
    }

    private def issueWarning(warning: Message): Unit = {
      val action = wconf.action(warning)

      val quickfixed = {
        if (!skipRewriteAction(action) && registerTextEdit(warning)) s"[rewritten by -quickfix] ${warning.msg}"
        else if (warning.actions.exists(_.edits.nonEmpty) && !settings.quickFixSilent) s"${warning.msg} [quickfixable]"
        else warning.msg
      }

      def helpMsg(level: String, isError: Boolean = false) = {
        def ifNonEmpty(kind: String, filter: String) = if (filter.nonEmpty) s", $kind=$filter" else ""
        def maybeSite = ifNonEmpty("site", warning.site)
        def maybeOrigin = warning match {
          case Message.Deprecation(_, _, _, origin, version, _) =>
            ifNonEmpty("origin", origin) + ifNonEmpty("version", version.filterString)
          case _ => ""
        }
        def filterHelp = s"msg=<part of the message>, cat=${warning.category.name}${maybeSite}${maybeOrigin}"
        def scala3migration =
          if (isError && warning.category == WarningCategory.Scala3Migration)
            "\nScala 3 migration messages are issued as errors under -Xsource:3. Use -Wconf or @nowarn to demote them to warnings or suppress."
          else ""
        s"$quickfixed$scala3migration\nApplicable -Wconf / @nowarn filters for this $level: $filterHelp"
      }

      action match {
        case Action.Error => reporter.error(warning.pos, helpMsg("fatal warning", isError = true), warning.actions)
        case Action.Warning => reporter.warning(warning.pos, quickfixed, warning.actions)
        case Action.WarningVerbose => reporter.warning(warning.pos, helpMsg("warning"), warning.actions)
        case Action.Info => reporter.echo(warning.pos, quickfixed, warning.actions)
        case Action.InfoVerbose => reporter.echo(warning.pos, helpMsg("message"), warning.actions)
        case a @ (Action.WarningSummary | Action.InfoSummary) =>
          val m = summaryMap(a, warning.category.summaryCategory)
          if (!m.contains(warning.pos)) m.addOne((warning.pos, warning))
        case Action.Silent =>
      }
    }

    def shouldSuspend(warning: Message): Boolean =
      warning.pos.source != NoSourceFile && !suppressionsComplete(repSrc(warning.pos.source))

    def issueIfNotSuppressed(warning: Message): Unit =
      if (shouldSuspend(warning))
        suspendedMessages.getOrElseUpdate(repSrc(warning.pos.source), mutable.LinkedHashSet.empty) += warning
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
      def skipAnon(s: Symbol, res: Symbol): Symbol =
        if (s.isRootSymbol || s == NoSymbol) res
        else if (s.isAnonymousClass || s.isLocalDummy) skipAnon(s.effectiveOwner, s.effectiveOwner)
        else skipAnon(s.effectiveOwner, res)
      // Similar to fullNameString, but don't jump to enclosing class. Keep full chain of symbols.
      def impl(s: Symbol): String =
        if (s.isRootSymbol || s == NoSymbol) s.nameString
        else if (s.owner.isEffectiveRoot) s.nameString
        else impl(s.effectiveOwner) + "." + s.nameString
      impl(skipAnon(sym, sym))
    } else ""

    override def deprecationWarning(pos: Position, msg: String, since: String, site: String, origin: String, actions: List[CodeAction] = Nil): Unit =
      issueIfNotSuppressed(Message.Deprecation(pos, msg, site, origin, Version.fromString(since), actions))

    // multiple overloads cannot use default args
    def deprecationWarning(pos: Position, origin: Symbol, site: Symbol, msg: String, since: String, actions: List[CodeAction]): Unit =
      deprecationWarning(pos, msg, since, siteName(site), siteName(origin), actions)
    def deprecationWarning(pos: Position, origin: Symbol, site: Symbol, msg: String, since: String): Unit =
      deprecationWarning(pos, msg, since, siteName(site), siteName(origin))

    def deprecationWarning(pos: Position, origin: Symbol, site: Symbol): Unit = {
      val version = origin.deprecationVersion.getOrElse("")
      val since   = if (version.isEmpty) version else s" (since $version)"
      val message = origin.deprecationMessage.filter(!_.isEmpty).map(": " + _).getOrElse("")
      deprecationWarning(pos, origin, site, s"${origin.undeprecatedString}${origin.locationString} is deprecated$since$message", version)
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
      val fqname  = s"scala.language.$featureName"
      val explain =
        if (reportedFeature contains featureTrait) ""
        else sm"""|
                  |This can be achieved by adding the import clause 'import $fqname'
                  |or by setting the compiler option -language:$featureName.
                  |See the Scaladoc for value $fqname for a discussion
                  |why the feature $req be explicitly enabled."""
      reportedFeature += featureTrait

      val msg = s"$featureDesc $req be enabled\nby making the implicit value $fqname visible.$explain".replace("#", construct)
      // on postfix error, include interesting infix warning
      def isXfix = featureName == "postfixOps" && suspendedMessages.get(repSrc(pos.source)).map(_.exists(w => pos.includes(w.pos))).getOrElse(false)
      if (required) {
        val amended = if (isXfix) s"$msg\n${suspendedMessages(repSrc(pos.source)).filter(pos includes _.pos).map(_.msg).mkString("\n")}" else msg
        reporter.error(pos, amended)
      } else warning(pos, msg, featureCategory(featureTrait.nameString), site)
    }

    // Used in the optimizer where we don't have symbols, the site string is created from the class internal name and method name.
    def warning(pos: Position, msg: String, category: WarningCategory, site: String, actions: List[CodeAction]): Unit =
      issueIfNotSuppressed(Message.Plain(pos, msg, category, site, actions))
    def warning(pos: Position, msg: String, category: WarningCategory, site: String): Unit =
      warning(pos, msg, category, site, Nil)

    // Preferred over the overload above whenever a site symbol is available
    def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol, actions: List[CodeAction] = Nil): Unit =
      warning(pos, msg, category, siteName(site), actions)

    // Provide an origin for the warning.
    def warning(pos: Position, msg: String, category: WarningCategory, site: Symbol, origin: String): Unit =
      issueIfNotSuppressed(Message.Origin(pos, msg, category, siteName(site), origin, actions = Nil))

    def codeAction(title: String, pos: Position, newText: String, desc: String, expected: Option[(String, CompilationUnit)] = None) =
      CodeAction(title, pos, newText, desc, expected.forall(e => e._1 == e._2.source.sourceAt(pos)))

    // Remember CodeActions that match `-quickfix` and report the error through the reporter
    def error(pos: Position, msg: String, actions: List[CodeAction]): Unit = {
      val quickfixed = {
        if (registerErrorTextEdit(pos, msg, actions)) s"[rewritten by -quickfix] $msg"
        else if (actions.exists(_.edits.nonEmpty) && !settings.quickFixSilent) s"$msg [quickfixable]"
        else msg
      }
      reporter.error(pos, quickfixed, actions)
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

    private object quickfix {
      /** Source code at a position. Either a line with caret (offset), else the code at the range position. */
      def codeOf(pos: Position, source: SourceFile): String =
        if (pos.start < pos.end) new String(source.content.slice(pos.start, pos.end))
        else {
          val line = source.offsetToLine(pos.point)
          val code = source.lines(line).next()
          val caret = " " * (pos.point - source.lineToOffset(line)) + "^"
          s"$code\n$caret"
        }


      def checkNoOverlap(patches: List[TextEdit], source: SourceFile): Boolean = {
        var ok = true
        for (List(p1, p2) <- patches.sliding(2) if p1.position.end > p2.position.start) {
          ok = false
          val msg =
            s"""overlapping quick fixes in ${source.file.file.getAbsolutePath}:
               |
               |add `${p1.newText}` at
               |${codeOf(p1.position, source)}
               |
               |add `${p2.newText}` at
               |${codeOf(p2.position, source)}""".stripMargin.trim
          issueWarning(Message.Plain(p1.position, msg, WarningCategory.Other, "", Nil))
        }
        ok
      }

      def underlyingFile(source: SourceFile): Option[Path] = {
        val fileClass = source.file.getClass.getName
        val p = if (fileClass.endsWith("xsbt.ZincVirtualFile")) {
          import scala.language.reflectiveCalls
          val path = source.file.asInstanceOf[ {def underlying(): {def id(): String}}].underlying().id()
          Some(Paths.get(path))
        } else
          Option(source.file.file).map(_.toPath)
        val r = p.filter(Files.exists(_))
        if (r.isEmpty)
          issueWarning(Message.Plain(NoPosition, s"Failed to apply quick fixes, file does not exist: ${source.file}", WarningCategory.Other, "", Nil))
        r
      }

      val encoding = Charset.forName(settings.encoding.value)

      def insertEdits(sourceChars: Array[Char], edits: List[TextEdit], file: Path): Array[Byte] = {
        val patchedChars = new Array[Char](sourceChars.length + edits.iterator.map(_.delta).sum)
        @tailrec def loop(edits: List[TextEdit], inIdx: Int, outIdx: Int): Unit = {
          def copy(upTo: Int): Int = {
            val untouched = upTo - inIdx
            System.arraycopy(sourceChars, inIdx, patchedChars, outIdx, untouched)
            outIdx + untouched
          }
          edits match {
            case e :: es =>
              val outNew = copy(e.position.start)
              e.newText.copyToArray(patchedChars, outNew)
              loop(es, e.position.end, outNew + e.newText.length)
            case _ =>
              val outNew = copy(sourceChars.length)
              if (outNew != patchedChars.length)
                issueWarning(Message.Plain(NoPosition, s"Unexpected content length when applying quick fixes; verify the changes to ${file.toFile.getAbsolutePath}", WarningCategory.Other, "", Nil))
          }
        }

        loop(edits, 0, 0)
        new String(patchedChars).getBytes(encoding)
      }

      def apply(edits: mutable.Set[TextEdit]): Unit = {
        for ((source, edits) <- edits.groupBy(_.position.source).view.mapValues(_.toList.sortBy(_.position.start))) {
          if (checkNoOverlap(edits, source)) {
            underlyingFile(source) foreach { file =>
              val sourceChars = new String(Files.readAllBytes(file), encoding).toCharArray
              try Files.write(file, insertEdits(sourceChars, edits, file))
              catch {
                case e: IOException =>
                  issueWarning(Message.Plain(NoPosition, s"Failed to apply quick fixes to ${file.toFile.getAbsolutePath}\n${e.getMessage}", WarningCategory.Other, "", Nil))
              }
            }
          }
        }
      }
    }
  }
}

object Reporting {
  sealed trait Message {
    def pos: Position
    def msg: String
    def category: WarningCategory
    def site: String // sym.FullName of the location where the warning is positioned, may be empty
    def actions: List[CodeAction]
  }

  object Message {
    // an ordinary Message has a `category` for filtering and the `site` where it was issued
    final case class Plain(pos: Position, msg: String, category: WarningCategory, site: String, actions: List[CodeAction]) extends Message

    // a Plain message with an `origin` which should not be empty. For example, the origin of an unused import is the fully-qualified selection
    final case class Origin(pos: Position, msg: String, category: WarningCategory, site: String, origin: String, actions: List[CodeAction]) extends Message

    // `site` and `origin` may be empty
    final case class Deprecation(pos: Position, msg: String, site: String, origin: String, since: Version, actions: List[CodeAction]) extends Message {
      def category: WarningCategory = WarningCategory.Deprecation
    }
  }

  sealed class WarningCategory {
    def includes(o: WarningCategory): Boolean = this eq o
    def summaryCategory: WarningCategory = this
    lazy val name: String = WarningCategory.nameOf(this)
  }

  object WarningCategory {
    private val camels = "(?=[A-Z][a-z])".r
    private def hyphenated(s: String): String = camels.split(s).mkString("-").toLowerCase

    private val _all: mutable.Map[String, WarningCategory] = mutable.Map.empty
    def all: collection.Map[String, WarningCategory] = _all

    // Add all WarningCategory members to all, by category name derived from field name.
    private def adderall(): Unit =
      for (f <- getClass.getDeclaredFields if classOf[WarningCategory].isAssignableFrom(f.getType))
        _all.put(hyphenated(f.getName), f.get(this).asInstanceOf[WarningCategory])
            .foreach(_ => throw new AssertionError(s"warning category '${f.getName}' added twice"))

    private def nameOf(w: WarningCategory): String =
      getClass.getDeclaredFields.find(_.get(this) eq w) match {
        case Some(f) => hyphenated(f.getName)
        case _ => hyphenated(w.getClass.getName).toLowerCase
      }

    private def apply(): WarningCategory = new WarningCategory

    // "top-level" categories
    val Deprecation, Unchecked, Optimizer, Scaladoc, JavaSource, Scala3Migration = WarningCategory()

    // miscellaneous warnings that are grouped together in summaries
    sealed class Other extends WarningCategory {
      override def summaryCategory: WarningCategory = Other
    }
    val Other = new Other {
      override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Other]
    }
    private def other(): Other = new Other
    val OtherShadowing,
        OtherPureStatement,
        OtherMigration, // API annotation
        OtherMatchAnalysis,
        OtherDebug,
        OtherNullaryOverride,
        OtherNonCooperativeEquals,
        OtherImplicitType
      = other()

    // categories corresponding to -W settings, such as -Wvalue-discard
    sealed class WFlag extends WarningCategory {
      override def summaryCategory: WarningCategory = WFlag
    }
    val WFlag = new WFlag {
      override def includes(o: WarningCategory): Boolean = o.isInstanceOf[WFlag]
    }
    private def wflag(): WFlag = new WFlag
    val WFlagDeadCode,
        WFlagExtraImplicit,
        WFlagNumericWiden,
        WFlagSelfImplicit,
        WFlagUnnamedBooleanLiteral,
        WFlagValueDiscard
      = wflag()

    sealed class Unused extends WarningCategory {
      override def summaryCategory: WarningCategory = Unused
    }
    val Unused = new Unused {
      override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Unused]
    }
    private def unused(): Unused = new Unused
    val UnusedImports,
        UnusedPatVars,
        UnusedPrivates,
        UnusedLocals,
        UnusedParams,
        UnusedNowarn
      = unused()

    sealed class Lint extends WarningCategory {
      override def summaryCategory: WarningCategory = Lint
    }
    val Lint = new Lint {
      override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Lint]
    }
    private def lint(): Lint = new Lint
    val LintAdaptedArgs,
        LintNullaryUnit,
        LintInaccessible,
        LintInferAny,
        LintMissingInterpolator,
        LintDocDetached,
        LintPrivateShadow,
        LintTypeParameterShadow,
        LintPolyImplicitOverload,
        LintOptionImplicit,
        LintDelayedinitSelect,
        LintPackageObjectClasses,
        LintStarsAlign,
        LintConstant,
        LintNonlocalReturn,
        LintImplicitNotFound,
        LintSerial,
        LintEtaZero,
        LintEtaSam,
        LintDeprecation,
        LintBynameImplicit,
        LintRecurseWithDefault,
        LintUnitSpecialization,
        LintMultiargInfix,
        LintPerformance,
        LintIntDivToFloat,
        LintUniversalMethods,
        LintNumericMethods
      = lint()

    sealed class Feature extends WarningCategory {
      override def summaryCategory: WarningCategory = Feature
    }
    val Feature = new Feature {
      override def includes(o: WarningCategory): Boolean = o.isInstanceOf[Feature]
    }
    private def feature(): Feature = new Feature
    val FeatureDynamics,
        FeatureExistentials,
        FeatureHigherKinds,
        FeatureImplicitConversions,
        FeaturePostfixOps,
        FeatureReflectiveCalls,
        FeatureMacros
      = feature()

    locally {
      adderall()
    }
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
      def check(msg: String) = pattern.findFirstIn(msg).nonEmpty
      def matches(message: Message): Boolean = check(message.msg)
    }

    final case class SitePattern(pattern: Regex) extends MessageFilter {
      def matches(message: Message): Boolean = pattern.matches(message.site)
    }

    final case class SourcePattern(pattern: Regex) extends MessageFilter {
      private[this] val cache = mutable.Map.empty[SourceFile, Boolean]

      def check(pos: Position) = cache.getOrElseUpdate(pos.source, {
        val sourcePath = pos.source.file.canonicalPath.replace("\\", "/")
        pattern.findFirstIn(sourcePath).nonEmpty
      })
      def matches(message: Message): Boolean = check(message.pos)
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
        case Message.Deprecation(_, _, _, _, mv: ParseableVersion, _) =>
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
