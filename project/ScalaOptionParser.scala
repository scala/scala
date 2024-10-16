package scala.build

import ParserUtil._
import sbt._
import sbt.complete.Parser._
import sbt.complete.Parsers._
import sbt.complete._

object ScalaOptionParser {
  /** An sbt parser for the Scala command line runners (scala, scalac, etc) */
  def scalaParser(entryPoint: String, globalBase: File): Parser[String] = {
    def BooleanSetting(name: String): Parser[String] =
      token(name)
    def StringSetting(name: String): Parser[String] = {
      val valueParser = name match {
        case "-d" => JarOrDirectoryParser
        case _ => token(StringBasic, TokenCompletions.displayOnly("<value>"))
      }
      concat(concat(token(name ~ Space.string)) ~ valueParser)
    }
    def MultiStringSetting(name: String): Parser[String] =
      concat(concat(token(name ~ ":")) ~ repsep(token(StringBasicNotStartingWithDash, TokenCompletions.displayOnly("<value>")), token(",")).map(_.mkString))
    def IntSetting(name: String): Parser[String] =
      concat(concat(token(name ~ ":")) ~ token(IntBasic.map(_.toString), TokenCompletions.displayOnly("<integer>")))
    def ChoiceSetting(name: String, choices: List[String]): Parser[String] =
      concat(token(concat(name ~ ":")) ~ token(StringBasic.examples(choices: _*)).map(_.mkString))
    def MultiChoiceSetting(name: String, choices: List[String]): Parser[String] =
      concat(token(concat(name ~ ":")) ~ rep1sep(token(StringBasic.examples(choices: _*)), token(",")).map(_.mkString(",")))
    def PathSetting(name: String): Parser[String] = {
      concat(concat(token(name) ~ Space.string) ~ rep1sep(JarOrDirectoryParser.filter(!_.contains(":"), x => x), token(java.io.File.pathSeparator)).map(_.mkString(java.io.File.pathSeparator)))
    }
    def FileSetting(name: String): Parser[String] = {
      concat(concat(token(name) ~ Space.string) ~ rep1sep(JarOrDirectoryParser.filter(!_.contains(":"), x => x), token(java.io.File.pathSeparator)).map(_.mkString(java.io.File.pathSeparator)))
    }
    val Phase = token(NotSpace.examples(phases: _*))
    def PhaseSettingParser(name: String): Parser[String] = {
      MultiChoiceSetting(name, phases)
    }
    def ScalaVersionSetting(name: String): Parser[String] = {
      concat(concat(token(name ~ ":")) ~ token(StringBasic, TokenCompletions.displayOnly("<scala version>")))
    }
    val Property: Parser[String] = {
      val PropName = concat(token("-D" ~ oneOrMore(NotSpaceClass & not('=', "not =")).string, TokenCompletions.displayOnly("-D<property name>")))
      val EqualsValue = concat("=" ~ token(OptNotSpace, TokenCompletions.displayOnly("<property value>")))
      concat(PropName ~ EqualsValue.?.map(_.getOrElse("")))
    }

    val sourceFile = FileParser(GlobFilter("*.scala") | GlobFilter("*.java"))

    // TODO Allow JVM settings via -J-... and temporarily add them to the ForkOptions
    val UniversalOpt = Property | oneOf(pathSettingNames.map(PathSetting) ++ phaseSettings.map(PhaseSettingParser) ++ booleanSettingNames.map(BooleanSetting) ++ stringSettingNames.map(StringSetting) ++ multiStringSettingNames.map(MultiStringSetting) ++ intSettingNames.map(IntSetting) ++ choiceSettingNames.map { case (k, v) => ChoiceSetting(k, v) } ++ multiChoiceSettingNames.map { case (k, v) => MultiChoiceSetting(k, v) } ++ scalaVersionSettings.map(ScalaVersionSetting))
    val ScalacOpt = sourceFile | UniversalOpt

    val ScalaExtraSettings = oneOf(
      scalaChoiceSettingNames.map { case (k, v) => ChoiceSetting(k,v)}.toList
      ++ scalaStringSettingNames.map(StringSetting)
      ++ scalaBooleanSettingNames.map(BooleanSetting))
    val ScalaOpt = UniversalOpt | ScalaExtraSettings

    val ScalaDocExtraSettings = oneOf(
      scalaDocBooleanSettingNames.map(BooleanSetting)
      ++ scalaDocIntSettingNames.map(IntSetting)
      ++ scalaDocChoiceSettingNames.map { case (k, v) => ChoiceSetting(k, v)}
      ++ scaladocStringSettingNames.map(StringSetting)
      ++ scaladocPathSettingNames.map(PathSetting)
      ++ scaladocMultiStringSettingNames.map(MultiStringSetting)
    )
    val ScalaDocOpt = ScalacOpt | ScalaDocExtraSettings

    val P = entryPoint match {
      case "scala" =>
        val runnable = token(StringBasicNotStartingWithDash, TokenCompletions.displayOnly("<script|class|object|jar>")).filter(!_.startsWith("-"), x => x)
        val runnableAndArgs = concat(runnable ~ Opt(concat(Space.string ~ repsep(token(StringBasic, TokenCompletions.displayOnly("<arg>")), Space).map(_.mkString(" ")))))
        val options = rep1sep(ScalaOpt, Space).map(_.mkString(" "))
        Opt(Space ~> (options | concat(concat(options ~ Space.string) ~ runnableAndArgs) | runnableAndArgs))
      case "scaladoc" =>
        Opt(Space ~> Opt(repsep(ScalaDocOpt, Space).map(_.mkString(" "))))
      case "scalac" =>
        Opt(Space ~> repsep(ScalacOpt, Space).map(_.mkString(" ")))
    }
    P <~ token(OptSpace)
  }

  // TODO retrieve these data programmatically, ala https://github.com/scala/scala-tool-support/blob/master/bash-completion/src/main/scala/BashCompletion.scala
  private def booleanSettingNames = List("-X", "-Xasync", "-Xcheckinit", "-Xdev", "-Xdisable-assertions", "-Xexperimental", "-Xfatal-warnings", "-Xlog-free-terms", "-Xlog-free-types", "-Xlog-implicit-conversions", "-Xlog-reflective-calls",
    "-Xnojline", "-Xno-forwarders", "-Xno-patmat-analysis", "-Xnon-strict-patmat-analysis", "-Xprint-pos", "-Xprint-types", "-Xprompt", "-Xresident", "-Xshow-phases", "-Xverify", "-Y",
    "-Ybreak-cycles", "-Ydebug", "-Ydebug-type-error", "-Ycompact-trees", "-YdisableFlatCpCaching", "-Ydoc-debug",
    "-Yide-debug",
    "-Ylog-classpath", "-Ymacro-debug-lite", "-Ymacro-debug-verbose", "-Ymacro-no-expand",
    "-Yno-completion", "-Yno-generic-signatures", "-Yno-imports", "-Yno-predef", "-Ymacro-annotations",
    "-Ypatmat-debug", "-Yno-adapted-args", "-Ypos-debug", "-Ypresentation-debug",
    "-Ypresentation-strict", "-Ypresentation-verbose", "-Yquasiquote-debug", "-Yrangepos", "-Yreify-copypaste", "-Yreify-debug", "-Yrepl-class-based",
    "-Yrepl-sync", "-Yshow-member-pos", "-Yshow-symkinds", "-Yshow-symowners", "-Yshow-syms", "-Yshow-trees", "-Yshow-trees-compact", "-Yshow-trees-stringified", "-Ytyper-debug",
    "-Ywarn-dead-code", "-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Ywarn-extra-implicit", "-Ywarn-self-implicit",
    "-V",
    "-Vclasspath", "-Vdebug", "-Vdebug-tasty", "-Vdebug-type-error", "-Vdoc", "-Vfree-terms", "-Vfree-types",
    "-Vhot-statistics", "-Vide", "-Vimplicit-conversions", "-Vimplicits", "-Vissue",
    "-Vmacro", "-Vmacro-lite", "-Vpatmat", "-Vphases", "-Vpos", "-Vprint-pos",
    "-Vprint-types", "-Vquasiquote", "-Vreflective-calls", "-Vreify",
    "-Vshow-member-pos", "-Vshow-symkinds", "-Vshow-symowners", "-Vsymbols", "-Vtype-diffs", "-Vtyper",
    "-W",
    "-Wdead-code", "-Werror", "-Wextra-implicit", "-Wnumeric-widen", "-Woctal-literal",
    "-Wvalue-discard", "-Wself-implicit",
    "-deprecation", "-explaintypes", "-feature", "-help", "-no-specialization", "-nobootcp", "-nowarn", "-optimise", "-print", "-unchecked", "-uniqid", "-usejavacp", "-usemanifestcp", "-verbose", "-version")
  private def stringSettingNames = List("-Xjline", "-Xgenerate-phase-graph", "-Xmain-class", "-Xpluginsdir", "-Xshow-class", "-Xshow-object", "-Vshow-object", "-Xsource-reader", "-Ydump-classes", "-Ygen-asmp",
    "-Ypresentation-log", "-Ypresentation-replay", "-Yrepl-outdir", "-d", "-dependencyfile", "-encoding", "-Xscript", "-Vinline", "-Vopt", "-Vshow-class", "-Vshow-member-pos")
  private def pathSettingNames = List("-bootclasspath", "-classpath", "-extdirs", "-javabootclasspath", "-javaextdirs", "-sourcepath", "-toolcp", "-Vprint-args")
  private val phases = List("all", "parser", "namer", "packageobjects", "typer", "patmat", "superaccessors", "extmethods", "pickler", "refchecks", "uncurry", "tailcalls", "specialize", "explicitouter", "erasure", "posterasure", "fields", "lambdalift", "constructors", "flatten", "mixin", "cleanup", "delambdafy", "jvm", "terminal")
  private val phaseSettings = List("-Ystop-after", "-Yskip", "-Yshow", "-Ystop-before",
    "-Ybrowse", "-Ylog", "-Ycheck", "-Xprint", "-Yvalidate-pos", "-Vbrowse", "-Vlog", "-Vprint", "-Vshow")
  private def multiStringSettingNames = List("-Xmacro-settings", "-Xplugin", "-Xplugin-disable", "-Xplugin-require", "-Ywarn-unused", "-opt-inline-from")
  private def intSettingNames = List("-Xelide-below", "-Ypatmat-exhaust-depth", "-Ypresentation-delay", "-Yrecursion")
  private def choiceSettingNames = Map[String, List[String]](
    "-YclasspathImpl" -> List("flat", "recursive"),
    "-Ydelambdafy" -> List("inline", "method"),
    "-Ymacro-expand" -> List("discard", "none"),
    "-Yresolve-term-conflict" -> List("error", "object", "package"),
    "-g" -> List("line", "none", "notailcails", "source", "vars"),
    "-target" -> targetSettingNames)
  private def multiChoiceSettingNames = Map[String, List[String]](
    "-Xlint" -> List("adapted-args", "nullary-unit", "inaccessible", "nullary-override", "infer-any", "missing-interpolator", "doc-detached", "private-shadow", "type-parameter-shadow", "poly-implicit-overload", "option-implicit", "delayedinit-select", "package-object-classes", "stars-align", "strict-unsealed-patmat", "constant", "unused", "eta-zero"),
    "-language" -> List("help", "_", "dynamics", "postfixOps", "reflectiveCalls", "implicitConversions", "higherKinds", "existentials", "experimental.macros"),
    "-opt" -> List("unreachable-code", "simplify-jumps", "compact-locals", "copy-propagation", "redundant-casts", "box-unbox", "nullness-tracking", "closure-invocations" , "allow-skip-core-module-init", "assume-modules-non-null", "allow-skip-class-loading", "inline", "l:none", "l:default", "l:method", "l:inline", "l:project", "l:classpath"),
    "-Ywarn-unused" -> List("imports", "patvars", "privates", "locals", "explicits", "implicits", "params"),
    "-Ywarn-macros" -> List("none", "before", "after", "both"),
    "-Ystatistics" -> List("parser", "typer", "patmat", "erasure", "cleanup", "jvm"),
    "-Vstatistics" -> List("parser", "typer", "patmat", "erasure", "cleanup", "jvm"),
    "-Wunused" -> List("imports", "patvars", "privates", "locals", "explicits", "implicits", "nowarn", "linted")
  )
  private def scalaVersionSettings = List("-Xmigration", "-Xsource")

  private def scalaChoiceSettingNames = Map("-howtorun" -> List("object", "script", "jar", "guess"))
  private def scalaStringSettingNames = List("-i", "-e")
  private def scalaBooleanSettingNames = List("-nc", "-save")

  private def scalaDocBooleanSettingNames = List("-implicits", "-implicits-debug", "-implicits-show-all", "-implicits-sound-shadowing", "-implicits-hide", "-author", "-diagrams", "-diagrams-debug", "-raw-output", "-no-prefixes", "-no-link-warnings", "-expand-all-types", "-groups")
  private def scalaDocIntSettingNames = List("-diagrams-max-classes", "-diagrams-max-implicits", "-diagrams-dot-timeout", "-diagrams-dot-restart")
  private def scalaDocChoiceSettingNames = Map("-doc-format" -> List("html"))
  private def scaladocStringSettingNames = List("-doc-title", "-doc-version", "-doc-footer", "-doc-no-compile", "-doc-source-url", "-doc-generator", "-skip-packages", "-jdk-api-doc-base")
  private def scaladocPathSettingNames = List("-doc-root-content", "-diagrams-dot-path")
  private def scaladocMultiStringSettingNames = List("-doc-external-doc")

  private val targetSettingNames = (8 to 24).map(_.toString).flatMap(v => v :: s"jvm-1.$v" :: s"jvm-$v" :: s"1.$v" :: Nil).toList
}
