/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala
package tools
package nsc
package settings

import java.util.zip.Deflater

import scala.language.existentials
import scala.annotation.elidable
import scala.tools.util.PathResolver.Defaults
import scala.collection.mutable

trait ScalaSettings extends AbsScalaSettings
                       with StandardScalaSettings
                       with Warnings {
  self: MutableSettings =>

  /** Set of settings */
  protected[scala] lazy val allSettings = mutable.HashSet[Setting]()

  /** The user class path, specified by `-classpath` or `-cp`,
   *  defaults to the value of CLASSPATH env var if it is set, as in Java,
   *  or else to `"."` for the current user directory.
   */
  protected def defaultClasspath = sys.env.getOrElse("CLASSPATH", ".")

  /** Enabled under -Xexperimental. */
  protected def experimentalSettings = List[BooleanSetting](YpartialUnification)

  /** Enabled under -Xfuture. */
  protected def futureSettings = List[BooleanSetting]()

  /** If any of these settings is enabled, the compiler should print a message and exit.  */
  def infoSettings = List[Setting](version, help, Xhelp, Yhelp, showPlugins, showPhases, genPhaseGraph, printArgs)

  /** Is an info setting set? Any -option:help? */
  def isInfo = infoSettings.exists(_.isSetByUser) || allSettings.exists(_.isHelping)

  /** Disable a setting */
  def disable(s: Setting) = allSettings -= s

  val jvmargs  = PrefixSetting("-J<flag>", "-J", "Pass <flag> directly to the runtime system.")
  val defines  = PrefixSetting("-Dproperty=value", "-D", "Pass -Dproperty=value directly to the runtime system.")
  /*val toolcp =*/ PathSetting("-toolcp", "Add to the runner classpath.", "")
  val nobootcp = BooleanSetting("-nobootcp", "Do not use the boot classpath for the scala jars.")

  /**
   *  Standard settings
   */
  // argfiles is only for the help message
  /*val argfiles = */ BooleanSetting    ("@<file>", "A text file containing compiler arguments (options and source files)")
  val classpath     = PathSetting       ("-classpath", "Specify where to find user class files.", defaultClasspath) withAbbreviation "-cp"
  val d             = OutputSetting     (outputDirs, ".")
  val nospecialization = BooleanSetting ("-no-specialization", "Ignore @specialize annotations.")

  // Would be nice to build this dynamically from scala.languageFeature.
  // The two requirements: delay error checking until you have symbols, and let compiler command build option-specific help.
  object languageFeatures extends MultiChoiceEnumeration {
    val dynamics            = Choice("dynamics",            "Allow direct or indirect subclasses of scala.Dynamic")
    val postfixOps          = Choice("postfixOps",          "Allow postfix operator notation, such as `1 to 10 toList'")
    val reflectiveCalls     = Choice("reflectiveCalls",     "Allow reflective access to members of structural types")
    val implicitConversions = Choice("implicitConversions", "Allow definition of implicit functions called views")
    val higherKinds         = Choice("higherKinds",         "Allow higher-kinded types")
    val existentials        = Choice("existentials",        "Existential types (besides wildcard types) can be written and inferred")
    val macros              = Choice("experimental.macros", "Allow macro definition (besides implementation and application)")
  }
  val language      = {
    val description = "Enable or disable language features"
    MultiChoiceSetting(
      name    = "-language",
      helpArg = "feature",
      descr   = description,
      domain  = languageFeatures
    )
  }
  val release = StringSetting("-release", "<release>", "Compile for a specific version of the Java platform. Supported targets: 6, 7, 8, 9", "").withPostSetHook { (value: StringSetting) =>
    if (value.value != "" && !scala.util.Properties.isJavaAtLeast("9")) {
      errorFn.apply("-release is only supported on Java 9 and higher")
    } else {
      // TODO validate numeric value
      // TODO validate release <= java.specification.version
    }
  }
  def releaseValue: Option[String] = Option(release.value).filter(_ != "")

  /*
   * The previous "-source" option is intended to be used mainly
   * though this helper.
   */
  private[this] val version211 = ScalaVersion("2.11.0")
  def isScala211: Boolean = source.value >= version211
  private[this] val version212 = ScalaVersion("2.12.0")
  def isScala212: Boolean = source.value >= version212
  private[this] val version213 = ScalaVersion("2.13.0")
  def isScala213: Boolean = source.value >= version213

  /**
   * -X "Advanced" settings
   */
  val Xhelp              = BooleanSetting      ("-X", "Print a synopsis of advanced options.")
  val checkInit          = BooleanSetting      ("-Xcheckinit", "Wrap field accessors to throw an exception on uninitialized access.")
  val developer          = BooleanSetting      ("-Xdev", "Indicates user is a developer - issue warnings about anything which seems amiss")
  val noassertions       = BooleanSetting      ("-Xdisable-assertions", "Generate no assertions or assumptions.") andThen (flag =>
                                                if (flag) elidebelow.value = elidable.ASSERTION + 1)
  val elidebelow         = IntSetting          ("-Xelide-below", "Calls to @elidable methods are omitted if method priority is lower than argument",
                                                elidable.MINIMUM, None, elidable.byName get _)
  val noForwarders       = BooleanSetting      ("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val genPhaseGraph      = StringSetting       ("-Xgenerate-phase-graph", "file", "Generate the phase graphs (outputs .dot files) to fileX.dot.", "")
  val XlogImplicits      = BooleanSetting      ("-Xlog-implicits", "Show more detail on why some implicits are not applicable.")
  val logImplicitConv    = BooleanSetting      ("-Xlog-implicit-conversions", "Print a message whenever an implicit conversion is inserted.")
  val logReflectiveCalls = BooleanSetting      ("-Xlog-reflective-calls", "Print a message when a reflective method call is generated")
  val logFreeTerms       = BooleanSetting      ("-Xlog-free-terms", "Print a message when reification creates a free term.")
  val logFreeTypes       = BooleanSetting      ("-Xlog-free-types", "Print a message when reification resorts to generating a free type.")
  val maxClassfileName   = IntSetting          ("-Xmax-classfile-name", "Maximum filename length for generated classes", 255, Some((72, 255)), _ => None)
  val maxerrs            = IntSetting          ("-Xmaxerrs", "Maximum errors to print", 100, None, _ => None)
  val maxwarns           = IntSetting          ("-Xmaxwarns", "Maximum warnings to print", 100, None, _ => None)
  val Xmigration         = ScalaVersionSetting ("-Xmigration", "version", "Warn about constructs whose behavior may have changed since version.", initial = NoScalaVersion, default = Some(AnyScalaVersion))
  val nouescape          = BooleanSetting      ("-Xno-uescape", "Disable handling of \\u unicode escapes.")
  val Xnojline           = BooleanSetting      ("-Xnojline", "Do not use JLine for editing.")
  val Xverify            = BooleanSetting      ("-Xverify", "Verify generic signatures in generated bytecode.")
  val plugin             = MultiStringSetting  ("-Xplugin", "paths", "Load a plugin from each classpath.")
  val disable            = MultiStringSetting  ("-Xplugin-disable", "plugin", "Disable plugins by name.")
  val showPlugins        = BooleanSetting      ("-Xplugin-list", "Print a synopsis of loaded plugins.")
  val require            = MultiStringSetting  ("-Xplugin-require", "plugin", "Abort if a named plugin is not loaded.")
  val pluginsDir         = StringSetting       ("-Xpluginsdir", "path", "Path to search for plugin archives.", Defaults.scalaPluginPath)
  val Xprint             = PhasesSetting       ("-Xprint", "Print out program after")
  val Xprintpos          = BooleanSetting      ("-Xprint-pos", "Print tree positions, as offsets.")
  val printtypes         = BooleanSetting      ("-Xprint-types", "Print tree types (debugging option).")
  val printArgs          = BooleanSetting      ("-Xprint-args", "Print all compiler arguments and exit.")
  val prompt             = BooleanSetting      ("-Xprompt", "Display a prompt after each error (debugging option).")
  val resident           = BooleanSetting      ("-Xresident", "Compiler stays resident: read source filenames from standard input.")
  val script             = StringSetting       ("-Xscript", "object", "Treat the source file as a script and wrap it in a main method.", "")
  val mainClass          = StringSetting       ("-Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val Xshowcls           = StringSetting       ("-Xshow-class", "class", "Show internal representation of class.", "")
  val Xshowobj           = StringSetting       ("-Xshow-object", "object", "Show internal representation of object.", "")
  val showPhases         = BooleanSetting      ("-Xshow-phases", "Print a synopsis of compiler phases.")
  val sourceReader       = StringSetting       ("-Xsource-reader", "classname", "Specify a custom method for reading source files.", "")
  val reporter           = StringSetting       ("-Xreporter", "classname", "Specify a custom reporter for compiler messages.", "scala.tools.nsc.reporters.ConsoleReporter")
  val strictInference    = BooleanSetting      ("-Xstrict-inference", "Don't infer known-unsound types")
  val source             = ScalaVersionSetting ("-Xsource", "version", "Treat compiler input as Scala source for the specified version, see scala/bug#8126.", initial = ScalaVersion("2.12"))

  val XnoPatmatAnalysis = BooleanSetting ("-Xno-patmat-analysis", "Don't perform exhaustivity/unreachability analysis. Also, ignore @switch annotation.")
  val XfullLubs         = BooleanSetting ("-Xfull-lubs", "Retains pre 2.10 behavior of less aggressive truncation of least upper bounds.")

  val XmixinForceForwarders = ChoiceSetting(
    name    = "-Xmixin-force-forwarders",
    helpArg = "mode",
    descr   = "Generate forwarder methods in classes inhering concrete methods from traits.",
    choices = List("true", "junit", "false"),
    default = "true",
    choicesHelp = List(
      "Always generate mixin forwarders.",
      "Generate mixin forwarders for JUnit-annotated methods (JUnit 4 does not support default methods).",
      "Only generate mixin forwarders required for program correctness."))

  object mixinForwarderChoices {
    def isTruthy = XmixinForceForwarders.value == "true"
    def isAtLeastJunit = isTruthy || XmixinForceForwarders.value == "junit"
  }

  // XML parsing options
  object XxmlSettings extends MultiChoiceEnumeration {
    val coalescing   = Choice("coalescing", "Convert PCData to Text and coalesce sibling nodes")
    def isCoalescing = Xxml contains coalescing
  }
  val Xxml = MultiChoiceSetting(
    name    = "-Xxml",
    helpArg = "property",
    descr   = "Configure XML parsing.",
    domain  = XxmlSettings
  )

  /** Compatibility stubs for options whose value name did
   *  not previously match the option name.
   */
  def debuginfo = g
  def dependenciesFile = dependencyfile
  def nowarnings = nowarn
  def outdir = d
  def printLate = print

  /**
   * -Y "Private" settings
   */
  val overrideObjects = BooleanSetting    ("-Yoverride-objects", "Allow member objects to be overridden.")
  val overrideVars    = BooleanSetting    ("-Yoverride-vars", "Allow vars to be overridden.")
  val Yhelp           = BooleanSetting    ("-Y", "Print a synopsis of private options.")
  val breakCycles     = BooleanSetting    ("-Ybreak-cycles", "Attempt to break cycles encountered during typing")
  val browse          = PhasesSetting     ("-Ybrowse", "Browse the abstract syntax tree after")
  val check           = PhasesSetting     ("-Ycheck", "Check the tree at the end of")
  val Yshow           = PhasesSetting     ("-Yshow", "(Requires -Xshow-class or -Xshow-object) Show after")
  val Ycompacttrees   = BooleanSetting    ("-Ycompact-trees", "Use compact tree printer when displaying trees.")
  val noCompletion    = BooleanSetting    ("-Yno-completion", "Disable tab-completion in the REPL.")
  val debug           = BooleanSetting    ("-Ydebug", "Increase the quantity of debugging output.")
  val termConflict    = ChoiceSetting     ("-Yresolve-term-conflict", "strategy", "Resolve term conflicts.", List("package", "object", "error"), "error")
  val log             = PhasesSetting     ("-Ylog", "Log operations during")
  val Ylogcp          = BooleanSetting    ("-Ylog-classpath", "Output information about what classpath is being applied.")
  val Ynogenericsig   = BooleanSetting    ("-Yno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val noimports       = BooleanSetting    ("-Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
  val nopredef        = BooleanSetting    ("-Yno-predef", "Compile without importing Predef.")
  val noAdaptedArgs   = BooleanSetting    ("-Yno-adapted-args", "Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.")
  val Yrecursion      = IntSetting        ("-Yrecursion", "Set recursion depth used when locking symbols.", 0, Some((0, Int.MaxValue)), (_: String) => None)
  val Xshowtrees      = BooleanSetting    ("-Yshow-trees", "(Requires -Xprint:) Print detailed ASTs in formatted form.")
  val XshowtreesCompact
                      = BooleanSetting    ("-Yshow-trees-compact", "(Requires -Xprint:) Print detailed ASTs in compact form.")
  val XshowtreesStringified
                      = BooleanSetting    ("-Yshow-trees-stringified", "(Requires -Xprint:) Print stringifications along with detailed ASTs.")
  val Yshowsyms       = BooleanSetting    ("-Yshow-syms", "Print the AST symbol hierarchy after each phase.")
  val Yshowsymkinds   = BooleanSetting    ("-Yshow-symkinds", "Print abbreviated symbol kinds next to symbol names.")
  val Yshowsymowners  = BooleanSetting    ("-Yshow-symowners", "Print owner identifiers next to symbol names.")
  val skip            = PhasesSetting     ("-Yskip", "Skip")
  val Ygenasmp        = StringSetting     ("-Ygen-asmp",  "dir", "Generate a parallel output directory of .asmp files (ie ASM Textifier output).", "")
  val Ydumpclasses    = StringSetting     ("-Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val stopAfter       = PhasesSetting     ("-Ystop-after", "Stop after") withAbbreviation ("-stop") // backward compat
  val stopBefore      = PhasesSetting     ("-Ystop-before", "Stop before")
  val Yrangepos       = BooleanSetting    ("-Yrangepos", "Use range positions for syntax trees.")
  val Ymemberpos      = StringSetting     ("-Yshow-member-pos", "output style", "Show start and end positions of members", "") withPostSetHook (_ => Yrangepos.value = true)
  val Yreifycopypaste = BooleanSetting    ("-Yreify-copypaste", "Dump the reified trees in copypasteable representation.")
  val Ymacroexpand    = ChoiceSetting     ("-Ymacro-expand", "policy", "Control expansion of macros, useful for scaladoc and presentation compiler.", List(MacroExpand.Normal, MacroExpand.None, MacroExpand.Discard), MacroExpand.Normal)
  val Ymacronoexpand  = BooleanSetting    ("-Ymacro-no-expand", "Don't expand macros. Might be useful for scaladoc and presentation compiler, but will crash anything which uses macros and gets past typer.") withDeprecationMessage(s"Use ${Ymacroexpand.name}:${MacroExpand.None}") withPostSetHook(_ => Ymacroexpand.value = MacroExpand.None)
  val Yreplsync       = BooleanSetting    ("-Yrepl-sync", "Do not use asynchronous code for repl startup")
  val Yreplclassbased = BooleanSetting    ("-Yrepl-class-based", "Use classes to wrap REPL snippets instead of objects")
  val Yreploutdir     = StringSetting     ("-Yrepl-outdir", "path", "Write repl-generated classfiles to given output directory (use \"\" to generate a temporary dir)" , "")
  val YmethodInfer    = BooleanSetting    ("-Yinfer-argument-types", "Infer types for arguments of overridden methods.")
  val YdisableFlatCpCaching  = BooleanSetting    ("-YdisableFlatCpCaching", "Do not cache flat classpath representation of classpath elements from jars across compiler instances.")
  val YcachePluginClassLoader  = CachePolicy.setting("plugin", "compiler plugins")
  val YcacheMacroClassLoader   = CachePolicy.setting("macro", "macros")
  val YpartialUnification = BooleanSetting ("-Ypartial-unification", "Enable partial unification in type constructor inference")
  val Yvirtpatmat     = BooleanSetting    ("-Yvirtpatmat", "Enable pattern matcher virtualization")

  val exposeEmptyPackage = BooleanSetting ("-Yexpose-empty-package", "Internal only: expose the empty package.").internalOnly()
  val Ydelambdafy        = ChoiceSetting  ("-Ydelambdafy", "strategy", "Strategy used for translating lambdas into JVM code.", List("inline", "method"), "method")

  val YaddBackendThreads = IntSetting   ("-Ybackend-parallelism", "maximum worker threads for backend", 1, Some((1,16)), (x: String) => None )
  val YmaxQueue = IntSetting   ("-Ybackend-worker-queue", "backend threads worker queue size", 0, Some((0,1000)), (x: String) => None )
  val YmaxImplicitCycleDepth = IntSetting("-Ymax-implicit-cycle-depth", "Fail with divergence on implicit resolution cycles > this depth", 0, Some((0,1000)), (x: String) => None )
  val YjarCompressionLevel = IntSetting("-Yjar-compression-level", "compression level to use when writing jar files",
    Deflater.DEFAULT_COMPRESSION, Some((Deflater.DEFAULT_COMPRESSION,Deflater.BEST_COMPRESSION)), (x: String) => None)

  sealed abstract class CachePolicy(val name: String, val help: String)
  object CachePolicy {
    def setting(style: String, styleLong: String) = ChoiceSetting(s"-Ycache-$style-class-loader", "policy", s"Policy for caching class loaders for $styleLong that are dynamically loaded.", values.map(_.name), None.name, values.map(_.help))
    object None extends CachePolicy("none", "Don't cache class loader")
    object LastModified extends CachePolicy("last-modified", "Cache class loader, using file last-modified time to invalidate")
    // TODO Jorge to add new policy. Think about whether there is a benefit to the user on offering this as a separate policy or unifying with the previous one.
    // object ZipMetadata extends CachePolicy("zip-metadata", "Cache classloade, using file last-modified time, then ZIP file metadata to invalidate")
    def values: List[CachePolicy] = List(None, LastModified)
  }

  object optChoices extends MultiChoiceEnumeration {
    val unreachableCode         = Choice("unreachable-code",          "Eliminate unreachable code, exception handlers guarding no instructions, redundant metadata (debug information, line numbers).")
    val simplifyJumps           = Choice("simplify-jumps",            "Simplify branching instructions, eliminate unnecessary ones.")
    val compactLocals           = Choice("compact-locals",            "Eliminate empty slots in the sequence of local variables.")
    val copyPropagation         = Choice("copy-propagation",          "Eliminate redundant local variables and unused values (including closures). Enables unreachable-code.")
    val redundantCasts          = Choice("redundant-casts",           "Eliminate redundant casts using a type propagation analysis.")
    val boxUnbox                = Choice("box-unbox",                 "Eliminate box-unbox pairs within the same method (also tuples, xRefs, value class instances). Enables unreachable-code.")
    val nullnessTracking        = Choice("nullness-tracking",         "Track nullness / non-nullness of local variables and apply optimizations.")
    val closureInvocations      = Choice("closure-invocations" ,      "Rewrite closure invocations to the implementation method.")
    val inline                  = Choice("inline",                    "Inline method invocations according to -Yopt-inline-heuristics and -opt-inline-from.")

    // note: unlike the other optimizer levels, "l:none" appears up in the `opt.value` set because it's not an expanding option (expandsTo is empty)
    val lNone = Choice("l:none",
      "Disable optimizations. Takes precedence: `-opt:l:none,+box-unbox` / `-opt:l:none -opt:box-unbox` don't enable box-unbox.")

    private val defaultChoices = List(unreachableCode)
    val lDefault = Choice(
      "l:default",
      "Enable default optimizations: " + defaultChoices.mkString("", ",", "."),
      expandsTo = defaultChoices)

    private val methodChoices = List(unreachableCode, simplifyJumps, compactLocals, copyPropagation, redundantCasts, boxUnbox, nullnessTracking, closureInvocations)
    val lMethod = Choice(
      "l:method",
      "Enable intra-method optimizations: " + methodChoices.mkString("", ",", "."),
      expandsTo = methodChoices)

    private val inlineChoices = List(lMethod, inline)
    val lInline = Choice("l:inline",
      "Enable cross-method optimizations (note: inlining requires -opt-inline-from): " + inlineChoices.mkString("", ",", "."),
      expandsTo = inlineChoices)

    val lProject = Choice(
      "l:project",
      "[deprecated, use -opt:l:inline, -opt-inline-from] Enable cross-method optimizations within the current project.")

    val lClasspath = Choice(
      "l:classpath",
      "[deprecated, use -opt:l:inline, -opt-inline-from] Enable cross-method optimizations across the entire classpath.")
  }

  // We don't use the `default` parameter of `MultiChoiceSetting`: it specifies the default values
  // when `-opt` is passed without explicit choices. When `-opt` is not explicitly specified, the
  // set `opt.value` is empty.
  val opt = MultiChoiceSetting(
    name = "-opt",
    helpArg = "optimization",
    descr = "Enable optimizations",
    domain = optChoices).withPostSetHook(s => {
    import optChoices._
    if (!s.value.contains(inline) && (s.value.contains(lProject) || s.value.contains(lClasspath)))
        s.enable(lInline)
    })

  private def optEnabled(choice: optChoices.Choice) = {
    !opt.contains(optChoices.lNone) && {
      opt.contains(choice) ||
      !opt.isSetByUser && optChoices.lDefault.expandsTo.contains(choice)
    }
  }

  def optNone                    = opt.contains(optChoices.lNone)
  def optUnreachableCode         = optEnabled(optChoices.unreachableCode)
  def optSimplifyJumps           = optEnabled(optChoices.simplifyJumps)
  def optCompactLocals           = optEnabled(optChoices.compactLocals)
  def optCopyPropagation         = optEnabled(optChoices.copyPropagation)
  def optRedundantCasts          = optEnabled(optChoices.redundantCasts)
  def optBoxUnbox                = optEnabled(optChoices.boxUnbox)
  def optNullnessTracking        = optEnabled(optChoices.nullnessTracking)
  def optClosureInvocations      = optEnabled(optChoices.closureInvocations)
  def optInlinerEnabled          = optEnabled(optChoices.inline)

  // deprecated inliner levels
  def optLProject                = optEnabled(optChoices.lProject)
  def optLClasspath              = optEnabled(optChoices.lClasspath)

  def optBuildCallGraph          = optInlinerEnabled || optClosureInvocations
  def optAddToBytecodeRepository = optBuildCallGraph || optInlinerEnabled || optClosureInvocations

  val optInlineFrom = MultiStringSetting(
    "-opt-inline-from",
    "patterns",
    "Patterns for classfile names from which to allow inlining, `help` for details.",
    helpText = Some(
      """Patterns for classfile names from which the inliner is allowed to pull in code.
        |  *              Matches classes in the empty package
        |  **             All classes
        |  a.C            Class a.C
        |  a.*            Classes in package a
        |  a.**           Classes in a and in sub-packages of a
        |  **.Util        Classes named Util in any package (including the empty package)
        |  a.**.*Util*    Classes in a and sub-packages with Util in their name (including a.Util)
        |  a.C$D          The nested class D defined in class a.C
        |  scala.Predef$  The scala.Predef object
        |  <sources>      Classes defined in source files compiled in the current compilation, either
        |                 passed explicitly to the compiler or picked up from the `-sourcepath`
        |
        |The setting accepts a list of patterns: `-opt-inline-from:p1:p2`. The setting can be passed
        |multiple times, the list of patterns gets extended. A leading `!` marks a pattern excluding.
        |The last matching pattern defines whether a classfile is included or excluded (default: excluded).
        |For example, `a.**:!a.b.**` includes classes in a and sub-packages, but not in a.b and sub-packages.
        |
        |Note: on the command-line you might need to quote patterns containing `*` to prevent the shell
        |from expanding it to a list of files in the current directory.""".stripMargin))

  val YoptInlineHeuristics = ChoiceSetting(
    name = "-Yopt-inline-heuristics",
    helpArg = "strategy",
    descr = "Set the heuristics for inlining decisions.",
    choices = List("at-inline-annotated", "everything", "default"),
    default = "default")

  object optWarningsChoices extends MultiChoiceEnumeration {
    val none                               = Choice("none"                       , "No optimizer warnings.")
    val atInlineFailedSummary              = Choice("at-inline-failed-summary"   , "One-line summary if there were @inline method calls that could not be inlined.")
    val atInlineFailed                     = Choice("at-inline-failed"           , "A detailed warning for each @inline method call that could not be inlined.")
    val anyInlineFailed                    = Choice("any-inline-failed"          , "A detailed warning for every callsite that was chosen for inlining by the heuristics, but could not be inlined.")
    val noInlineMixed                      = Choice("no-inline-mixed"            , "In mixed compilation, warn at callsites methods defined in java sources (the inlining decision cannot be made without bytecode).")
    val noInlineMissingBytecode            = Choice("no-inline-missing-bytecode" , "Warn if an inlining decision cannot be made because a the bytecode of a class or member cannot be found on the compilation classpath.")
    val noInlineMissingScalaInlineInfoAttr = Choice("no-inline-missing-attribute", "Warn if an inlining decision cannot be made because a Scala classfile does not have a ScalaInlineInfo attribute.")
  }

  val optWarnings = MultiChoiceSetting(
    name = "-opt-warnings",
    helpArg = "warning",
    descr = "Enable optimizer warnings",
    domain = optWarningsChoices,
    default = Some(List(optWarningsChoices.atInlineFailed.name)))

  def optWarningsSummaryOnly = optWarnings.value subsetOf Set(optWarningsChoices.none, optWarningsChoices.atInlineFailedSummary)

  def optWarningEmitAtInlineFailed =
    !optWarnings.isSetByUser ||
      optWarnings.contains(optWarningsChoices.atInlineFailedSummary) ||
      optWarnings.contains(optWarningsChoices.atInlineFailed) ||
      optWarnings.contains(optWarningsChoices.anyInlineFailed)

  def optWarningNoInlineMixed                      = optWarnings.contains(optWarningsChoices.noInlineMixed)
  def optWarningNoInlineMissingBytecode            = optWarnings.contains(optWarningsChoices.noInlineMissingBytecode)
  def optWarningNoInlineMissingScalaInlineInfoAttr = optWarnings.contains(optWarningsChoices.noInlineMissingScalaInlineInfoAttr)

  val YoptTrace = StringSetting("-Yopt-trace", "package/Class.method", "Trace the optimizer progress for methods; `_` to print all, prefix match to select.", "")

  val YoptLogInline = StringSetting("-Yopt-log-inline", "package/Class.method", "Print a summary of inliner activity; `_` to print all, prefix match to select.", "")

  import scala.reflect.internal.util.Statistics
  val Ystatistics = PhasesSetting("-Ystatistics", "Print compiler statistics for specific phases", "parser,typer,patmat,erasure,cleanup,jvm")
  override def YstatisticsEnabled = Ystatistics.value.nonEmpty

  val YhotStatistics = BooleanSetting("-Yhot-statistics-enabled", s"Enable `${Ystatistics.name}` to print hot statistics.")
  override def YhotStatisticsEnabled = YhotStatistics.value

  val YprofileEnabled = BooleanSetting("-Yprofile-enabled", "Enable profiling.")
  val YprofileDestination = StringSetting("-Yprofile-destination", "file", "where to send profiling output - specify a file, default is to the console.", "").
    withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileExternalTool = PhasesSetting("-Yprofile-external-tool", "Enable profiling for a phase using an external tool hook. Generally only useful for a single phase", "typer").
    withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileRunGcBetweenPhases = PhasesSetting("-Yprofile-run-gc", "Run a GC between phases - this allows heap size to be accurate at the expense of more time. Specify a list of phases, or all", "_").
    withPostSetHook( _ => YprofileEnabled.value = true )



  /** Area-specific debug output.
   */
  val Ydocdebug               = BooleanSetting("-Ydoc-debug", "Trace all scaladoc activity.")
  val Yidedebug               = BooleanSetting("-Yide-debug", "Generate, validate and output trees using the interactive compiler.")
  val Yissuedebug             = BooleanSetting("-Yissue-debug", "Print stack traces when a context issues an error.")
  val YmacrodebugLite         = BooleanSetting("-Ymacro-debug-lite", "Trace essential macro-related activities.")
  val YmacrodebugVerbose      = BooleanSetting("-Ymacro-debug-verbose", "Trace all macro-related activities: compilation, generation of synthetics, classloading, expansion, exceptions.")
  val Yposdebug               = BooleanSetting("-Ypos-debug", "Trace position validation.")
  val Yreifydebug             = BooleanSetting("-Yreify-debug", "Trace reification.")
  val Ytyperdebug             = BooleanSetting("-Ytyper-debug", "Trace all type assignments.")
  val Ypatmatdebug            = BooleanSetting("-Ypatmat-debug", "Trace pattern matching translation.")
  val YpatmatExhaustdepth     = IntSetting("-Ypatmat-exhaust-depth", "off", 20, Some((10, Int.MaxValue)),
    str => Some(if(str.equalsIgnoreCase("off")) Int.MaxValue else str.toInt))
  val Yquasiquotedebug        = BooleanSetting("-Yquasiquote-debug", "Trace quasiquote-related activities.")

  /** Groups of Settings.
   */
  val future        = BooleanSetting("-Xfuture", "Turn on future language features.") enablingIfNotSetByUser futureSettings
  val optimise      = BooleanSetting("-optimise", "Compiler flag for the optimizer in Scala 2.11")
    .withAbbreviation("-optimize")
    .withDeprecationMessage("In 2.12, -optimise enables -opt:l:inline -opt-inline-from:**. Check -opt:help for using the Scala 2.12 optimizer.")
    .withPostSetHook(_ => {
      opt.enable(optChoices.lInline)
      optInlineFrom.value = List("**")
    })
  val Xexperimental = BooleanSetting("-Xexperimental", "Enable experimental extensions.") enablingIfNotSetByUser experimentalSettings

  // Feature extensions
  val XmacroSettings          = MultiStringSetting("-Xmacro-settings", "option", "Custom settings for macros.")

  /**
   * IDE-specific settings
   */
  val YpresentationVerbose = BooleanSetting("-Ypresentation-verbose", "Print information about presentation compiler tasks.")
  val YpresentationDebug   = BooleanSetting("-Ypresentation-debug",  "Enable debugging output for the presentation compiler.")
  val YpresentationAnyThread  = BooleanSetting("-Ypresentation-any-thread", "Allow use of the presentation compiler from any thread")
  val YpresentationStrict     = BooleanSetting("-Ypresentation-strict", "Do not report type errors in sources with syntax errors.")
  val YpresentationLog     = StringSetting("-Ypresentation-log", "file", "Log presentation compiler events into file", "")
  val YpresentationReplay  = StringSetting("-Ypresentation-replay", "file", "Replay presentation compiler events from file", "")
  val YpresentationDelay   = IntSetting("-Ypresentation-delay", "Wait number of ms after typing before starting typechecking", 0, Some((0, 999)), str => Some(str.toInt))

  /**
   * -P "Plugin" settings
   */
  val pluginOptions = MultiStringSetting("-P", "plugin:opt", "Pass an option to a plugin") .
                        withHelpSyntax("-P:<plugin>:<opt>")

  /** Test whether this is scaladoc we're looking at */
  def isScaladoc = false

  object MacroExpand {
    val None = "none"
    val Normal = "normal"
    val Discard = "discard"
  }

  def conflictWarning: Option[String] = {
    // See cd878232b5 for an example how to warn about conflicting settings

    /*
    def checkSomeConflict: Option[String] = ...

    List(/* checkSomeConflict, ... */).flatten match {
      case Nil => None
      case warnings => Some("Conflicting compiler settings were detected. Some settings will be ignored.\n" + warnings.mkString("\n"))
    }
    */

    if (opt.value.contains(optChoices.lProject))
      Some("-opt:l:project is deprecated, use -opt:l:inline and -opt-inline-from")
    else if (opt.value.contains(optChoices.lClasspath))
      Some("-opt:l:classpath is deprecated, use -opt:l:inline and -opt-inline-from")
    else
      None
  }
}
