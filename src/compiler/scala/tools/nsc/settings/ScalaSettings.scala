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
import scala.reflect.internal.util.StringContextStripMarginOps
import scala.tools.nsc.util.DefaultJarFactory


trait ScalaSettings extends StandardScalaSettings with Warnings {
  self: MutableSettings =>

  /** Set of settings */
  protected[scala] lazy val allSettings = mutable.LinkedHashMap[String, Setting]()

  /** The user class path, specified by `-classpath` or `-cp`,
   *  defaults to the value of CLASSPATH env var if it is set, as in Java,
   *  or else to `"."` for the current user directory.
   */
  protected def defaultClasspath = Option(System.getenv("CLASSPATH")).getOrElse(".")

  /** If any of these settings is enabled, the compiler should print a message and exit.  */
  def infoSettings = List[Setting](version, help, Vhelp, Whelp, Xhelp, Yhelp, showPlugins, showPhases, genPhaseGraph, printArgs)

  /** Is an info setting set? Any -option:help? */
  def isInfo = infoSettings.exists(_.isSetByUser) || allSettings.valuesIterator.exists(_.isHelping)

  /** Disable a setting */
  def disable(s: Setting) = allSettings -= s.name

  val jvmargs  = PrefixSetting("-J<flag>", "-J", "Pass <flag> directly to the runtime system.")
  val defines  = PrefixSetting("-Dproperty=value", "-D", "Pass -Dproperty=value directly to the runtime system.")
  /*val toolcp =*/ PathSetting("-toolcp", "Add to the runner classpath.", "") withAbbreviation "--tool-class-path"
  val nobootcp = BooleanSetting("-nobootcp", "Do not use the boot classpath for the scala jars.") withAbbreviation "--no-boot-class-path"

  /**
   *  Standard settings
   */
  // argfiles is only for the help message
  /*val argfiles = */ BooleanSetting    ("@<file>", "A text file containing compiler arguments (options and source files)")
  val classpath     = PathSetting       ("-classpath", "Specify where to find user class files.", defaultClasspath) withAbbreviation "-cp" withAbbreviation "--class-path"
  val d             = OutputSetting     (outputDirs, ".")
  val nospecialization = BooleanSetting ("-no-specialization", "Ignore @specialize annotations.") withAbbreviation "--no-specialization"

  // Would be nice to build this dynamically from scala.languageFeature.
  // The two requirements: delay error checking until you have symbols, and let compiler command build option-specific help.
  object languageFeatures extends MultiChoiceEnumeration {
    val dynamics            = Choice("dynamics",            "Allow direct or indirect subclasses of scala.Dynamic")
    val existentials        = Choice("existentials",        "Existential types (besides wildcard types) can be written and inferred")
    val higherKinds         = Choice("higherKinds",         "Allow higher-kinded types")
    val implicitConversions = Choice("implicitConversions", "Allow definition of implicit functions called views")
    val postfixOps          = Choice("postfixOps",          "Allow postfix operator notation, such as `1 to 10 toList` (not recommended)")
    val reflectiveCalls     = Choice("reflectiveCalls",     "Allow reflective access to members of structural types")
    val macros              = Choice("experimental.macros", "Allow macro definition (besides implementation and application)")
  }
  val language      = {
    val description = "Enable or disable language features"
    MultiChoiceSetting(
      name    = "-language",
      helpArg = "feature",
      descr   = description,
      domain  = languageFeatures
    ) withAbbreviation "--language"
  }
  val release = StringSetting("-release", "release", "Compile for a specific version of the Java platform. Supported targets: 6, 7, 8, 9", "").withPostSetHook { (value: StringSetting) =>
    if (value.value != "" && !scala.util.Properties.isJavaAtLeast("9")) {
      errorFn.apply("-release is only supported on Java 9 and higher")
    } else {
      // TODO validate numeric value
      // TODO validate release <= java.specification.version
    }
  } withAbbreviation "--release"
  def releaseValue: Option[String] = Option(release.value).filter(_ != "")

  /*
   * The previous "-source" option is intended to be used mainly
   * though this helper.
   */
  private[this] val version212 = ScalaVersion("2.12.0")
  def isScala212: Boolean = source.value >= version212
  private[this] val version213 = ScalaVersion("2.13.0")
  def isScala213: Boolean = source.value >= version213
  private[this] val version214 = ScalaVersion("2.14.0")
  def isScala214: Boolean = source.value >= version214
  private[this] val version300 = ScalaVersion("3.0.0")
  def isScala300: Boolean = source.value >= version300

  /**
   * -X "Advanced" settings
   */
  val Xhelp              = BooleanSetting      ("-X", "Print a synopsis of advanced options.")
  val checkInit          = BooleanSetting      ("-Xcheckinit", "Wrap field accessors to throw an exception on uninitialized access.")
  val developer          = BooleanSetting      ("-Xdev", "Issue warnings about anything which seems amiss in compiler internals. Intended for compiler developers")
  val noassertions       = BooleanSetting      ("-Xdisable-assertions", "Generate no assertions or assumptions.") andThen (flag =>
                                                if (flag) elidebelow.value = elidable.ASSERTION + 1)
  val elidebelow         = IntSetting          ("-Xelide-below", "Calls to @elidable methods are omitted if method priority is lower than argument",
                                                elidable.MINIMUM, None, elidable.byName get _)
  val noForwarders       = BooleanSetting      ("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val genPhaseGraph      = StringSetting       ("-Xgenerate-phase-graph", "file", "Generate the phase graphs (outputs .dot files) to fileX.dot.", "")
  val maxerrs            = IntSetting          ("-Xmaxerrs", "Maximum errors to print", 100, None, _ => None)
  val maxwarns           = IntSetting          ("-Xmaxwarns", "Maximum warnings to print", 100, None, _ => None)
  val Xmigration         = ScalaVersionSetting ("-Xmigration", "version", "Warn about constructs whose behavior may have changed since version.", initial = NoScalaVersion, default = Some(AnyScalaVersion))
  val Xnojline           = BooleanSetting      ("-Xnojline", "Do not use JLine for editing.")
  val Xverify            = BooleanSetting      ("-Xverify", "Verify generic signatures in generated bytecode.")
  val plugin             = MultiStringSetting  ("-Xplugin", "paths", "Load a plugin from each classpath.")
  val disable            = MultiStringSetting  ("-Xplugin-disable", "plugin", "Disable plugins by name.")
  val showPlugins        = BooleanSetting      ("-Xplugin-list", "Print a synopsis of loaded plugins.")
  val require            = MultiStringSetting  ("-Xplugin-require", "plugin", "Abort if a named plugin is not loaded.")
  val pluginsDir         = StringSetting       ("-Xpluginsdir", "path", "Path to search for plugin archives.", Defaults.scalaPluginPath)
  val prompt             = BooleanSetting      ("-Xprompt", "Display a prompt after each error (debugging option).")
  val resident           = BooleanSetting      ("-Xresident", "Compiler stays resident: read source filenames from standard input.")
  val script             = StringSetting       ("-Xscript", "object", "Treat the source file as a script and wrap it in a main method.", "")
  val mainClass          = StringSetting       ("-Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val sourceReader       = StringSetting       ("-Xsource-reader", "classname", "Specify a custom method for reading source files.", "")
  val reporter           = StringSetting       ("-Xreporter", "classname", "Specify a custom subclass of FilteringReporter for compiler messages.", "scala.tools.nsc.reporters.ConsoleReporter")
  val source             = ScalaVersionSetting ("-Xsource", "version", "Enable features that will be available in a future version of Scala, for purposes of early migration and alpha testing.", initial = version213).withPostSetHook(s =>
      if (s.value < version213) errorFn.apply(s"-Xsource must be at least the current major version (${version213.versionString})"))

  val XnoPatmatAnalysis = BooleanSetting ("-Xno-patmat-analysis", "Don't perform exhaustivity/unreachability analysis. Also, ignore @switch annotation.")

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
  val Yhelp           = BooleanSetting    ("-Y", "Print a synopsis of private options.")
  val breakCycles     = BooleanSetting    ("-Ybreak-cycles", "Attempt to break cycles encountered during typing")
  val check           = PhasesSetting     ("-Ycheck", "Check the tree at the end of")
  val Ycompacttrees   = BooleanSetting    ("-Ycompact-trees", "Use compact tree printer when displaying trees.")
  val noCompletion    = BooleanSetting    ("-Yno-completion", "Disable tab-completion in the REPL.")
  val termConflict    = ChoiceSetting     ("-Yresolve-term-conflict", "strategy", "Resolve term conflicts.", List("package", "object", "error"), "error")
  val Ynogenericsig   = BooleanSetting    ("-Yno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val noimports       = BooleanSetting    ("-Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
                       .withPostSetHook(bs => if (bs) imports.value = Nil)
  val nopredef        = BooleanSetting    ("-Yno-predef", "Compile without importing Predef.")
                       .withPostSetHook(bs => if (bs && !noimports) imports.value = "java.lang" :: "scala" :: Nil)
  val imports         = MultiStringSetting(name="-Yimports", arg="import", descr="Custom root imports, default is `java.lang,scala,scala.Predef`.", helpText=Some(
  sm"""|Specify a list of packages and objects to import from as "root" imports.
       |Root imports form the root context in which all Scala source is evaluated.
       |The names supplied to `-Yimports` must be fully-qualified.
       |
       |For example, the default scala.Predef results in an `import scala.Predef._`.
       |Ordinary access and scoping rules apply. Root imports increase the scoping
       |depth, so that later root imports shadow earlier ones. In addition,
       |names bound by root imports have lowest binding precedence, so that they
       |cannot induce ambiguities in user code, where definitions and imports
       |always have a higher precedence. Root imports are imports of last resort.
       |
       |By convention, an explicit import from a root import object such as
       |Predef disables that root import for the current source file. The import
       |is disabled when the import expression is compiled, so, also by convention,
       |the import should be placed early in source code order. The textual name
       |in the import does not need to match the value of `-Yimports`; the import
       |works in the usual way, subject to renames and name binding precedence.
       |
    """
  ))
  val Yrecursion      = IntSetting        ("-Yrecursion", "Set recursion depth used when locking symbols.", 0, Some((0, Int.MaxValue)), (_: String) => None)

  val YprintTrees = ChoiceSetting(
      name = "-Yprint-trees",
      helpArg = "style",
      descr = "How to print trees when -Vprint is enabled.",
      choices = List("text", "compact", "format", "text+format"),
      default = "text"
    ).withPostSetHook(pt => pt.value match {
      case "text"        =>
      case "compact"     => XshowtreesCompact.value = true
      case "format"      => Xshowtrees.value = true
      case "text+format" => XshowtreesStringified.value = true
    })

  val Xshowtrees      = BooleanSetting    ("-Yshow-trees", "(Requires -Vprint:) Print detailed ASTs in formatted form.").internalOnly()
  val XshowtreesCompact
                      = BooleanSetting    ("-Yshow-trees-compact", "(Requires -Vprint:) Print detailed ASTs in compact form.").internalOnly()
  val XshowtreesStringified
                      = BooleanSetting    ("-Yshow-trees-stringified", "(Requires -Vprint:) Print stringifications along with detailed ASTs.").internalOnly()

  val skip            = PhasesSetting     ("-Yskip", "Skip")
  val Ygenasmp        = StringSetting     ("-Ygen-asmp",  "dir", "Generate a parallel output directory of .asmp files (ie ASM Textifier output).", "")
  val Ydumpclasses    = StringSetting     ("-Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val stopAfter       = PhasesSetting     ("-Ystop-after", "Stop after") withAbbreviation ("-stop") // backward compat
  val stopBefore      = PhasesSetting     ("-Ystop-before", "Stop before")
  val Yrangepos       = BooleanSetting    ("-Yrangepos", "Use range positions for syntax trees.")
  val Yvalidatepos    = PhasesSetting     ("-Yvalidate-pos", s"Validate positions after the given phases (implies ${Yrangepos.name})") withPostSetHook (_ => Yrangepos.value = true)
  val Yreifycopypaste = BooleanSetting    ("-Yreify-copypaste", "Dump the reified trees in copypasteable representation.")
  val Ymacroexpand    = ChoiceSetting     ("-Ymacro-expand", "policy", "Control expansion of macros, useful for scaladoc and presentation compiler.", List(MacroExpand.Normal, MacroExpand.None, MacroExpand.Discard), MacroExpand.Normal)
  val YmacroFresh     = BooleanSetting    ("-Ymacro-global-fresh-names", "Should fresh names in macros be unique across all compilation units")
  val YmacroAnnotations = BooleanSetting  ("-Ymacro-annotations", "Enable support for macro annotations, formerly in macro paradise.")
  val Yreplclassbased = BooleanSetting    ("-Yrepl-class-based", "Use classes to wrap REPL snippets instead of objects") withDefault true
  val YreplMagicImport = BooleanSetting   ("-Yrepl-use-magic-imports", "In the code that wraps REPL snippets, use magic imports rather than nesting wrapper object/classes") withDefault true
  val Yreploutdir     = StringSetting     ("-Yrepl-outdir", "path", "Write repl-generated classfiles to given output directory (use \"\" to generate a temporary dir)" , "")
  @deprecated("Unused setting will be removed", since="2.13")
  val Yreplsync       = new BooleanSetting    ("-Yrepl-sync", "Legacy setting for sbt compatibility, unused.").internalOnly()
  val Yscriptrunner   = StringSetting     ("-Yscriptrunner", "classname", "Specify a scala.tools.nsc.ScriptRunner (default, resident, shutdown, or a class name).", "default")
  val YdisableFlatCpCaching  = BooleanSetting    ("-Yno-flat-classpath-cache", "Do not cache flat classpath representation of classpath elements from jars across compiler instances.") withAbbreviation "-YdisableFlatCpCaching"
  val YcachePluginClassLoader  = CachePolicy.setting("plugin", "compiler plugins")
  val YcacheMacroClassLoader   = CachePolicy.setting("macro", "macros")
  val YmacroClasspath = PathSetting       ("-Ymacro-classpath", "The classpath used to reflectively load macro implementations, default is the compilation classpath.", "")

  val Youtline        = BooleanSetting    ("-Youtline", "Don't compile method bodies. Use together with `-Ystop-afer:pickler to generate the pickled signatures for all source files.").internalOnly()

  val exposeEmptyPackage = BooleanSetting ("-Yexpose-empty-package", "Internal only: expose the empty package.").internalOnly()
  val Ydelambdafy        = ChoiceSetting  ("-Ydelambdafy", "strategy", "Strategy used for translating lambdas into JVM code.", List("inline", "method"), "method")

  // Allows a specialised jar to be written. For instance one that provides stable hashing of content, or customisation of the file storage
  val YjarFactory = StringSetting   ("-YjarFactory", "classname", "factory for jar files", classOf[DefaultJarFactory].getName)
  val YaddBackendThreads = IntSetting   ("-Ybackend-parallelism", "maximum worker threads for backend", 1, Some((1,16)), (x: String) => None )
  val YmaxQueue = IntSetting   ("-Ybackend-worker-queue", "backend threads worker queue size", 0, Some((0,1000)), (x: String) => None )
  val YjarCompressionLevel = IntSetting("-Yjar-compression-level", "compression level to use when writing jar files",
    Deflater.DEFAULT_COMPRESSION, Some((Deflater.DEFAULT_COMPRESSION,Deflater.BEST_COMPRESSION)), (x: String) => None)
  val YpickleJava = BooleanSetting("-Ypickle-java", "Pickler phase should compute pickles for .java defined symbols for use by build tools").internalOnly()
  val YpickleWrite = StringSetting("-Ypickle-write", "directory|jar", "destination for generated .sig files containing type signatures.", "", None).internalOnly()
  val YpickleWriteApiOnly = BooleanSetting("-Ypickle-write-api-only", "Exclude private members (other than those material to subclass compilation, such as private trait vals) from generated .sig files containing type signatures.").internalOnly()

  sealed abstract class CachePolicy(val name: String, val help: String)
  object CachePolicy {
    def setting(style: String, styleLong: String) = ChoiceSetting(s"-Ycache-$style-class-loader", "policy", s"Policy for caching class loaders for $styleLong that are dynamically loaded.", values.map(_.name), None.name, values.map(_.help))
    object None extends CachePolicy("none", "Don't cache class loader")
    object LastModified extends CachePolicy("last-modified", "Cache class loader, using file last-modified time to invalidate")
    object Always extends CachePolicy("always", "Cache class loader with no invalidation")
    // TODO Jorge to add new policy. Think about whether there is a benefit to the user on offering this as a separate policy or unifying with the previous one.
    // object ZipMetadata extends CachePolicy("zip-metadata", "Cache classloader, using file last-modified time, then ZIP file metadata to invalidate")
    def values: List[CachePolicy] = List(None, LastModified, Always)
  }

  object optChoices extends MultiChoiceEnumeration {
    val unreachableCode         = Choice("unreachable-code",            "Eliminate unreachable code, exception handlers guarding no instructions, redundant metadata (debug information, line numbers).")
    val simplifyJumps           = Choice("simplify-jumps",              "Simplify branching instructions, eliminate unnecessary ones.")
    val compactLocals           = Choice("compact-locals",              "Eliminate empty slots in the sequence of local variables.")
    val copyPropagation         = Choice("copy-propagation",            "Eliminate redundant local variables and unused values (including closures). Enables unreachable-code.")
    val redundantCasts          = Choice("redundant-casts",             "Eliminate redundant casts using a type propagation analysis.")
    val boxUnbox                = Choice("box-unbox",                   "Eliminate box-unbox pairs within the same method (also tuples, xRefs, value class instances). Enables unreachable-code.")
    val nullnessTracking        = Choice("nullness-tracking",           "Track nullness / non-nullness of local variables and apply optimizations.")
    val closureInvocations      = Choice("closure-invocations" ,        "Rewrite closure invocations to the implementation method.")
    val allowSkipCoreModuleInit = Choice("allow-skip-core-module-init", "Allow eliminating unused module loads for core modules of the standard library (e.g., Predef, ClassTag).")
    val assumeModulesNonNull    = Choice("assume-modules-non-null",     "Assume loading a module never results in null (happens if the module is accessed in its super constructor).")
    val allowSkipClassLoading   = Choice("allow-skip-class-loading",    "Allow optimizations that can skip or delay class loading.")
    val inline                  = Choice("inline",                      "Inline method invocations according to -Yopt-inline-heuristics and -opt-inline-from.")

    // l:none is not an expanding option, unlike the other l: levels. But it is excluded from -opt:_ below.
    val lNone = Choice("l:none",
      "Disable optimizations. Takes precedence: `-opt:l:none,+box-unbox` / `-opt:l:none -opt:box-unbox` don't enable box-unbox.")

    private val defaultChoices = List(unreachableCode)
    val lDefault = Choice(
      "l:default",
      "Enable default optimizations: " + defaultChoices.mkString("", ",", "."),
      expandsTo = defaultChoices)

    private val methodChoices = List(unreachableCode, simplifyJumps, compactLocals, copyPropagation, redundantCasts, boxUnbox, nullnessTracking, closureInvocations, allowSkipCoreModuleInit, assumeModulesNonNull, allowSkipClassLoading)
    val lMethod = Choice(
      "l:method",
      "Enable intra-method optimizations: " + methodChoices.mkString("", ",", "."),
      expandsTo = methodChoices)

    private val inlineChoices = List(lMethod, inline)
    val lInline = Choice("l:inline",
      "Enable cross-method optimizations (note: inlining requires -opt-inline-from): " + inlineChoices.mkString("", ",", "."),
      expandsTo = inlineChoices)

    // "l:none" is excluded from wildcard expansion so that -opt:_ does not disable all settings
    override def wildcardChoices = super.wildcardChoices.filter(_ ne lNone)
  }

  // We don't use the `default` parameter of `MultiChoiceSetting`: it specifies the default values
  // when `-opt` is passed without explicit choices. When `-opt` is not explicitly specified, the
  // set `opt.value` is empty.
  val opt = MultiChoiceSetting(
    name = "-opt",
    helpArg = "optimization",
    descr = "Enable optimizations, `help` for details.",
    domain = optChoices,
  )

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
  def optAllowSkipCoreModuleInit = optEnabled(optChoices.allowSkipCoreModuleInit)
  def optAssumeModulesNonNull    = optEnabled(optChoices.assumeModulesNonNull)
  def optAllowSkipClassLoading   = optEnabled(optChoices.allowSkipClassLoading)
  def optInlinerEnabled          = optEnabled(optChoices.inline)

  def optBuildCallGraph          = optInlinerEnabled || optClosureInvocations
  def optAddToBytecodeRepository = optBuildCallGraph || optInlinerEnabled || optClosureInvocations
  def optUseAnalyzerCache        = opt.isSetByUser && !optNone && (optBuildCallGraph || opt.value.size > 1)

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
        |The setting accepts a list of patterns: `-opt-inline-from:p1,p2`. The setting can be passed
        |multiple times, the list of patterns gets extended. A leading `!` marks a pattern excluding.
        |The last matching pattern defines whether a classfile is included or excluded (default: excluded).
        |For example, `a.**,!a.b.**` includes classes in a and sub-packages, but not in a.b and sub-packages.
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
    descr = "Enable optimizer warnings, `help` for details.",
    domain = optWarningsChoices,
    default = Some(List(optWarningsChoices.atInlineFailed.name))) withPostSetHook { _ =>
    // no need to set `Wconf` to `silent` if optWarnings is none, since no warnings are reported
    if (optWarningsSummaryOnly) Wconf.tryToSet(List(s"cat=optimizer:ws"))
    else Wconf.tryToSet(List(s"cat=optimizer:w"))
  }

  def optWarningsSummaryOnly: Boolean = optWarnings.value subsetOf Set(optWarningsChoices.none, optWarningsChoices.atInlineFailedSummary)

  def optWarningEmitAtInlineFailed =
    !optWarnings.isSetByUser ||
      optWarnings.contains(optWarningsChoices.atInlineFailedSummary) ||
      optWarnings.contains(optWarningsChoices.atInlineFailed) ||
      optWarnings.contains(optWarningsChoices.anyInlineFailed)

  def optWarningNoInlineMixed                      = optWarnings.contains(optWarningsChoices.noInlineMixed)
  def optWarningNoInlineMissingBytecode            = optWarnings.contains(optWarningsChoices.noInlineMissingBytecode)
  def optWarningNoInlineMissingScalaInlineInfoAttr = optWarnings.contains(optWarningsChoices.noInlineMissingScalaInlineInfoAttr)

  val YprofileEnabled = BooleanSetting("-Yprofile-enabled", "Enable profiling.")
  val YprofileDestination = StringSetting("-Yprofile-destination", "file", "Profiling output - specify a file or `-` for console.", "").
    withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileTrace = StringSetting("-Yprofile-trace", "file", "Capture trace of compilation in Chrome Trace format", "profile.trace").
    withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileExternalTool = PhasesSetting("-Yprofile-external-tool", "Enable profiling for a phase using an external tool hook. Generally only useful for a single phase", "typer").
    withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileRunGcBetweenPhases = PhasesSetting("-Yprofile-run-gc", "Run a GC between phases - this allows heap size to be accurate at the expense of more time. Specify a list of phases, or all", "_").
    withPostSetHook( _ => YprofileEnabled.value = true )

  val YpatmatExhaustdepth     = IntSetting("-Ypatmat-exhaust-depth", "off", 20, Some((10, Int.MaxValue)),
    str => Some(if(str.equalsIgnoreCase("off")) Int.MaxValue else str.toInt))

  /**
   * -V "Verbose" settings
   */
  val Vhelp              = BooleanSetting("-V", "Print a synopsis of verbose options.")
  val browse          = PhasesSetting("-Vbrowse", "Browse the abstract syntax tree after") withAbbreviation "-Ybrowse"
  val debug           = BooleanSetting("-Vdebug", "Increase the quantity of debugging output.") withAbbreviation "-Ydebug"
  val Ydocdebug          = BooleanSetting("-Vdoc", "Trace scaladoc activity.") withAbbreviation "-Ydoc-debug"
  val Yidedebug          = BooleanSetting("-Vide", "Generate, validate and output trees using the interactive compiler.") withAbbreviation "-Yide-debug"
  val Yissuedebug        = BooleanSetting("-Vissue", "Print stack traces when a context issues an error.") withAbbreviation "-Yissue-debug"
  val log                = PhasesSetting("-Vlog", "Log operations during") withAbbreviation "-Ylog"
  val Ylogcp             = BooleanSetting("-Vclasspath", "Output information about what classpath is being applied.") withAbbreviation "-Ylog-classpath"
  val YmacrodebugLite    = BooleanSetting("-Vmacro-lite", "Trace macro activities with less output.") withAbbreviation "-Ymacro-debug-lite"
  val YmacrodebugVerbose = BooleanSetting("-Vmacro", "Trace macro activities: compilation, generation of synthetics, classloading, expansion, exceptions.") withAbbreviation "-Ymacro-debug-verbose"
  val YoptTrace = StringSetting("-Vopt", "package/Class.method", "Trace the optimizer progress for methods; `_` to print all, prefix match to select.", "")
    .withAbbreviation("-Yopt-trace")
  val YoptLogInline = StringSetting("-Vinline", "package/Class.method", "Print a summary of inliner activity; `_` to print all, prefix match to select.", "")
    .withAbbreviation("-Yopt-log-inline")
  val Ypatmatdebug       = BooleanSetting("-Vpatmat", "Trace pattern matching translation.") withAbbreviation "-Ypatmat-debug"
  val showPhases         = BooleanSetting("-Vphases", "Print a synopsis of compiler phases.")
    .withAbbreviation("-Xshow-phases")
  val Yposdebug          = BooleanSetting("-Vpos", "Trace position validation.") withAbbreviation "-Ypos-debug"
  val Xprint             = PhasesSetting("-Vprint", "Print out program after")
    .withAbbreviation("-Xprint")
  val Xprintpos          = BooleanSetting("-Vprint-pos", "Print tree positions, as offsets.")
    .withAbbreviation("-Xprint-pos")
  val printtypes         = BooleanSetting("-Vprint-types", "Print tree types (debugging option).")
    .withAbbreviation("-Xprint-types")
  val printArgs          = StringSetting("-Vprint-args", "file", "Print all compiler arguments to the specified location. Use - to echo to the reporter.", "-")
    .withAbbreviation("-Xprint-args")
  val Yquasiquotedebug   = BooleanSetting("-Vquasiquote", "Trace quasiquotations.") withAbbreviation "-Yquasiquote-debug"
  val Yreifydebug        = BooleanSetting("-Vreify", "Trace reification.") withAbbreviation "-Yreify-debug"
  val Yshow           = PhasesSetting     ("-Vshow", "(Requires -Xshow-class or -Xshow-object) Show after")
    .withAbbreviation("-Yshow")
  val Xshowcls           = StringSetting("-Vshow-class", "class", "Show internal representation of class.", "")
    .withAbbreviation("-Xshow-class")
  val Xshowobj           = StringSetting("-Vshow-object", "object", "Show internal representation of object.", "")
    .withAbbreviation("-Xshow-object")
  val Ymemberpos      = StringSetting("-Vshow-member-pos", "output style", s"Show start and end positions of members (implies ${Yrangepos.name})", "")
    .withPostSetHook(_ => Yrangepos.value = true)
    .withAbbreviation("-Yshow-member-pos")
  val Yshowsymkinds   = BooleanSetting    ("-Vshow-symkinds", "Print abbreviated symbol kinds next to symbol names.")
    .withAbbreviation("-Yshow-symkinds")
  val Yshowsymowners  = BooleanSetting    ("-Vshow-symowners", "Print owner identifiers next to symbol names.")
    .withAbbreviation("-Yshow-symowners")
  val Ystatistics = PhasesSetting("-Vstatistics", "Print compiler statistics for specific phases", "parser,typer,patmat,erasure,cleanup,jvm")
    .withPostSetHook(s => YstatisticsEnabled.value = s.value.nonEmpty)
    .withAbbreviation("-Ystatistics")
  val YstatisticsEnabled = BooleanSetting("-Ystatistics-enabled", "Internal setting, indicating that statistics are enabled for some phase.").internalOnly()
  val YhotStatisticsEnabled = BooleanSetting("-Vhot-statistics", s"Enable `${Ystatistics.name}` to also print hot statistics.")
    .withAbbreviation("-Yhot-statistics")
  val Yshowsyms       = BooleanSetting("-Vsymbols", "Print the AST symbol hierarchy after each phase.") withAbbreviation "-Yshow-syms"
  val Ytyperdebug        = BooleanSetting("-Vtyper", "Trace type assignments.") withAbbreviation "-Ytyper-debug"
  val XlogImplicits      = BooleanSetting("-Vimplicits", "Show more detail on why some implicits are not applicable.")
    .withAbbreviation("-Xlog-implicits")
  val logImplicitConv    = BooleanSetting("-Vimplicit-conversions", "Print a message whenever an implicit conversion is inserted.")
    .withAbbreviation("-Xlog-implicit-conversions")
  val logReflectiveCalls = BooleanSetting("-Vreflective-calls", "Print a message when a reflective method call is generated")
    .withAbbreviation("-Xlog-reflective-calls")
  val logFreeTerms       = BooleanSetting("-Vfree-terms", "Print a message when reification creates a free term.")
    .withAbbreviation("-Xlog-free-terms")
  val logFreeTypes       = BooleanSetting("-Vfree-types", "Print a message when reification resorts to generating a free type.")
    .withAbbreviation("-Xlog-free-types")

  /** Groups of Settings.
   */
  val future        = BooleanSetting("-Xfuture", "Replaced by -Xsource.").withDeprecationMessage("Not used since 2.13.")
  val optimise      = BooleanSetting("-optimize", "Enables optimizations.")
    .withAbbreviation("-optimise")
    .withDeprecationMessage("Since 2.12, enables -opt:l:inline -opt-inline-from:**. See -opt:help.")
    .withPostSetHook(_ => {
      opt.enable(optChoices.lInline)
      optInlineFrom.value = List("**")
    })
  val Xexperimental = BooleanSetting("-Xexperimental", "Former graveyard for language-forking extensions.")
    .withDeprecationMessage("Not used since 2.13.")

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

  val YpresentationLocateSourceFile = BooleanSetting("-Ypresentation-locate-source-file", "Enables legacy code in the classfile parser to locate a .scala file in the output directories corresponding to the SourceFile attribute .class file.")

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
    None
  }
}
