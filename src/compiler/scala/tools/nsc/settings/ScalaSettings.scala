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

import scala.annotation.{elidable, nowarn}
import scala.collection.mutable
import scala.language.existentials
import scala.reflect.internal.util.StatisticsStatics
import scala.tools.nsc.util.DefaultJarFactory
import scala.tools.util.PathResolver.Defaults
import scala.util.chaining._

trait ScalaSettings extends StandardScalaSettings with Warnings { _: MutableSettings =>

  /** Set of settings */
  protected[scala] lazy val allSettings = mutable.LinkedHashMap[String, Setting]()

  /** The user class path, specified by `-classpath` or `-cp`,
   *  defaults to the value of CLASSPATH env var if it is set, as in Java,
   *  or else to `"."` for the current user directory.
   */
  protected def defaultClasspath = Option(System.getenv("CLASSPATH")).getOrElse(".")

  /** If any of these settings is enabled, the compiler should print a message and exit.  */
  def infoSettings = List[Setting](version, help, Vhelp, Whelp, Xhelp, Yhelp, showPlugins, showPhases, genPhaseGraph)

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
  /*val argfiles = */ BooleanSetting("@<file>", "A text file containing compiler arguments (options and source files)")
  val classpath     = PathSetting   ("-classpath", "Specify where to find user class files.", defaultClasspath) withAbbreviation "-cp" withAbbreviation "--class-path"
  val outdir        = OutputSetting (".").withPostSetHook(s => try outputDirs.setSingleOutput(s.value) catch { case FatalError(msg) => errorFn(msg) }).tap(_.postSetHook())

  val nospecialization = BooleanSetting("-no-specialization", "Ignore @specialize annotations.") withAbbreviation "--no-specialization"

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

  /**
   * -X "Advanced" settings
   */
  val Xhelp              = BooleanSetting      ("-X", "Print a synopsis of advanced options.")
  val async              = BooleanSetting      ("-Xasync", "Enable the async phase for scala.async.Async.{async,await}.")
  val checkInit          = BooleanSetting      ("-Xcheckinit", "Wrap field accessors to throw an exception on uninitialized access.")
  val developer          = BooleanSetting      ("-Xdev", "Issue warnings about anything which seems amiss in compiler internals. Intended for compiler developers").withPostSetHook(s => if (s.value) StatisticsStatics.enableDeveloperAndDeoptimize())
  val noassertions       = BooleanSetting      ("-Xdisable-assertions", "Generate no assertions or assumptions.") andThen (flag =>
                                                if (flag) elidebelow.value = elidable.ASSERTION + 1)
  val elidebelow         = IntSetting          ("-Xelide-below", "Calls to @elidable methods are omitted if method priority is lower than argument",
                                                elidable.MINIMUM, None, elidable.byName get _)
  val noForwarders       = BooleanSetting      ("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val genPhaseGraph      = StringSetting       ("-Vphase-graph", arg="file", descr="Generate phase graph to <file>-*.dot.").withAbbreviation("-Xgenerate-phase-graph")
  val maxerrs            = IntSetting          ("-Xmaxerrs", "Maximum errors to print", 100, None, _ => None)
  val maxwarns           = IntSetting          ("-Xmaxwarns", "Maximum warnings to print", 100, None, _ => None)
  val Xmigration         = ScalaVersionSetting ("-Xmigration", "version", "Warn about constructs whose behavior may have changed since version.", initial = NoScalaVersion, default = Some(AnyScalaVersion))
  val Xnojline           = BooleanSetting      ("-Xnojline", "Do not use JLine for editing.")
  val Xjline             = ChoiceSetting       (
    name    = "-Xjline",
    helpArg = "mode",
    descr   = "Select JLine mode.",
    choices = List("emacs", "vi", "off"),
    default = "emacs",
    choicesHelp = List(
      "emacs key bindings.",
      "vi key bindings",
      "No JLine editing."))
    .withDeprecationMessage("Replaced by use of '~/.inputrc'. Set 'editing-mode' to 'vi', 'emacs' or 'dumb'")

  val Xverify            = BooleanSetting      ("-Xverify", "Verify generic signatures in generated bytecode.")
  val plugin             = MultiStringSetting  ("-Xplugin", "paths", "Load a plugin from each classpath.")
  val disable            = MultiStringSetting  ("-Xplugin-disable", "plugin", "Disable plugins by name.")
  val showPlugins        = BooleanSetting      ("-Xplugin-list", "Print a synopsis of loaded plugins.")
  val require            = MultiStringSetting  ("-Xplugin-require", "plugin", "Abort if a named plugin is not loaded.")
  val pluginsDir         = StringSetting       ("-Xpluginsdir", "path", "Path to search for plugin archives.", Defaults.scalaPluginPath)
  val prompt             = BooleanSetting      ("-Xprompt", "Display a prompt after each error (debugging option).")
  val resident           = BooleanSetting      ("-Xresident", "Compiler stays resident: read source filenames from standard input.")
  val script             = StringSetting       ("-Xscript", "object", "Treat the source file as a script and wrap it in a main method.", "Main")
  val mainClass          = StringSetting       ("-Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val sourceReader       = StringSetting       ("-Xsource-reader", "classname", "Specify a custom method for reading source files.", "")
  val reporter           = StringSetting       ("-Xreporter", "classname", "Specify a custom subclass of FilteringReporter for compiler messages.", "scala.tools.nsc.reporters.ConsoleReporter")
  private val XsourceHelp =
    sm"""|-Xsource:3 is for migrating a codebase. -Xsource-features can be added for
         |cross-building to adopt certain Scala 3 behaviors.
         |
         |See also "Scala 2 with -Xsource:3" on docs.scala-lang.org.
         |
         |-Xsource:3 issues migration warnings in category `cat=scala3-migration`,
         |which are promoted to errors by default using a `-Wconf` configuration.
         |Examples of promoted warnings:
         |  * Implicit definitions must have an explicit type
         |  * (x: Any) + "" is deprecated
         |  * An empty argument list is not adapted to the unit value
         |  * Member classes cannot shadow a same-named class defined in a parent
         |  * Presence or absence of parentheses in overrides must match exactly
         |
         |Certain benign syntax features are enabled:
         |  * case C(xs*) =>
         |  * A & B type intersection
         |  * import p.*
         |  * import p.m as n
         |  * import p.{given, *}
         |  * Eta-expansion `x.m` of methods without trailing `_`
         |
         |The following constructs emit a migration warning under -Xsource:3. To adopt
         |Scala 3 semantics, see `-Xsource-features:help`.
         |${sourceFeatures.values.toList.collect { case c: sourceFeatures.Choice if c.expandsTo.isEmpty => c.help }.map(h => s"  * $h").mkString("\n")}
         |"""
  @nowarn("cat=deprecation")
  val source = ScalaVersionSetting ("-Xsource", "version", "Enable warnings and features for a future version.", initial = ScalaVersion("2.13"), helpText = Some(XsourceHelp)).withPostSetHook { s =>
    if (s.value.unparse == "3.0.0-cross")
      XsourceFeatures.tryToSet(List("_"))
    if (s.value < ScalaVersion("3"))
      if (s.value >= ScalaVersion("2.14"))
        s.withDeprecationMessage("instead of -Xsource:2.14, use -Xsource:3 and optionally -Xsource-features").value = ScalaVersion("3")
      else if (s.value < ScalaVersion("2.13"))
        errorFn.apply(s"-Xsource must be at least the current major version (${ScalaVersion("2.13").versionString})")
  }

  private val scala3Version = ScalaVersion("3")
  @deprecated("Use currentRun.isScala3 instead", since="2.13.9")
  def isScala3 = source.value >= scala3Version

  // buffet of features available under -Xsource:3
  object sourceFeatures extends MultiChoiceEnumeration {
    // Changes affecting binary encoding
    val caseApplyCopyAccess    = Choice("case-apply-copy-access", "Constructor modifiers are used for apply / copy methods of case classes. [bin]")
    val caseCompanionFunction  = Choice("case-companion-function", "Synthetic case companion objects no longer extend FunctionN. [bin]")
    val caseCopyByName         = Choice("case-copy-by-name", "Synthesize case copy method with by-name parameters. [bin]")
    val inferOverride          = Choice("infer-override", "Inferred type of member uses type of overridden member. [bin]")

    // Other semantic changes
    val any2StringAdd          = Choice("any2stringadd", "Implicit `any2stringadd` is never inferred.")
    val unicodeEscapesRaw      = Choice("unicode-escapes-raw", "Don't process unicode escapes in triple quoted strings and raw interpolations.")
    val stringContextScope     = Choice("string-context-scope", "String interpolations always desugar to scala.StringContext.")
    val leadingInfix           = Choice("leading-infix", "Leading infix operators continue the previous line.")
    val packagePrefixImplicits = Choice("package-prefix-implicits", "The package prefix p is no longer part of the implicit search scope for type p.A.")
    val implicitResolution     = Choice("implicit-resolution", "Use Scala-3-style downwards comparisons for implicit search and overloading resolution (see github.com/scala/scala/pull/6037).")

    val v13_13_choices = List(caseApplyCopyAccess, caseCompanionFunction, inferOverride, any2StringAdd, unicodeEscapesRaw, stringContextScope, leadingInfix, packagePrefixImplicits)

    val v13_13 = Choice(
      "v2.13.13",
      v13_13_choices.mkString("", ",", "."),
      expandsTo = v13_13_choices)

    val v13_14_choices = implicitResolution :: v13_13_choices

    val v13_14 = Choice(
      "v2.13.14",
      "v2.13.13 plus implicit-resolution",
      expandsTo = v13_14_choices)
  }
  val XsourceFeatures = MultiChoiceSetting(
    name = "-Xsource-features",
    helpArg = "feature",
    descr = "Enable Scala 3 features under -Xsource:3: `-Xsource-features:help` for details.",
    domain = sourceFeatures,
    helpText = Some(
      sm"""Enable Scala 3 features under -Xsource:3.
          |
          |Instead of `-Xsource-features:_`, it is recommended to enable individual features.
          |Features can also be removed from a feature group by prefixing with `-`;
          |for example, `-Xsource-features:v2.13.14,-case-companion-function`.
          |Listing features explicitly ensures new semantic changes in future Scala versions are
          |not silently adopted; new features can be enabled after auditing migration warnings.
          |
          |`-Xsource:3-cross` is a shorthand for `-Xsource:3 -Xsource-features:_`.
          |
          |Features marked with [bin] affect the binary encoding. Enabling them in a project
          |with existing releases for Scala 2.13 can break binary compatibility.
          |
          |Available features:
          |""")
  )

  val XnoPatmatAnalysis = BooleanSetting ("-Xno-patmat-analysis", "Don't perform exhaustivity/unreachability analysis. Also, ignore @switch annotation.")

  val XmixinForceForwarders = ChoiceSetting(
    name    = "-Xmixin-force-forwarders",
    helpArg = "mode",
    descr   = "Generate forwarder methods in classes inheriting concrete methods from traits.",
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

  val nonStrictPatmatAnalysis = BooleanSetting("-Xnon-strict-patmat-analysis", "Disable strict exhaustivity analysis, which assumes guards are false and refutable extractors don't match")

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
  @deprecated("Use outdir instead.", since="2.13.2")
  def d = outdir
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
                       .withPostSetHook(bs => if (bs.value) imports.value = Nil)
  val nopredef        = BooleanSetting    ("-Yno-predef", "Compile without importing Predef.")
                       .withPostSetHook(bs => if (bs.value && !noimports.value) imports.value = "java.lang" :: "scala" :: Nil)
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
      choices = List("text", "compact", "format", "text+format", "diff"),
      default = "text"
    ).withPostSetHook(pt => pt.value match {
      case "compact"     => XshowtreesCompact.value = true
      case "format"      => Xshowtrees.value = true
      case "text+format" => XshowtreesStringified.value = true
      case _             =>
    })

  def showTreeDiff: Boolean = YprintTrees.value == "diff"
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
  val Yrangepos       = BooleanSetting    ("-Yrangepos", "Use range positions for syntax trees.", default = true)
  val Yvalidatepos    = PhasesSetting     ("-Yvalidate-pos", s"Validate positions after the given phases (implies ${Yrangepos.name})") withPostSetHook (_ => Yrangepos.value = true)
  val Yreifycopypaste = BooleanSetting    ("-Yreify-copypaste", "Dump the reified trees in copypasteable representation.")
  val Ymacroexpand    = ChoiceSetting     ("-Ymacro-expand", "policy", "Control expansion of macros, useful for scaladoc and presentation compiler.", List(MacroExpand.Normal, MacroExpand.None, MacroExpand.Discard), MacroExpand.Normal)
  val YmacroFresh     = BooleanSetting    ("-Ymacro-global-fresh-names", "Should fresh names in macros be unique across all compilation units")
  val YmacroAnnotations = BooleanSetting  ("-Ymacro-annotations", "Enable support for macro annotations, formerly in macro paradise.")
  val YtastyNoAnnotations = BooleanSetting("-Ytasty-no-annotations", "Disable support for reading annotations from TASTy, this will prevent safety features such as pattern match exhaustivity and reachability analysis.")
  val YtastyReader        = BooleanSetting("-Ytasty-reader", "Enable support for reading Scala 3's TASTy files, allowing consumption of libraries compiled with Scala 3 (provided they don't use any Scala 3 only features).")
  val Yreplclassbased = BooleanSetting    ("-Yrepl-class-based", "Use classes to wrap REPL snippets instead of objects", default = true)
  val YreplMagicImport = BooleanSetting   ("-Yrepl-use-magic-imports", "In the code that wraps REPL snippets, use magic imports rather than nesting wrapper object/classes", default = true)
  val Yreploutdir     = StringSetting     ("-Yrepl-outdir", "path", "Write repl-generated classfiles to given output directory (use \"\" to generate a temporary dir)" , "")
  @deprecated("Unused setting will be removed", since="2.13")
  val Yreplsync       = new BooleanSetting    ("-Yrepl-sync", "Legacy setting for sbt compatibility, unused.", default = false).internalOnly()
  val Yscriptrunner   = StringSetting     ("-Yscriptrunner", "classname", "Specify a scala.tools.nsc.ScriptRunner (default, resident, shutdown, or a class name).", "default")
  val YdisableFlatCpCaching  = BooleanSetting    ("-Yno-flat-classpath-cache", "Do not cache flat classpath representation of classpath elements from jars across compiler instances.").withAbbreviation("-YdisableFlatCpCaching")
  // Zinc adds YdisableFlatCpCaching automatically for straight-to-JAR compilation, this is a way to override that choice.
  val YforceFlatCpCaching  = BooleanSetting    ("-Yforce-flat-cp-cache", "Force caching flat classpath representation of classpath elements from jars across compiler instances. Has precedence over: " + YdisableFlatCpCaching.name).internalOnly()
  val YcachePluginClassLoader  = CachePolicy.setting("plugin", "compiler plugins")
  val YcacheMacroClassLoader   = CachePolicy.setting("macro", "macros")
  val YmacroClasspath = PathSetting       ("-Ymacro-classpath", "The classpath used to reflectively load macro implementations, default is the compilation classpath.", "")

  val Youtline        = BooleanSetting    ("-Youtline", "Don't compile method bodies. Use together with `-Ystop-after:pickler` to generate the pickled signatures for all source files.").internalOnly()

  val unsafe = MultiStringSetting("-Yrelease", "packages", "Expose platform packages hidden under --release")
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
  val YtrackDependencies = BooleanSetting("-Ytrack-dependencies", "Record references to in unit.depends. Deprecated feature that supports SBT 0.13 with incOptions.withNameHashing(false) only.", default = true)
  val Yscala3ImplicitResolution = BooleanSetting("-Yscala3-implicit-resolution", "Use Scala-3-style downwards comparisons for implicit search and overloading resolution (see github.com/scala/scala/pull/6037).")
    .withDeprecationMessage("Use -Xsource:3 -Xsource-features:implicit-resolution instead")

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
    val ell                     = Choice("l",                           "Deprecated l:none, l:default, l:method, l:inline.")

    // none is not an expanding option. It is excluded from -opt:_ below.
    val lNone = Choice("none", "Disable all optimizations, including explicit options.")

    val defaultOptimizations = List(unreachableCode)
    val lDefault = Choice(
      "default",
      defaultOptimizations.mkString("Enable default optimizations: ", ",", "."),
      expandsTo = defaultOptimizations)

    val localOptimizations = List(simplifyJumps, compactLocals, copyPropagation, redundantCasts, boxUnbox, nullnessTracking, closureInvocations, allowSkipCoreModuleInit, assumeModulesNonNull, allowSkipClassLoading)
    val lMethod = Choice(
      "local",
      (defaultOptimizations ::: localOptimizations).mkString("Enable intra-method optimizations: ", ",", "."),
      expandsTo = defaultOptimizations ::: localOptimizations)

    val inlineFrom = Choice(
      "inline",
      s"Inline method invocations and enable all optimizations; specify where to inline from, as shown below. See also -Yopt-inline-heuristics.\n$inlineHelp",
      expandsTo = defaultOptimizations ::: localOptimizations,
      requiresSelections = true
    )

    // "none" is excluded from wildcard expansion so that -opt:_ does not disable all settings
    override def wildcardChoices = super.wildcardChoices.filter(_ ne lNone)
  }

  // We don't use the `default` parameter of `MultiChoiceSetting`: it specifies the default values
  // when `-opt` is passed without explicit choices. When `-opt` is not explicitly specified, the
  // set `opt.value` is empty.
  val opt = MultiChoiceSetting(
    name = "-opt",
    helpArg = "optimization",
    descr = "Enable optimizations: `-opt:local`, `-opt:inline:<pattern>`; `-opt:help` for details.",
    domain = optChoices,
  ).withPostSetHook { ss =>
    // kludge alert: will be invoked twice, with selections available 2nd time
    // for -opt:l:method reset the ell selections then enable local
    if (ss.contains(optChoices.ell) && optChoices.ell.selections.nonEmpty) {
      val todo = optChoices.ell.selections.map {
        case "none"    => "none"
        case "default" => "default"
        case "method"  => "local"
        case "inline"  => "local"   // enable all except inline, see -opt-inline-from
      }
      optChoices.ell.selections = Nil
      ss.tryToSetColon(todo)
    }
  }

  private def optEnabled(choice: optChoices.Choice) =
    !optNone && {
      opt.contains(choice) ||
      !opt.isSetByUser && optChoices.lDefault.expandsTo.contains(choice)
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
  def optInlinerEnabled          = !optInlineFrom.isEmpty && !optNone

  def optBuildCallGraph          = optInlinerEnabled || optClosureInvocations
  def optAddToBytecodeRepository = optBuildCallGraph || optInlinerEnabled || optClosureInvocations
  def optUseAnalyzerCache        = opt.isSetByUser && !optNone && (optBuildCallGraph || opt.value.size > 1)

  def optInlineFrom: List[String] = optChoices.inlineFrom.selections

  def inlineHelp =
      """
        |Inlining requires a list of patterns defining where code can be inlined from: `-opt:inline:p1,p2`.
        |
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
        |`-opt:inline:p` may be specified multiple times to extend the list of patterns.
        |A leading `!` means exclude anything that matches the pattern. The last matching pattern wins.
        |For example, `a.**,!a.b.**` includes classes in `a` and sub-packages, but not in `a.b` and sub-packages.
        |
        |When patterns are supplied on a command line, it is usually necessary to quote special shell characters
        |such as `*`, `<`, `>`, and `$`: `'-opt:inline:p.*,!p.C$D' '-opt:inline:<sources>'`.
        |Quoting may not be needed in a build file.""".stripMargin

  @deprecated("Deprecated alias", since="2.13.8")
  val xoptInlineFrom = MultiStringSetting(
    "-opt-inline-from",
    "patterns",
    "Patterns for classfile names from which to allow inlining, `help` for details.",
    helpText = Some(inlineHelp))
      //.withDeprecationMessage("use -opt:inline:**")
      .withPostSetHook(from => opt.add("inline", from.value))

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
    name = "-Wopt",
    helpArg = "warning",
    descr = "Enable optimizer warnings, `help` for details.",
    domain = optWarningsChoices,
    default = Some(List(optWarningsChoices.atInlineFailed.name))
  ).withPostSetHook { _ =>
    // no need to set `Wconf` to `silent` if optWarnings is none, since no warnings are reported
    if (optWarningsSummaryOnly) Wconf.tryToSet(List(s"cat=optimizer:ws"))
    else Wconf.tryToSet(List(s"cat=optimizer:w"))
  }
  @deprecated("Deprecated alias", since="2.13.8")
  val xoptWarnings = MultiChoiceSetting(
    name = "-opt-warnings",
    helpArg = "warning",
    descr = "Enable optimizer warnings, `help` for details.",
    domain = optWarningsChoices,
    default = Some(List(optWarningsChoices.atInlineFailed.name))
  ).withPostSetHook { ow =>
    optWarnings.value = ow.value
  }//.withDeprecationMessage("Use -Wopt instead.")

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
  val cyclic          = BooleanSetting("-Vcyclic", "Debug cyclic reference error.")
  val debug           = BooleanSetting("-Vdebug", "Increase the quantity of debugging output.") withAbbreviation "-Ydebug" withPostSetHook (s => if (s.value) StatisticsStatics.enableDebugAndDeoptimize())
  val YdebugTasty     = BooleanSetting("-Vdebug-tasty", "Increase the quantity of debugging output when unpickling tasty.") withAbbreviation "-Ydebug-tasty"
  val VdebugTypeError = BooleanSetting("-Vdebug-type-error", "Print the stack trace when any error is caught.") withAbbreviation "-Ydebug-type-error"
  val Ydocdebug       = BooleanSetting("-Vdoc", "Trace scaladoc activity.") withAbbreviation "-Ydoc-debug"
  val Yidedebug          = BooleanSetting("-Vide", "Generate, validate and output trees using the interactive compiler.") withAbbreviation "-Yide-debug"
//  val Yissuedebug        = BooleanSetting("-Vissue", "Print stack traces when a context issues an error.") withAbbreviation "-Yissue-debug"
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
  val Xprint             = PhasesSetting("-Vprint", "Print out program after (or ~phase for before and after)", "typer")
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
  val Ystatistics = PhasesSetting("-Vstatistics", "Print compiler statistics for specific phases (implies `-Ycollect-statistics`)", "parser,typer,patmat,erasure,cleanup,jvm")
    .withPostSetHook(s => if (s.value.nonEmpty) YstatisticsEnabled.value = true)
    .withAbbreviation("-Ystatistics")
  val YstatisticsEnabled = BooleanSetting("-Ystatistics-enabled", "Internal setting, indicating that statistics are enabled for some phase.").internalOnly().withPostSetHook(s => if (s.value) StatisticsStatics.enableColdStatsAndDeoptimize())
  val YhotStatisticsEnabled = BooleanSetting("-Vhot-statistics", s"Enable `${Ystatistics.name}` to also print hot statistics.")
    .withAbbreviation("-Yhot-statistics").withPostSetHook(s => if (s.value && YstatisticsEnabled.value) StatisticsStatics.enableHotStatsAndDeoptimize())
  val YcollectStatistics = BooleanSetting("-Ycollect-statistics", "Collect cold statistics (quietly, unless `-Vstatistics` is set)")
    .withPostSetHook(s => if (s.value) YstatisticsEnabled.value = true)
  val Yshowsyms       = BooleanSetting("-Vsymbols", "Print the AST symbol hierarchy after each phase.") withAbbreviation "-Yshow-syms"
  val Ytyperdebug        = BooleanSetting("-Vtyper", "Trace type assignments.") withAbbreviation "-Ytyper-debug"
  val Vimplicits            = BooleanSetting("-Vimplicits", "Print dependent missing implicits.").withAbbreviation("-Xlog-implicits")
  val VimplicitsVerboseTree = BooleanSetting("-Vimplicits-verbose-tree", "Display all intermediate implicits in a chain.")
  val VimplicitsMaxRefined  = IntSetting("-Vimplicits-max-refined", "max chars for printing refined types, abbreviate to `F {...}`", Int.MaxValue, Some((0, Int.MaxValue)), _ => None)
  val VtypeDiffs            = BooleanSetting("-Vtype-diffs", "Print found/required error messages as colored diffs.")
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
    .withDeprecationMessage("Since 2.12, enables -opt:inline:**. This can be dangerous.")
    .withPostSetHook(_ => opt.add("inline", List("**")))
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
    @nowarn("cat=deprecation")
    def sourceFeatures: Option[String] =
      Option.when(XsourceFeatures.value.nonEmpty && !isScala3)(s"${XsourceFeatures.name} requires -Xsource:3")

    List(sourceFeatures).flatten match {
      case Nil => None
      case warnings => Some("Conflicting compiler settings were detected. Some settings will be ignored.\n" + warnings.mkString("\n"))
    }
  }
}
