/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala
package tools
package nsc
package settings

import scala.annotation.elidable
import scala.tools.util.PathResolver.Defaults
import scala.collection.mutable
import scala.language.{implicitConversions, existentials}

trait ScalaSettings extends AbsScalaSettings
                       with StandardScalaSettings
                       with Warnings {
  self: MutableSettings =>

  /** Set of settings */
  protected lazy val allSettings = mutable.HashSet[Setting]()

  /** Against my better judgment, giving in to martin here and allowing
   *  CLASSPATH to be used automatically.  So for the user-specified part
   *  of the classpath:
   *
   *  - If -classpath or -cp is given, it is that
   *  - Otherwise, if CLASSPATH is set, it is that
   *  - If neither of those, then "." is used.
   */
  protected def defaultClasspath = sys.env.getOrElse("CLASSPATH", ".")

  /** Enabled under -Xexperimental. */
  protected def experimentalSettings = List[BooleanSetting](YmethodInfer, overrideObjects, overrideVars)

  /** Enabled under -Xfuture. */
  protected def futureSettings = List[BooleanSetting]()

  /** Enabled under -optimise. */
  def optimiseSettings = List[BooleanSetting](inline, inlineHandlers, Xcloselim, Xdce, YconstOptimization)

  /** Internal use - syntax enhancements. */
  private class EnableSettings[T <: BooleanSetting](val s: T) {
    def enabling(toEnable: List[BooleanSetting]): s.type = s withPostSetHook (_ => toEnable foreach (_.value = s.value))
    def disabling(toDisable: List[BooleanSetting]): s.type = s withPostSetHook (_ => toDisable foreach (_.value = !s.value))
    def andThen(f: s.T => Unit): s.type                  = s withPostSetHook (setting => f(setting.value))
  }
  private implicit def installEnableSettings[T <: BooleanSetting](s: T) = new EnableSettings(s)

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
  val nospecialization = BooleanSetting    ("-no-specialization", "Ignore @specialize annotations.")
  val language      = MultiStringSetting("-language", "feature", "Enable one or more language features.")

  /**
   * -X "Advanced" settings
   */
  val Xhelp         = BooleanSetting    ("-X", "Print a synopsis of advanced options.")
  val checkInit     = BooleanSetting    ("-Xcheckinit", "Wrap field accessors to throw an exception on uninitialized access.")
  val developer     = BooleanSetting    ("-Xdev", "Indicates user is a developer - issue warnings about anything which seems amiss")
  val noassertions  = BooleanSetting    ("-Xdisable-assertions", "Generate no assertions or assumptions.")
  val elidebelow    = IntSetting        ("-Xelide-below", "Calls to @elidable methods are omitted if method priority is lower than argument",
                                                elidable.MINIMUM, None, elidable.byName get _)
  val noForwarders  = BooleanSetting    ("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val genPhaseGraph = StringSetting     ("-Xgenerate-phase-graph", "file", "Generate the phase graphs (outputs .dot files) to fileX.dot.", "")
  val XlogImplicits = BooleanSetting    ("-Xlog-implicits", "Show more detail on why some implicits are not applicable.")
  val logImplicitConv = BooleanSetting  ("-Xlog-implicit-conversions", "Print a message whenever an implicit conversion is inserted.")
  val logReflectiveCalls = BooleanSetting("-Xlog-reflective-calls", "Print a message when a reflective method call is generated")
  val logFreeTerms  = BooleanSetting    ("-Xlog-free-terms", "Print a message when reification creates a free term.")
  val logFreeTypes  = BooleanSetting    ("-Xlog-free-types", "Print a message when reification resorts to generating a free type.")
  val maxClassfileName = IntSetting     ("-Xmax-classfile-name", "Maximum filename length for generated classes", 255, Some((72, 255)), _ => None)
  val Xmigration    = ScalaVersionSetting("-Xmigration", "version", "Warn about constructs whose behavior may have changed since version.", AnyScalaVersion)
  val nouescape     = BooleanSetting    ("-Xno-uescape", "Disable handling of \\u unicode escapes.")
  val Xnojline      = BooleanSetting    ("-Xnojline", "Do not use JLine for editing.")
  val Xverify       = BooleanSetting    ("-Xverify", "Verify generic signatures in generated bytecode (asm backend only.)")
  val plugin        = MultiStringSetting("-Xplugin", "file", "Load one or more plugins from files.")
  val disable       = MultiStringSetting("-Xplugin-disable", "plugin", "Disable the given plugin(s).")
  val showPlugins   = BooleanSetting    ("-Xplugin-list", "Print a synopsis of loaded plugins.")
  val require       = MultiStringSetting("-Xplugin-require", "plugin", "Abort unless the given plugin(s) are available.")
  val pluginsDir    = StringSetting     ("-Xpluginsdir", "path", "Path to search compiler plugins.", Defaults.scalaPluginPath)
  val Xprint        = PhasesSetting     ("-Xprint", "Print out program after")
  val writeICode    = PhasesSetting     ("-Xprint-icode", "Log internal icode to *.icode files after", "icode")
  val Xprintpos     = BooleanSetting    ("-Xprint-pos", "Print tree positions, as offsets.")
  val printtypes    = BooleanSetting    ("-Xprint-types", "Print tree types (debugging option).")
  val prompt        = BooleanSetting    ("-Xprompt", "Display a prompt after each error (debugging option).")
  val resident      = BooleanSetting    ("-Xresident", "Compiler stays resident: read source filenames from standard input.")
  val script        = StringSetting     ("-Xscript", "object", "Treat the source file as a script and wrap it in a main method.", "")
  val mainClass     = StringSetting     ("-Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val Xshowcls      = StringSetting     ("-Xshow-class", "class", "Show internal representation of class.", "")
  val Xshowobj      = StringSetting     ("-Xshow-object", "object", "Show internal representation of object.", "")
  val showPhases    = BooleanSetting    ("-Xshow-phases", "Print a synopsis of compiler phases.")
  val sourceReader  = StringSetting     ("-Xsource-reader", "classname", "Specify a custom method for reading source files.", "")

  val XnoPatmatAnalysis = BooleanSetting ("-Xno-patmat-analysis", "Don't perform exhaustivity/unreachability analysis. Also, ignore @switch annotation.")
  val XfullLubs     = BooleanSetting    ("-Xfull-lubs", "Retains pre 2.10 behavior of less aggressive truncation of least upper bounds.")

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
  val Xcloselim       = BooleanSetting    ("-Yclosure-elim", "Perform closure elimination.")
  val YconstOptimization  = BooleanSetting    ("-Yconst-opt", "Perform optimization with constant values.")
  val Ycompacttrees   = BooleanSetting    ("-Ycompact-trees", "Use compact tree printer when displaying trees.")
  val noCompletion    = BooleanSetting    ("-Yno-completion", "Disable tab-completion in the REPL.")
  val Xdce            = BooleanSetting    ("-Ydead-code", "Perform dead code elimination.")
  val debug           = BooleanSetting    ("-Ydebug", "Increase the quantity of debugging output.")
  //val doc           = BooleanSetting    ("-Ydoc", "Generate documentation")
  val termConflict    = ChoiceSetting     ("-Yresolve-term-conflict", "strategy", "Resolve term conflicts", List("package", "object", "error"), "error")
  val inline          = BooleanSetting    ("-Yinline", "Perform inlining when possible.")
  val inlineHandlers  = BooleanSetting    ("-Yinline-handlers", "Perform exception handler inlining when possible.")
  val YinlinerWarnings= BooleanSetting    ("-Yinline-warnings", "Emit inlining warnings. (Normally surpressed due to high volume)")
  val Xlinearizer     = ChoiceSetting     ("-Ylinearizer", "which", "Linearizer to use", List("normal", "dfs", "rpo", "dump"), "rpo")
  val log             = PhasesSetting     ("-Ylog", "Log operations during")
  val Ylogcp          = BooleanSetting    ("-Ylog-classpath", "Output information about what classpath is being applied.")
  val Ynogenericsig   = BooleanSetting    ("-Yno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val noimports       = BooleanSetting    ("-Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
  val nopredef        = BooleanSetting    ("-Yno-predef", "Compile without importing Predef.")
  val noAdaptedArgs   = BooleanSetting    ("-Yno-adapted-args", "Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.")
  val Yrecursion      = IntSetting        ("-Yrecursion", "Set recursion depth used when locking symbols.", 0, Some((0, Int.MaxValue)), (_: String) => None)
  val selfInAnnots    = BooleanSetting    ("-Yself-in-annots", "Include a \"self\" identifier inside of annotations.")
  val Xshowtrees      = BooleanSetting    ("-Yshow-trees", "(Requires -Xprint:) Print detailed ASTs in formatted form.")
  val XshowtreesCompact
                      = BooleanSetting    ("-Yshow-trees-compact", "(Requires -Xprint:) Print detailed ASTs in compact form.")
  val XshowtreesStringified
                      = BooleanSetting    ("-Yshow-trees-stringified", "(Requires -Xprint:) Print stringifications along with detailed ASTs.")
  val Yshowsyms       = BooleanSetting    ("-Yshow-syms", "Print the AST symbol hierarchy after each phase.")
  val Yshowsymkinds   = BooleanSetting    ("-Yshow-symkinds", "Print abbreviated symbol kinds next to symbol names.")
  val skip            = PhasesSetting     ("-Yskip", "Skip")
  val Ygenjavap       = StringSetting     ("-Ygen-javap", "dir", "Generate a parallel output directory of .javap files.", "")
  val Ygenasmp        = StringSetting     ("-Ygen-asmp",  "dir", "Generate a parallel output directory of .asmp files (ie ASM Textifier output).", "")
  val Ydumpclasses    = StringSetting     ("-Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val Ynosqueeze      = BooleanSetting    ("-Yno-squeeze", "Disable creation of compact code in matching.")
  val Ystatistics     = BooleanSetting    ("-Ystatistics", "Print compiler statistics.") andThen (scala.reflect.internal.util.Statistics.enabled = _)
  val stopAfter       = PhasesSetting     ("-Ystop-after", "Stop after") withAbbreviation ("-stop") // backward compat
  val stopBefore      = PhasesSetting     ("-Ystop-before", "Stop before")
  val Yrangepos       = BooleanSetting    ("-Yrangepos", "Use range positions for syntax trees.")
  val Ymemberpos      = StringSetting     ("-Yshow-member-pos", "output style", "Show start and end positions of members", "") withPostSetHook (_ => Yrangepos.value = true)
  val Yreifycopypaste = BooleanSetting    ("-Yreify-copypaste", "Dump the reified trees in copypasteable representation.")
  val Yreplsync       = BooleanSetting    ("-Yrepl-sync", "Do not use asynchronous code for repl startup")
  val Yreploutdir     = StringSetting     ("-Yrepl-outdir", "path", "Write repl-generated classfiles to given output directory (use \"\" to generate a temporary dir)" , "")
  val YmethodInfer    = BooleanSetting    ("-Yinfer-argument-types", "Infer types for arguments of overriden methods.")
  val etaExpandKeepsStar = BooleanSetting ("-Yeta-expand-keeps-star", "Eta-expand varargs methods to T* rather than Seq[T].  This is a temporary option to ease transition.").
                                          withDeprecationMessage("This flag is scheduled for removal in 2.12. If you have a case where you need this flag then please report a bug.")
  val Yinvalidate     = StringSetting     ("-Yinvalidate", "classpath-entry", "Invalidate classpath entry before run", "")
  val YvirtClasses    = false // too embryonic to even expose as a -Y //BooleanSetting    ("-Yvirtual-classes", "Support virtual classes")
  val YdisableUnreachablePrevention = BooleanSetting("-Ydisable-unreachable-prevention", "Disable the prevention of unreachable blocks in code generation.")

  val exposeEmptyPackage = BooleanSetting("-Yexpose-empty-package", "Internal only: expose the empty package.").internalOnly()

  /** Area-specific debug output.
   */
  val Ydocdebug               = BooleanSetting("-Ydoc-debug", "Trace all scaladoc activity.")
  val Yidedebug               = BooleanSetting("-Yide-debug", "Generate, validate and output trees using the interactive compiler.")
  val Yinferdebug             = BooleanSetting("-Yinfer-debug", "Trace type inference and implicit search.")
  val Yissuedebug             = BooleanSetting("-Yissue-debug", "Print stack traces when a context issues an error.")
  val YmacrodebugLite         = BooleanSetting("-Ymacro-debug-lite", "Trace essential macro-related activities.")
  val YmacrodebugVerbose      = BooleanSetting("-Ymacro-debug-verbose", "Trace all macro-related activities: compilation, generation of synthetics, classloading, expansion, exceptions.")
  val Ypmatdebug              = BooleanSetting("-Ypmat-debug", "Trace all pattern matcher activity.")
  val Yposdebug               = BooleanSetting("-Ypos-debug", "Trace position validation.")
  val Yreifydebug             = BooleanSetting("-Yreify-debug", "Trace reification.")
  val Ytyperdebug             = BooleanSetting("-Ytyper-debug", "Trace all type assignments.")
  val Ypatmatdebug            = BooleanSetting("-Ypatmat-debug", "Trace pattern matching translation.")

  /** Groups of Settings.
   */
  val future        = BooleanSetting("-Xfuture", "Turn on future language features.") enabling futureSettings
  val optimise      = BooleanSetting("-optimise", "Generates faster bytecode by applying optimisations to the program") withAbbreviation "-optimize" enabling optimiseSettings
  val nooptimise    = BooleanSetting("-Ynooptimise", "Clears all the flags set by -optimise. Useful for testing optimizations in isolation.") withAbbreviation "-Ynooptimize" disabling optimise::optimiseSettings
  val Xexperimental = BooleanSetting("-Xexperimental", "Enable experimental extensions.") enabling experimentalSettings

  /**
   * Settings motivated by GenBCode
   */
  val Ybackend = ChoiceSetting ("-Ybackend", "choice of bytecode emitter", "Choice of bytecode emitter.",
                                List("GenASM", "GenBCode"),
                                "GenASM")
  // Feature extensions
  val XmacroSettings          = MultiStringSetting("-Xmacro-settings", "option", "Custom settings for macros.")

  /**
   * IDE-specific settings
   */
  val YpresentationVerbose = BooleanSetting("-Ypresentation-verbose", "Print information about presentation compiler tasks.")
  val YpresentationDebug   = BooleanSetting("-Ypresentation-debug",  "Enable debugging output for the presentation compiler.")
  val YpresentationStrict  = BooleanSetting("-Ypresentation-strict", "Do not report type errors in sources with syntax errors.")

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

  /**
   * Helper utilities for use by checkConflictingSettings()
   */
  def isBCodeActive   = !isICodeAskedFor
  def isBCodeAskedFor = (Ybackend.value != "GenASM")
  def isICodeAskedFor = ((Ybackend.value == "GenASM") || optimiseSettings.exists(_.value) || writeICode.isSetByUser)

}
