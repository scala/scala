/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools
package nsc
package settings

import annotation.elidable
import scala.tools.util.PathResolver.Defaults
import scala.collection.mutable.HashSet

trait ScalaSettings extends AbsScalaSettings with StandardScalaSettings {
  self: MutableSettings =>

  import Defaults.scalaUserClassPath

  /** Set of settings */
  protected lazy val allSettings = HashSet[Setting]()

  /** Disable a setting */
  def disable(s: Setting) = allSettings -= s

  /**
   *  Standard settings
   */
  // argfiles is only for the help message
  val argfiles      = BooleanSetting    ("@<file>", "A text file containing compiler arguments (options and source files)")
  val classpath     = PathSetting       ("-classpath", "Specify where to find user class files.", scalaUserClassPath) .
                                            withAbbreviation ("-cp")
  val d             = OutputSetting     (outputDirs, ".")
  val optimise      = BooleanSetting    ("-optimise", "Generates faster bytecode by applying optimisations to the program") .
                                            withAbbreviation("-optimize") .
                                            withPostSetHook(set => List(inline, Xcloselim, Xdce) foreach (_.value = set.value))
  val nospecialization = BooleanSetting    ("-no-specialization", "Ignore @specialize annotations.")

  /**
   * -X "Advanced" settings
   */
  val Xhelp         = BooleanSetting    ("-X", "Print a synopsis of advanced options.")
  val assemname     = StringSetting     ("-Xassem-name", "file", "(Requires -target:msil) Name of the output assembly.", "").dependsOn(target, "msil")
  val assemrefs     = StringSetting     ("-Xassem-path", "path", "(Requires -target:msil) List of assemblies referenced by the program.", ".").dependsOn(target, "msil")
  val assemextdirs  = StringSetting     ("-Xassem-extdirs", "dirs", "(Requires -target:msil) List of directories containing assemblies.  default:lib", Defaults.scalaLibDir.path).dependsOn(target, "msil")
  val sourcedir     = StringSetting     ("-Xsourcedir", "directory", "(Requires -target:msil) Mirror source folder structure in output directory.", ".").dependsOn(target, "msil")
  val checkInit     = BooleanSetting    ("-Xcheckinit", "Wrap field accessors to throw an exception on uninitialized access.")
  val noassertions  = BooleanSetting    ("-Xdisable-assertions", "Generate no assertions or assumptions.")
  val elidebelow    = IntSetting        ("-Xelide-below", "Generate calls to @elidable-marked methods only if method priority is greater than argument.",
                                                elidable.ASSERTION, None, elidable.byName.get(_))
  val noForwarders  = BooleanSetting    ("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val future        = BooleanSetting    ("-Xfuture", "Turn on future language features.")
  val genPhaseGraph = StringSetting     ("-Xgenerate-phase-graph", "file", "Generate the phase graphs (outputs .dot files) to fileX.dot.", "")
  val XlogImplicits = BooleanSetting    ("-Xlog-implicits", "Show more detail on why some implicits are not applicable.")
  val Xmigration28  = BooleanSetting    ("-Xmigration", "Warn about constructs whose behavior may have changed between 2.7 and 2.8.")
  val nouescape     = BooleanSetting    ("-Xno-uescape", "Disable handling of \\u unicode escapes.")
  val Xnojline      = BooleanSetting    ("-Xnojline", "Do not use JLine for editing.")
  val plugin        = MultiStringSetting("-Xplugin", "file", "Load one or more plugins from files.")
  val disable       = MultiStringSetting("-Xplugin-disable", "plugin", "Disable the given plugin(s).")
  val showPlugins   = BooleanSetting    ("-Xplugin-list", "Print a synopsis of loaded plugins.")
  val require       = MultiStringSetting("-Xplugin-require", "plugin", "Abort unless the given plugin(s) are available.")
  val pluginsDir    = StringSetting     ("-Xpluginsdir", "path", "Path to search compiler plugins.", Defaults.scalaPluginPath)
  val Xprint        = PhasesSetting     ("-Xprint", "Print out program after")
  val writeICode    = BooleanSetting    ("-Xprint-icode", "Log internal icode to *.icode files.")
  val Xprintpos     = BooleanSetting    ("-Xprint-pos", "Print tree positions, as offsets.")
  val printtypes    = BooleanSetting    ("-Xprint-types", "Print tree types (debugging option).")
  val prompt        = BooleanSetting    ("-Xprompt", "Display a prompt after each error (debugging option).")
  val resident      = BooleanSetting    ("-Xresident", "Compiler stays resident: read source filenames from standard input.")
  val script        = StringSetting     ("-Xscript", "object", "Treat the source file as a script and wrap it in a main method.", "")
  val Xshowcls      = StringSetting     ("-Xshow-class", "class", "Show internal representation of class.", "")
  val Xshowobj      = StringSetting     ("-Xshow-object", "object", "Show internal representation of object.", "")
  val showPhases    = BooleanSetting    ("-Xshow-phases", "Print a synopsis of compiler phases.")
  val sourceReader  = StringSetting     ("-Xsource-reader", "classname", "Specify a custom method for reading source files.", "")

  val Xwarnfatal    = BooleanSetting    ("-Xfatal-warnings", "Fail the compilation if there are any warnings.")
  val Xwarninit     = BooleanSetting    ("-Xwarninit", "Warn about possible changes in initialization semantics.")
  val Xchecknull    = BooleanSetting    ("-Xcheck-null", "Emit warning on selection of nullable reference.")

  // Experimental Extensions
  val Xexperimental = BooleanSetting    ("-Xexperimental", "Enable experimental extensions.") .
                          withPostSetHook(set => List(YdepMethTpes, YmethodInfer) foreach (_.value = set.value)) //YvirtClasses,

  /** Compatibility stubs for options whose value name did
   *  not previously match the option name.
   */
  def XO = optimise
  def debuginfo = g
  def dependenciesFile = dependencyfile
  def nowarnings = nowarn
  def outdir = d
  def printLate = print

  /**
   * -Y "Private" settings
   */
  val Yhelp         = BooleanSetting    ("-Y", "Print a synopsis of private options.")
  val browse        = PhasesSetting     ("-Ybrowse", "Browse the abstract syntax tree after")
  val check         = PhasesSetting     ("-Ycheck", "Check the tree at the end of")
  val Yshow         = PhasesSetting     ("-Yshow", "(Requires -Xshow-class or -Xshow-object) Show after")
  val Xcloselim     = BooleanSetting    ("-Yclosure-elim", "Perform closure elimination.")
  val Ycompacttrees = BooleanSetting    ("-Ycompact-trees", "Use compact tree printer when displaying trees.")
  val noCompletion  = BooleanSetting    ("-Yno-completion", "Disable tab-completion in the REPL.")
  val Xdce          = BooleanSetting    ("-Ydead-code", "Perform dead code elimination.")
  val debug         = BooleanSetting    ("-Ydebug", "Increase the quantity of debugging output.")
  // val doc           = BooleanSetting    ("-Ydoc", "Generate documentation")
  val inline        = BooleanSetting    ("-Yinline", "Perform inlining when possible.")
  val Xlinearizer   = ChoiceSetting     ("-Ylinearizer", "which", "Linearizer to use", List("normal", "dfs", "rpo", "dump"), "rpo")
  val log           = PhasesSetting     ("-Ylog", "Log operations during")
  val Ylogcp        = BooleanSetting    ("-Ylog-classpath", "Output information about what classpath is being applied.")
  val Ynogenericsig = BooleanSetting    ("-Yno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val Yverifysigs   = BooleanSetting    ("-Yverify-generics", "Validated generated generic signatures.")
  val noimports     = BooleanSetting    ("-Yno-imports", "Compile without any implicit imports.")
  // Not actually doing anything, so disabled.
  // val nopredefs     = BooleanSetting    ("-Yno-predefs", "Compile without any implicit predefined values.")
  val Yprofile      = PhasesSetting     ("-Yprofile", "(Requires jvm -agentpath to contain yjgpagent) Profile CPU usage of given phases.")
  val YprofileMem   = BooleanSetting    ("-Yprofile-memory", "Profile memory, get heap snapshot after each compiler run (requires yjpagent, see above).")
  val YprofileClass = StringSetting     ("-Yprofile-class", "class", "Name of profiler class.", "scala.tools.util.YourkitProfiling")
  val Yrecursion    = IntSetting        ("-Yrecursion", "Set recursion depth used when locking symbols.", 0, Some(0, Int.MaxValue), (_: String) => None)
  val selfInAnnots  = BooleanSetting    ("-Yself-in-annots", "Include a \"self\" identifier inside of annotations.")
  val Xshowtrees    = BooleanSetting    ("-Yshow-trees", "(Requires -Xprint:) Print detailed ASTs.")
  val skip          = PhasesSetting     ("-Yskip", "Skip")
  val Ynosqueeze    = BooleanSetting    ("-Yno-squeeze", "Disable creation of compact code in matching.")
  val Ystatistics   = BooleanSetting    ("-Ystatistics", "Print compiler statistics.") .
                                          withPostSetHook(set => util.Statistics.enabled = set.value)
  val stop          = PhasesSetting     ("-Ystop", "Stop after phase")
  val refinementMethodDispatch =
                      ChoiceSetting     ("-Ystruct-dispatch", "policy", "structural method dispatch policy",
                        List("no-cache", "mono-cache", "poly-cache", "invoke-dynamic"), "poly-cache")
  val Yrangepos     = BooleanSetting    ("-Yrangepos", "Use range positions for syntax trees.")
  val YrichExes     = BooleanSetting    ("-Yrich-exceptions", "More revealing exceptions.  Set SOURCEPATH to java/scala source jars.")
  val Yidedebug     = BooleanSetting    ("-Yide-debug", "Generate, validate and output trees using the interactive compiler.")
  val Ybuilderdebug = ChoiceSetting     ("-Ybuilder-debug", "manager", "Compile using the specified build manager.", List("none", "refined", "simple"), "none")
  val Ybuildmanagerdebug =
                      BooleanSetting    ("-Ybuild-manager-debug", "Generate debug information for the Refined Build Manager compiler.")
  val Ytyperdebug   = BooleanSetting    ("-Ytyper-debug", "Trace all type assignements.")
  val Ypmatdebug    = BooleanSetting    ("-Ypmat-debug", "Trace all pattern matcher activity.")
  val Yrepldebug    = BooleanSetting    ("-Yrepl-debug", "Trace all repl activity.") .
                                          withPostSetHook(set => interpreter._debug = true)
  val Ycompletion   = BooleanSetting    ("-Ycompletion-debug", "Trace all tab completion activity.")
  val Ypmatnaive    = BooleanSetting    ("-Ypmat-naive", "Desugar matches as naively as possible.")
  val Ymurmur       = BooleanSetting    ("-Ymurmur", "Use Murmur hash algorithm for case class generated hashCodes.")
  val Ynotnull      = BooleanSetting    ("-Ynotnull", "Enable (experimental and incomplete) scala.NotNull.")
  val YdepMethTpes  = BooleanSetting    ("-Ydependent-method-types", "Allow dependent method types.")
  val YmethodInfer  = BooleanSetting    ("-Yinfer-argument-types", "Infer types for arguments of overriden methods.")
  val noSelfCheck   = BooleanSetting    ("-Yno-self-type-checks", "Suppress check for self-type conformance among inherited members.")
  val YvirtClasses  = false // too embryonic to even expose as a -Y //BooleanSetting    ("-Yvirtual-classes", "Support virtual classes")

  /**
   * Warnings
   */
  val Ywarndeadcode = BooleanSetting    ("-Ywarn-dead-code", "Emit warnings for dead code")

  /**
   * IDE-specific settings
   */
  val YpresentationVerbose = BooleanSetting("-Ypresentation-verbose", "Print information about presentation compiler tasks.")
  val YpresentationDebug   = BooleanSetting("-Ypresentation-debug",  "Enable debugging output for the presentation compiler.")

  val YpresentationLog     = StringSetting("-Ypresentation-log", "file", "Log presentation comnpiler events into file", "")
  val YpresentationReplay  = StringSetting("-Ypresentation-replay", "file", "Replay presentation comnpiler events from file", "")
  /**
   * "fsc-specific" settings.
   */
  val fscShutdown   = BooleanSetting    ("-shutdown", "Shutdown the fsc daemon")

  /**
   * -P "Plugin" settings
   */
  val pluginOptions = MultiStringSetting("-P", "plugin:opt", "Pass an option to a plugin") .
                        withHelpSyntax("-P:<plugin>:<opt>")
}
