/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package settings

import io.AbstractFile
import util.{ ClassPath, SourceFile, CommandLineParser }
import annotation.elidable
import scala.tools.util.{ PathResolver, StringOps }
import scala.collection.mutable.{ HashSet, ListBuffer }
import scala.collection.immutable.TreeSet
import interpreter.{ returning }

trait ScalaSettings extends AbsScalaSettings with StandardScalaSettings {
  self: MutableSettings =>

  import PathResolver.{ Defaults, Environment }

  /** Set of settings */
  protected lazy val allSettings = HashSet[Setting]()

  /** Disable a setting */
  def disable(s: Setting) = allSettings -= s

  /**
   *  Temporary Settings
   */
  val suppressVTWarn = BooleanSetting    ("-Ysuppress-vt-typer-warnings", "Suppress warnings from the typer when testing the virtual class encoding, NOT FOR FINAL!")
  val javaignorecp   = BooleanSetting    ("-javaignorecp", "Does nothing - is being removed.")  // !!! marked for death, but need new starr.

  /**
   *  Standard settings
   */
  // argfiles is only for the help message
  val argfiles      = BooleanSetting    ("@<file>", "A text file containing compiler arguments (options and source files)")
  val classpath     = PathSetting       ("-classpath", "path", "Specify where to find user class files", ".") .
                                            withAbbreviation ("-cp")
  val d             = OutputSetting     (outputDirs, ".")
  val defines       = DefinesSetting()
  val optimise      = BooleanSetting    ("-optimise", "Generates faster bytecode by applying optimisations to the program") .
                                            withAbbreviation("-optimize") .
                                            withPostSetHook(_ => List(inline, Xcloselim, Xdce) foreach (_.value = true))

  /**
   * -X "Advanced" settings
   */
  val Xhelp         = BooleanSetting    ("-X", "Print a synopsis of advanced options")
  val assemname     = StringSetting     ("-Xassem-name", "file", "Name of the output assembly (only relevant with -target:msil)", "").dependsOn(target, "msil")
  val assemrefs     = StringSetting     ("-Xassem-path", "path", "List of assemblies referenced by the program (only relevant with -target:msil)", ".").dependsOn(target, "msil")
  val assemextdirs  = StringSetting     ("-Xassem-extdirs", "dirs", "List of directories containing assemblies, defaults to `lib'", Defaults.scalaLibDir.path).dependsOn(target, "msil")
  val sourcedir     = StringSetting     ("-Xsourcedir", "directory", "When -target:msil, the source folder structure is mirrored in output directory.", ".").dependsOn(target, "msil")
  val checkInit     = BooleanSetting    ("-Xcheckinit", "Add runtime checks on field accessors. Uninitialized accesses result in an exception being thrown.")
  val noassertions  = BooleanSetting    ("-Xdisable-assertions", "Generate no assertions and assumptions")
  val elidebelow    = IntSetting        ("-Xelide-below", "Generate calls to @elidable-marked methods only if method priority is greater than argument.",
                                                elidable.ASSERTION, None, elidable.byName.get(_))
  val Xexperimental = BooleanSetting    ("-Xexperimental", "Enable experimental extensions")
  val noForwarders  = BooleanSetting    ("-Xno-forwarders", "Do not generate static forwarders in mirror classes")
  val future        = BooleanSetting    ("-Xfuture", "Turn on future language features")
  val genPhaseGraph = StringSetting     ("-Xgenerate-phase-graph", "file", "Generate the phase graphs (outputs .dot files) to fileX.dot", "")
  val XlogImplicits = BooleanSetting    ("-Xlog-implicits", "Show more info on why some implicits are not applicable")
  val Xmigration28  = BooleanSetting    ("-Xmigration", "Warn about constructs whose behavior may have changed between 2.7 and 2.8")
  val nouescape     = BooleanSetting    ("-Xno-uescape", "Disables handling of \\u unicode escapes")
  val Xnojline      = BooleanSetting    ("-Xnojline", "Do not use JLine for editing")
  val plugin        = MultiStringSetting("-Xplugin", "file", "Load a plugin from a file")
  val disable       = MultiStringSetting("-Xplugin-disable", "plugin", "Disable a plugin")
  val showPlugins   = BooleanSetting    ("-Xplugin-list", "Print a synopsis of loaded plugins")
  val require       = MultiStringSetting("-Xplugin-require", "plugin", "Abort unless a plugin is available")
  val pluginsDir    = StringSetting     ("-Xpluginsdir", "path", "Path to search compiler plugins", Defaults.scalaPluginPath)
  val Xprint        = PhasesSetting     ("-Xprint", "Print out program after")
  val writeICode    = BooleanSetting    ("-Xprint-icode", "Log internal icode to *.icode files")
  val Xprintpos     = BooleanSetting    ("-Xprint-pos", "Print tree positions (as offsets)")
  val printtypes    = BooleanSetting    ("-Xprint-types", "Print tree types (debugging option)")
  val prompt        = BooleanSetting    ("-Xprompt", "Display a prompt after each error (debugging option)")
  val resident      = BooleanSetting    ("-Xresident", "Compiler stays resident, files to compile are read from standard input")
  val script        = StringSetting     ("-Xscript", "object", "Compile as a script, wrapping the code into object.main()", "")
  val Xshowcls      = StringSetting     ("-Xshow-class", "class", "Show class info", "")
  val Xshowobj      = StringSetting     ("-Xshow-object", "object", "Show object info", "")
  val showPhases    = BooleanSetting    ("-Xshow-phases", "Print a synopsis of compiler phases")
  val sourceReader  = StringSetting     ("-Xsource-reader", "classname", "Specify a custom method for reading source files", "scala.tools.nsc.io.SourceReader")

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
  val Yhelp         = BooleanSetting    ("-Y", "Print a synopsis of private options")
  val browse        = PhasesSetting     ("-Ybrowse", "Browse the abstract syntax tree after")
  val check         = PhasesSetting     ("-Ycheck", "Check the tree at the end of")
  val Xcloselim     = BooleanSetting    ("-Yclosure-elim", "Perform closure elimination")
  val Ycompacttrees = BooleanSetting    ("-Ycompact-trees", "Use compact tree printer when displaying trees")
  val noCompletion  = BooleanSetting    ("-Yno-completion", "Disable tab-completion in the REPL")
  val Xdce          = BooleanSetting    ("-Ydead-code", "Perform dead code elimination")
  val debug         = BooleanSetting    ("-Ydebug", "Output debugging messages")
  val Xdetach       = BooleanSetting    ("-Ydetach", "Perform detaching of remote closures")
  // val doc           = BooleanSetting    ("-Ydoc", "Generate documentation")
  val inline        = BooleanSetting    ("-Yinline", "Perform inlining when possible")
  val Xlinearizer   = ChoiceSetting     ("-Ylinearizer", "Linearizer to use", List("normal", "dfs", "rpo", "dump"), "rpo") .
                                          withHelpSyntax("-Ylinearizer:<which>")
  val log           = PhasesSetting     ("-Ylog", "Log operations in")
  val Ylogcp        = BooleanSetting    ("-Ylog-classpath", "Output information about what classpath is being applied.")
  val Ynogenericsig = BooleanSetting    ("-Yno-generic-signatures", "Suppress generation of generic signatures for Java")
  val noimports     = BooleanSetting    ("-Yno-imports", "Compile without any implicit imports")
  val nopredefs     = BooleanSetting    ("-Yno-predefs", "Compile without any implicit predefined values")
  val Yrecursion    = IntSetting        ("-Yrecursion", "Recursion depth used when locking symbols", 0, Some(0, Int.MaxValue), (_: String) => None)
  val selfInAnnots  = BooleanSetting    ("-Yself-in-annots", "Include a \"self\" identifier inside of annotations")
  val Xshowtrees    = BooleanSetting    ("-Yshow-trees", "Show detailed trees when used in connection with -print:phase")
  val skip          = PhasesSetting     ("-Yskip", "Skip")
  val Xsqueeze      = ChoiceSetting     ("-Ysqueeze", "if on, creates compact code in matching", List("on","off"), "on") .
                                          withHelpSyntax("-Ysqueeze:<enabled>")
  val Ystatistics   = BooleanSetting    ("-Ystatistics", "Print compiler statistics")
  val stop          = PhasesSetting     ("-Ystop", "Stop after phase")
  val refinementMethodDispatch =
                      ChoiceSetting     ("-Ystruct-dispatch", "Selects dispatch method for structural refinement method calls",
                        List("no-cache", "mono-cache", "poly-cache", "invoke-dynamic"), "poly-cache") .
                        withHelpSyntax("-Ystruct-dispatch:<method>")
  val specialize    = BooleanSetting    ("-Yspecialize", "Specialize generic code on types.")
  val Yrangepos     = BooleanSetting    ("-Yrangepos", "Use range positions for syntax trees.")
  val Yidedebug     = BooleanSetting    ("-Yide-debug", "Generate, validate and output trees using the interactive compiler.")
  val Ybuilderdebug = ChoiceSetting     ("-Ybuilder-debug", "Compile using the specified build manager", List("none", "refined", "simple"), "none") .
                        withHelpSyntax("-Ybuilder-debug:<method>")
  val Ybuildmanagerdebug =
                      BooleanSetting    ("-Ybuild-manager-debug", "Generate debug information for the Refined Build Manager compiler.")
  val Ytyperdebug   = BooleanSetting    ("-Ytyper-debug", "Trace all type assignements")
  val Ypmatdebug    = BooleanSetting    ("-Ypmat-debug", "Trace all pattern matcher activity.")
  val Yrepldebug    = BooleanSetting    ("-Yrepl-debug", "Trace all repl activity.")
  val Ypmatnaive    = BooleanSetting    ("-Ypmat-naive", "Desugar matches as naively as possible..")
  val Ytailrec      = BooleanSetting    ("-Ytailrecommend", "Alert methods which would be tail-recursive if private or final.")
  val Yjenkins      = BooleanSetting    ("-Yjenkins-hashCodes", "Use jenkins hash algorithm for case class generated hashCodes.")

  // Warnings
  val Ywarnfatal    = BooleanSetting    ("-Yfatal-warnings", "Fail the compilation if there are any warnings.")
  val Xwarninit     = BooleanSetting    ("-Xwarninit", "Warn about possible changes in initialization semantics")
  val Xchecknull    = BooleanSetting    ("-Xcheck-null", "Emit warning on selection of nullable reference")
  val Xwarndeadcode = BooleanSetting    ("-Ywarn-dead-code", "Emit warnings for dead code")
  val YwarnShadow   = BooleanSetting    ("-Ywarn-shadowing", "Emit warnings about possible variable shadowing.")
  val YwarnCatches  = BooleanSetting    ("-Ywarn-catches", "Emit warnings about catch blocks which catch everything.")
  val Xwarnings     = BooleanSetting    ("-Xstrict-warnings", "Emit warnings about lots of things.") .
                          withPostSetHook(_ =>
                            List(YwarnShadow, YwarnCatches, Xwarndeadcode, Xwarninit) foreach (_.value = true)
                          )
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
