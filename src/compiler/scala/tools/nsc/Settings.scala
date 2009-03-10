/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.File
import java.lang.System

class Settings(error: String => Unit) {

  def this() = this(Console.println)

  private var allsettings: List[Setting] = List()

  // optionizes a system property
  private def sysprop(name: String): Option[String] =
    System.getProperty(name) match {
      case null | ""  => None
      case x          => Some(x)
    }

  // given any number of possible path segments, flattens down to a
  // :-separated style path
  protected def concatPath(segments: Option[String]*): String =
    segments.toList.flatMap(x => x) mkString File.pathSeparator

  protected val classpathDefault =
    sysprop("env.classpath") orElse sysprop("java.class.path") getOrElse ""

  protected val bootclasspathDefault =
    concatPath(sysprop("sun.boot.class.path"), guessedScalaBootClassPath)

  protected val extdirsDefault =
    concatPath(sysprop("java.ext.dirs"), guessedScalaExtDirs)

  protected val pluginsDirDefault =
    guess(List("misc", "scala-devel", "plugins"), _.isDirectory) getOrElse ""

  def onull[T <: AnyRef](x: T): Option[T] = if (x eq null) None else Some(x)
  def mkPath(base: String, segments: String*) = new File(base, segments.mkString(File.separator))
  def scalaHome: Option[String] = onull(Properties.scalaHome)

  // examine path relative to scala home and return Some(path) if it meets condition
  private def guess(xs: List[String], cond: (File) => Boolean): Option[String] = {
    if (scalaHome.isEmpty) return None
    val f = mkPath(scalaHome.get, xs: _*)
    if (cond(f)) Some(f.getAbsolutePath) else None
  }

  private def guessedScalaBootClassPath: Option[String] =
    guess(List("lib", "scala-library.jar"), _.isFile) orElse
    guess(List("classes", "library"), _.isDirectory)

  private def guessedScalaExtDirs: Option[String] =
    guess(List("lib"), _.isDirectory)

  val debuginfo     = new DebugSetting  ("-g", "Specify level of generated debugging info", List("none", "source", "line", "vars", "notailcalls"), "vars", "vars")
  val nowarnings    = BooleanSetting    ("-nowarn", "Generate no warnings").hideToIDE
  val verbose       = BooleanSetting    ("-verbose", "Output messages about what the compiler is doing").hideToIDE
  val deprecation   = BooleanSetting    ("-deprecation", "Output source locations where deprecated APIs are used").hideToIDE
  val unchecked     = BooleanSetting    ("-unchecked", "Enable detailed unchecked warnings").hideToIDE
  val classpath     = (new StringSetting ("-classpath", "path", "Specify where to find user class files", classpathDefault) { override val abbreviation = "-cp" }).hideToIDE
  val sourcepath    = StringSetting     ("-sourcepath", "path", "Specify where to find input source files", "").hideToIDE
  val bootclasspath = StringSetting     ("-bootclasspath", "path", "Override location of bootstrap class files", bootclasspathDefault).hideToIDE
  val extdirs       = StringSetting     ("-extdirs", "dirs", "Override location of installed extensions", extdirsDefault).hideToIDE
  val outdir        = StringSetting     ("-d", "directory", "Specify where to place generated class files", ".").hideToIDE
  val encoding      = StringSetting     ("-encoding", "encoding", "Specify character encoding used by source files", Properties.encodingString).hideToIDE
  val target        = ChoiceSetting     ("-target", "Specify for which target object files should be built", List("jvm-1.5", "jvm-1.4", "msil"), "jvm-1.5")
  val printLate     = BooleanSetting    ("-print", "Print program with all Scala-specific features removed").hideToIDE
  val XO            = BooleanSetting    ("-optimise", "Generates faster bytecode by applying optimisations to the program")
  val explaintypes  = BooleanSetting    ("-explaintypes", "Explain type errors in more detail").hideToIDE
  val uniqid        = BooleanSetting    ("-uniqid", "Print identifiers with unique names for debugging").hideToIDE
  val version       = BooleanSetting    ("-version", "Print product version and exit").hideToIDE
  val dependenciesFile  = StringSetting ("-dependencyfile", "file", "Specify the file in which dependencies are tracked", ".scala_dependencies")
  val make          = ChoiceSetting     ("-make", "Specify the behaviour for selecting which files need to be recompiled", List("all", "changed", "immediate", "transitive"), "all")
  val help          = BooleanSetting    ("-help", "Print a synopsis of standard options").hideToIDE
  val Xhelp         = BooleanSetting    ("-X", "Print a synopsis of advanced options").hideToIDE
  val argfiles      = BooleanSetting    ("@<file>", "A text file containing compiler arguments (options and source files)") // only for the help message

  val assemname     = StringSetting     ("-Xassem", "file", "Name of the output assembly (only relevant with -target:msil)", "").dependsOn(target, "msil").hideToIDE
  val assemrefs     = StringSetting     ("-Xassem-path", "path", "List of assemblies referenced by the program (only relevant with -target:msil)", ".").dependsOn(target, "msil").hideToIDE
  val Xchecknull    = BooleanSetting    ("-Xcheck-null", "Emit warning on selection of nullable reference")
  val Xwarninit     = BooleanSetting    ("-Xwarninit", "Warn about possible changes in initialization semantics")
  val checkInit     = BooleanSetting    ("-Xcheckinit", "Add runtime checks on field accessors. Uninitialized accesses result in an exception being thrown.")
  val noassertions  = BooleanSetting    ("-Xdisable-assertions", "Generate no assertions and assumptions")
  val Xexperimental = BooleanSetting    ("-Xexperimental", "Enable experimental extensions")
  val XlogImplicits = BooleanSetting    ("-Xlog-implicits", "Show more info on why some implicits are not applicable")
  val Xnojline      = new BooleanSetting("-Xnojline", "Do not use JLine for editing").hideToIDE
  val nouescape     = BooleanSetting    ("-Xno-uescape", "Disables handling of \\u unicode escapes")
  val plugin        = MultiStringSetting("-Xplugin", "file", "Load a plugin from a file")
  val disable       = MultiStringSetting("-Xplugin-disable", "plugin", "Disable a plugin")
  val showPlugins   = BooleanSetting    ("-Xplugin-list", "Print a synopsis of loaded plugins").hideToIDE
  val pluginOptions = new MultiStringSetting("-P", "plugin:opt", "Pass an option to a plugin") { override def helpSyntax = "-P:<plugin>:<opt>" }
  val require       = MultiStringSetting("-Xplugin-require", "plugin", "Abort unless a plugin is available")
  val pluginsDir    = StringSetting     ("-Xpluginsdir", "path", "Location to find compiler plugins", pluginsDirDefault)
  val print         = PhasesSetting     ("-Xprint", "Print out program after")
  val writeICode    = BooleanSetting    ("-Xprint-icode", "Log internal icode to *.icode files").hideToIDE
  val Xprintpos     = BooleanSetting    ("-Xprint-pos", "Print tree positions (as offsets)").hideToIDE
  val printtypes    = BooleanSetting    ("-Xprint-types", "Print tree types (debugging option)").hideToIDE
  val prompt        = BooleanSetting    ("-Xprompt", "Display a prompt after each error (debugging option)").hideToIDE
  val resident      = BooleanSetting    ("-Xresident", "Compiler stays resident, files to compile are read from standard input").hideToIDE
  val Xshowcls      = StringSetting     ("-Xshow-class", "class", "Show class info", "").hideToIDE
  val Xshowobj      = StringSetting     ("-Xshow-object", "object", "Show object info", "").hideToIDE
  val showPhases    = BooleanSetting    ("-Xshow-phases", "Print a synopsis of compiler phases").hideToIDE
  val genPhaseGraph = StringSetting     ("-Xgenerate-phase-graph", "filename", "Generate the phase graphs (outputs .dot files) to filenameX.dot", "").hideToIDE
  val sourceReader  = StringSetting     ("-Xsource-reader", "classname", "Specify a custom method for reading source files", "scala.tools.nsc.io.SourceReader").hideToIDE
  val future        = BooleanSetting    ("-Xfuture", "Turn on future language features")

  val Yhelp         = BooleanSetting    ("-Y", "Print a synopsis of private options").hideToIDE
  val browse        = PhasesSetting     ("-Ybrowse", "Browse the abstract syntax tree after")
  val check         = PhasesSetting     ("-Ycheck", "Check the tree at the end of")
  val Xcloselim     = BooleanSetting    ("-Yclosure-elim", "Perform closure elimination")
  val Xcodebase     = StringSetting     ("-Ycodebase", "codebase", "Specify the URL containing the Scala libraries", "").hideToIDE
  val debug         = BooleanSetting    ("-Ydebug", "Output debugging messages").hideToIDE
  val debugger      = BooleanSetting    ("-Ydebugger", "Enable interactive debugger").hideToIDE
  val Xdce          = BooleanSetting    ("-Ydead-code", "Perform dead code elimination")
  val Xdetach       = BooleanSetting    ("-Ydetach", "Perform detaching of remote closures")
//  val doc           = BooleanSetting    ("-Ydoc", "Generate documentation").hideToIDE
  val inline        = BooleanSetting    ("-Yinline", "Perform inlining when possible")
  val Xlinearizer   = ChoiceSetting     ("-Ylinearizer", "Linearizer to use", List("normal", "dfs", "rpo", "dump"), "rpo")
  val log           = PhasesSetting     ("-Ylog", "Log operations in")
  val noimports     = BooleanSetting    ("-Yno-imports", "Compile without any implicit imports")
  val nopredefs     = BooleanSetting    ("-Yno-predefs", "Compile without any implicit predefined values")
  val Yrecursion    = IntSetting        ("-Yrecursion", "Recursion depth used when locking symbols", 0, Some(0), None).hideToIDE
  val script        = StringSetting     ("-Xscript", "object", "Compile as a script, wrapping the code into object.main()", "").hideToIDE

  val Xshowtrees    = BooleanSetting    ("-Yshow-trees", "Show detailed trees when used in connection with -print:phase").hideToIDE
  val skip          = PhasesSetting     ("-Yskip", "Skip")
  val completion    = BooleanSetting    ("-Ycompletion", "Enable tab-completion in the REPL").hideToIDE
  val Xsqueeze      = ChoiceSetting     ("-Ysqueeze", "if on, creates compact code in matching", List("on","on","off"), "on").hideToIDE
  val statistics    = BooleanSetting    ("-Ystatistics", "Print compiler statistics").hideToIDE
  val stop          = PhasesSetting     ("-Ystop", "Stop after phase")
  val refinementMethodDispatch
                    = ChoiceSetting     ("-Ystruct-dispatch", "Selects what dispatch method to use for structural refinement method calls", List("no-cache", "mono-cache", "poly-cache"), "poly-cache").hideToIDE
  val Xwarndeadcode = BooleanSetting    ("-Ywarn-dead-code", "Emit warnings for dead code")
  val Ynogenericsig = BooleanSetting    ("-Yno-generic-signatures", "Suppress generation of generic signatures for Java")

  val XnoVarargsConversion = BooleanSetting("-Xno-varargs-conversion", "disable varags conversion")
  val selfInAnnots = BooleanSetting    ("-Yself-in-annots", "Include a \"self\" identifier inside of annotations")

  val suppressVTWarn = BooleanSetting    ("-Ysuppress-vt-typer-warnings", "Suppress warnings from the typer when testing the virtual class encoding, NOT FOR FINAL!")

  /** A list of all settings */
  def allSettings: List[Setting] = allsettings.reverse
  /** Disable a setting */
  def disable(s: Setting) = {
    allsettings = allsettings filter (s !=)
  }

  override def equals(that: Any) = that match {
    case s: Settings  => this.allSettings == s.allSettings
    case _            => false
  }

  def checkDependencies: Boolean = {
    def hasValue(s: Setting, value: String): Boolean = s match {
      case bs: BooleanSetting => bs.value
      case ss: StringSetting  => ss.value == value
      case cs: ChoiceSetting  => cs.value == value
      case _ => "" == value
    }
    var ok = true
    for (setting <- allsettings if !setting.dependency.isEmpty) {
      val (dep, value) = setting.dependency.get
      if (! (setting.isDefault || hasValue(dep, value))) {
        error("incomplete option " + setting.name + " (requires " + dep.name + ")")
        ok = false
      }
    }
    ok
  }

  /** Try to add additional command line parameters. */
  def parseParams(line: String, error: String => Nothing) {
    var args =
      if (line.trim() == "") Nil
      else List.fromArray(line.trim().split(" ")).map(_.trim())
    while (!args.isEmpty) {
      val argsBuf = args
      if (args.head startsWith "-") {
        for (setting <- allSettings)
          args = setting.tryToSet(args);
      }
      else error("Parameter '" + args.head + "' does not start with '-'.")
      if (argsBuf eq args)
        error("Parameter '" + args.head + "' is not recognised by Scalac.")
    }
  }

  /** A base class for settings of all types.
   *  Subclasses each define a `value' field of the appropriate type.
   */
  abstract class Setting(descr: String) {

    /** The name of the option as written on the command line, '-' included. */
    def name: String

    /** If first arg defines this setting, consume it as well as all following
     *  args needed to define the setting. If this can be done without
     *  error, set value field and return suffix of args else
     *  issue error message and return the arguments unchanged.
     *  If first arg does not define this setting return args unchanged.
     */
    def tryToSet(args: List[String]): List[String]

    /** The syntax defining this setting in a help string */
    def helpSyntax: String = name

    /** A description of the purpose of this setting in a help string */
    def helpDescription = descr

    /** Return a list of strings that can be used to recreate
      * the receiver's current setting.
      */
    def unparse: List[String]

    /** override if option should be hidden from IDE.
      */
    var hiddenToIDE: Boolean = false
    def hideToIDE: this.type = {
      hiddenToIDE = true
      this
    }
    def showToIDE: this.type = {
      hiddenToIDE = false
      this
    }

    protected var setByUser: Boolean = false
    def isDefault: Boolean = !setByUser

    protected[Settings] var dependency: Option[(Setting, String)] = None
    def dependsOn(s: Setting, value: String): this.type = { dependency = Some((s, value)); this }
    def dependsOn(s: Setting): this.type = dependsOn(s, "")

    def isStandard: Boolean = !isAdvanced && !isPrivate && !(name eq "-Y")
    def isAdvanced: Boolean =
      (name startsWith "-X") && !(name eq "-X")
    def isPrivate: Boolean =
      (name == "-P") || ((name startsWith "-Y") && !(name eq "-Y"))
    def equalLists[T <% Ordered[T]](xs: List[T], ys: List[T]) = xs.sort(_ < _) == ys.sort(_ < _)

/*
    def isDocOption: Boolean =
      !dependency.isEmpty && dependency.get._1 == doc
*/
    // initialization
    allsettings = this :: allsettings
  }

  /** A setting represented by a positive integer */
  case class IntSetting(name: String, descr: String, default: Int, min: Option[Int], max: Option[Int]) extends Setting(descr) {
    // Validate that min and max are consistent
    (min, max) match {
      case (Some(i), Some(j)) => assert(i <= j)
      case _ => ()
    }

    // Helper to validate an input
    private def isInputValid(k: Int): Boolean =
      (min, max) match {
        case (Some(i), Some(j)) => (i <= k) && (k <= j)
        case (Some(i), None) => (i <= k)
        case (None, Some(j)) => (k <= j)
        case _ => true
      }

    // Helper to generate a textual explaination of valid inputs
    private def getValidText: String =
      (min, max) match {
        case (Some(i), Some(j)) => "must be between "+i+" and "+j
        case (Some(i), None) => "must be greater than or equal to "+i
        case (None, Some(j)) => "must be less than or equal to "+j
        case _ => throw new Error("this should never be used")
      }

    // Ensure that the default value is actually valid
    assert(isInputValid(default))

    protected var v: Int = default

    def errorMsg = error("invalid setting for -"+name+" "+getValidText)

    def value: Int = this.v
    def value_=(s: Int) {
      if (!isInputValid(s)) errorMsg
      setByUser = true;
      this.v = s
    }

    def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (name == n) =>
        if (rest.isEmpty) {
          error("missing argument")
          args
        } else {
          try {
            value = rest.head.toInt
        } catch {
            case e: java.lang.NumberFormatException => errorMsg
        }
          rest.tail
        }
      case _ => args
    }

    def unparse: List[String] =
      if (value == default) Nil else List(name, value.toString)

    override def equals(that: Any) = that match {
      case is: Settings#IntSetting => this.name == is.name && this.value == is.value
      case _ => false
    }

  }

  /** A setting represented by a boolean flag (false, unless set) */
  case class BooleanSetting(name: String, descr: String) extends Setting(descr) {
    protected var v: Boolean = false

    def value: Boolean = this.v
    def value_=(s: Boolean) { setByUser = true; this.v = s }

    def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (n == name) => value = true; rest
      case _ => args
    }

    def unparse: List[String] = if (value) List(name) else Nil

    override def equals(that: Any) = that match {
      case bs:  Settings#BooleanSetting => this.name == bs.name && this.value == bs.value
      case _ => false
    }

  }

  /** A setting represented by a string, (`default' unless set) */
  case class StringSetting(name: String, arg: String, descr: String, default: String)
  extends Setting(descr) {
    def abbreviation: String = null

    protected var v: String = default

    def value: String = this.v
    def value_=(s: String) { setByUser = true; this.v = s }

    def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (name == n || abbreviation == n) =>
        if (rest.isEmpty) {
          error("missing argument")
          args
        } else {
          value = rest.head
          rest.tail
        }
      case _ => args
    }

    override def helpSyntax = name + " <" + arg + ">"

    def unparse: List[String] =
      if (value == default) Nil else List(name, value)

    override def equals(that: Any) = that match {
      case ss: Settings# StringSetting => this.name == ss.name && this.value == ss.value
      case _ => false
    }

  }

  /** A setting that accumulates all strings supplied to it */
  case class MultiStringSetting(name: String, arg: String, descr: String)
  extends Setting(descr) {
    hideToIDE
    protected var v: List[String] = Nil
    def value = v
    def appendToValue(str: String) { v = v ::: List(str) }

    protected val nameColon = name + ":"
    def tryToSet(args: List[String]): List[String] = args match {
      case arg :: rest if (arg startsWith nameColon) =>
        val toadd = arg.substring(nameColon.length())
        if (toadd.length == 0) {
          error("empty argument to " + nameColon)
          args
        } else {
          appendToValue(toadd)
          rest
        }

      case opt :: arg :: rest if (opt == name) =>
        appendToValue(arg)
        rest

      case _ => args
    }

    override def helpSyntax = name + ":<" + arg + ">"

    def unparse: List[String] =
      for (opt <- value) yield nameColon+opt

    override def equals(that: Any) = that match {
      case mss: Settings# MultiStringSetting =>
        this.name == mss.name && equalLists(this.value, mss.value)
      case _ => false
    }

  }

  /** A setting represented by a string in a given set of <code>choices</code>,
   *  (<code>default</code> unless set).
   */
  case class ChoiceSetting(name: String, descr: String, choices: List[String], default: String)
  extends Setting(descr + choices.mkString(" (", ",", ")")) {
    protected var v: String = default

    def value: String = this.v
    def value_=(s: String) { setByUser = true; this.v = s }

    protected def argument: String = name.substring(1)

    def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (n startsWith (name + ":")) =>
        val choice = n.substring(name.length() + 1)
        if (!(choices contains choice)) {
          error(
            if (choice == "") "missing " + argument
            else "unknown " + argument + " '" + choice + "'")
          args
        } else {
          value = choice
          rest
        }
      case n :: choice :: rest if n == name => // alternative to be consistent with IDE
        if (!(choices contains choice)) {
          error(
            if (choice == "") "missing " + argument
            else "unknown " + argument + " '" + choice + "'")
          args
        } else {
          value = choice
          rest
        }
      case _ => args
    }

    override def helpSyntax = name + ":<" + argument + ">"

    def unparse: List[String] =
      if (value == default) Nil else List(name + ":" + value)

    override def equals(that: Any) = that match {
      case cs: Settings# ChoiceSetting => this.name == cs.name && this.value == cs.value
      case _ => false
    }

  }

  /** Same as ChoiceSetting but have a <code>level</code> int which tells the
   *  index of the selected choice. The <code>defaultEmpty</code> is used when
   *  this setting is used without specifying any of the available choices.
   */
  class DebugSetting(name: String, descr: String, choices: List[String], default: String, defaultEmpty: String)
  extends ChoiceSetting(name, descr, choices, default) {

    def indexOf[a](xs: List[a], e: a): Option[Int] = xs match {
      case y :: ys => if (e == y) Some(0) else indexOf(ys, e) match {
          case Some(idx) => Some(1 + idx)
          case None => None
        }
      case _ => None
    }
    var level: Int = indexOf(choices, default).get

    override def value_=(choice: String) {
      setByUser = true
      this.v = choice
      this.level = indexOf(choices, choice).get
    }

    override def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (n startsWith (name + ":")) =>
        val choice = n.substring(name.length() + 1)
        if (!(choices contains choice)) {
          error(
              if (choice == "") "missing " + argument
              else "unknown " + argument + " '" + choice + "'")
          args
        } else {
          value = choice
          rest
        }

      case n :: rest if (n startsWith name) =>
        value = defaultEmpty
        rest

      case _ => args
    }
  }

  /** A setting represented by a list of strings which should be prefixes of
   *  phase names. This is not checked here, however.  Alternatively the string
   *  "all" can be used to represent all phases.
   *  (the empty list, unless set)
   */
  case class PhasesSetting(name: String, descr: String)
  extends Setting(descr + " <phase> or \"all\"") {
    hideToIDE
    protected var v: List[String] = List()

    def value: List[String] = this.v
    def value_=(s: List[String]) { setByUser = true; this.v = s }
    def doAllPhases() = value contains "all"

    def tryToSet(args: List[String]): List[String] = args match {
      case n :: rest if (n startsWith (name + ":")) =>
        val phase = n.substring(name.length() + 1)
        if (phase == "") {
          error("missing phase")
          args
        } else {
          value = value ::: List(phase)
          rest
        }
      case _ => args
    }

    override def helpSyntax = name + ":<phase>"

    // we slightly abuse the usual meaning of "contains" here by returning
    // true if our phase list contains "all", regardless of the incoming argument
    def contains(phasename: String): Boolean =
      doAllPhases || (value exists (str => phasename startsWith str))

    def unparse: List[String] = value.map(phase => name + ":" + phase)

    override def equals(that: Any) = that match {
      case ps: Settings#PhasesSetting =>
        (this.name == ps.name) &&
        (this.doAllPhases && ps.doAllPhases || equalLists(this.value, ps.value))
      case _ => false
    }

  }
}
