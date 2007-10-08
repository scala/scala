/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.File
import java.lang.System

class Settings(error: String => Unit) {
  def this() = this(Console.println)

  private var allsettings: List[Setting] = List()

  private def getProperty(name: String): String =
    if (System.getProperty(name) != "")
      System.getProperty(name)
    else null

  private val classpathDefault =
    if (System.getProperty("env.classpath") ne null)
      alternatePath(
        getProperty("env.classpath"),
        ".")
    else getProperty("java.class.path")

  private val bootclasspathDefault =
    alternatePath(
      concatPath(
        getProperty("sun.boot.class.path"),
        guessedScalaBootClassPath),
      "")

  private val extdirsDefault =
    alternatePath(
      concatPath(
        getProperty("java.ext.dirs"),
        guessedScalaExtDirs),
      "")

  private val pluginsDirDefault =
    if (Properties.scalaHome == null)
      ""
    else
      new File(
	new File(
	  new File(Properties.scalaHome, "misc"),
	  "scala-devel"),
	"plugins").getAbsolutePath

  private def alternatePath(p1: String, p2: => String) =
    if (p1 ne null) p1 else p2

  private def concatPath(p1: String, p2: String) =
     if ((p1 ne null) && (p2 ne null)) p1 + File.pathSeparator + p2
     else if (p1 ne null) p1
     else p2


  private def guessedScalaBootClassPath = {
    val scalaHome = Properties.scalaHome
    if (scalaHome ne null) {
      val guessJar = new File(new File(new File(scalaHome), "lib"), "scala-library.jar")
      if (guessJar.isFile()) guessJar.getPath()
      else {
        val guessDir = new File(new File(new File(scalaHome), "lib"), "library")
        if (guessDir.isDirectory()) guessDir.getPath() else null
      }
    } else null
  }

  private def guessedScalaExtDirs = {
    val scalaHome = Properties.scalaHome
    if (scalaHome ne null) {
      val guess = new File(new File(scalaHome), "lib")
      if (guess.isDirectory()) guess.getPath else null
    } else null
  }

  private val encodingDefault =
    new java.io.OutputStreamWriter(
      new java.io.ByteArrayOutputStream()).getEncoding

  val debuginfo     = new DebugSetting  ("-g", "Specify level of generated debugging info", List("none", "source", "line", "vars", "notc"), "vars", "vars")
  val nowarnings    = BooleanSetting    ("-nowarn", "Generate no warnings").hideToIDE
  val verbose       = BooleanSetting    ("-verbose", "Output messages about what the compiler is doing").hideToIDE
  val deprecation   = BooleanSetting    ("-deprecation", "Output source locations where deprecated APIs are used").hideToIDE
  val unchecked     = BooleanSetting    ("-unchecked", "Enable detailed unchecked warnings").hideToIDE
  val noide         = BooleanSetting    ("-noide", "Do not generate class-source mappings that are used by the Scala IDE").hideToIDE
  val classpath     = new StringSetting ("-classpath", "path", "Specify where to find user class files", classpathDefault) { override val abbreviation = "-cp" }
  val sourcepath    = StringSetting     ("-sourcepath", "path", "Specify where to find input source files", "")
  val bootclasspath = StringSetting     ("-bootclasspath", "path", "Override location of bootstrap class files", bootclasspathDefault)
  val extdirs       = StringSetting     ("-extdirs", "dirs", "Override location of installed extensions", extdirsDefault)
  val outdir        = StringSetting     ("-d", "directory", "Specify where to place generated class files", ".")
  val encoding      = StringSetting     ("-encoding", "encoding", "Specify character encoding used by source files", encodingDefault)
  val target        = ChoiceSetting     ("-target", "Specify for which target object files should be built", List("jvm-1.5", "jvm-1.4", "msil", "cldc"), "jvm-1.4")
  val printLate     = BooleanSetting    ("-print", "Print program with all Scala-specific features removed").hideToIDE
  val XO            = BooleanSetting    ("-optimise", "Generates faster bytecode by applying optimisations to the program")
  val explaintypes  = BooleanSetting    ("-explaintypes", "Explain type errors in more detail").hideToIDE
  val uniqid        = BooleanSetting    ("-uniqid", "Print identifiers with unique names for debugging").hideToIDE
  val version       = BooleanSetting    ("-version", "Print product version and exit").hideToIDE
  val help          = BooleanSetting    ("-help", "Print a synopsis of standard options").hideToIDE
  val Xhelp         = BooleanSetting    ("-X", "Print a synopsis of advanced options").hideToIDE

  val assemname     = StringSetting     ("-Xassem", "file", "Name of the output assembly (only relevant with -target:msil)", "").dependsOn(target, "msil")
  val assemrefs     = StringSetting     ("-Xassem-path", "path", "List of assemblies referenced by the program (only relevant with -target:msil)", ".").dependsOn(target, "msil")
  val Xchecknull    = BooleanSetting    ("-Xcheck-null", "Emit warning on selection of nullable reference")
  val noassertions  = BooleanSetting    ("-Xdisable-assertions", "Generate no assertions and assumptions")
  val Xexperimental = BooleanSetting    ("-Xexperimental", "Enable experimental extensions")
  val Xnojline      = new BooleanSetting("-Xnojline", "Do not use JLine for editing").hideToIDE
  val nouescape     = BooleanSetting    ("-Xno-uescape", "Disables handling of \\u unicode escapes")
  val Xplugtypes    = BooleanSetting    ("-Xplug-types", "Process annotations on types")
  val plugin        = MultiStringSetting("-Xplugin", "file", "Load a plugin from a file")
  val disable       = MultiStringSetting("-Xplugin-disable", "plugin", "Disable a plugin")
  val showPlugins   = BooleanSetting    ("-Xplugin-list", "Print a synopsis of loaded plugins").hideToIDE
  val pluginOptions = new MultiStringSetting("-P", "plugin:opt", "Pass an option to a plugin") { override def helpSyntax = "-P:<plugin>:<opt>" }
  val require       = MultiStringSetting("-Xplugin-require", "plugin", "Abort unless a plugin is available")
  val pluginsDir    = StringSetting     ("-Xpluginsdir", "path", "Location to find compiler plugins", pluginsDirDefault)
  val print         = PhasesSetting     ("-Xprint", "Print out program after")
  val Xprintpos     = BooleanSetting    ("-Xprint-pos", "Print tree positions (as offsets)").hideToIDE
  val printtypes    = BooleanSetting    ("-Xprint-types", "Print tree types (debugging option)").hideToIDE
  val prompt        = BooleanSetting    ("-Xprompt", "Display a prompt after each error (debugging option)").hideToIDE
  val resident      = BooleanSetting    ("-Xresident", "Compiler stays resident, files to compile are read from standard input").hideToIDE
  val Xshowcls      = StringSetting     ("-Xshow-class", "class", "Show class info", "")
  val Xshowobj      = StringSetting     ("-Xshow-object", "object", "Show object info", "")
  val showPhases    = BooleanSetting    ("-Xshow-phases", "Print a synopsis of compiler phases").hideToIDE
  val sourceReader  = StringSetting     ("-Xsource-reader", "classname", "Specify a custom method for reading source files", "scala.tools.nsc.io.SourceReader")

  val Yhelp         = BooleanSetting    ("-Y", "Print a synopsis of private options").hideToIDE
  val browse        = PhasesSetting     ("-Ybrowse", "Browse the abstract syntax tree after")
  val check         = PhasesSetting     ("-Ycheck", "Check the tree at start of")
  val Xcloselim     = BooleanSetting    ("-Yclosure-elim", "Perform closure elimination")
  val Xcodebase     = StringSetting     ("-Ycodebase", "codebase", "Specify the URL containing the Scala libraries", "")
  val debug         = BooleanSetting    ("-Ydebug", "Output debugging messages").hideToIDE
  val Xdce          = BooleanSetting    ("-Ydead-code", "Perform dead code elimination")
  val Xdetach       = BooleanSetting    ("-Ydetach", "Perform detaching of remote closures")
  val doc           = BooleanSetting    ("-Ydoc", "Generate documentation").hideToIDE
  val Xgenerics     = BooleanSetting    ("-Ygenerics", "Use generic Java types")
  val inline        = BooleanSetting    ("-Yinline", "Perform inlining when possible")
  val Xlinearizer   = ChoiceSetting     ("-Ylinearizer", "Linearizer to use", List("normal", "dfs", "rpo", "dump"), "rpo")
  val log           = PhasesSetting     ("-Ylog", "Log operations in")
  val logAll        = BooleanSetting    ("-Ylog-all", "Log all operations").hideToIDE
  val noimports     = BooleanSetting    ("-Yno-imports", "Compile without any implicit imports")
  val nopredefs     = BooleanSetting    ("-Yno-predefs", "Compile without any implicit predefined values")
  val script        = StringSetting     ("-Xscript", "object", "compile as a script, wrapping the code into object.main()", "").hideToIDE
  val Xshowtrees    = BooleanSetting    ("-Yshow-trees", "Show detailed trees when used in connection with -print:phase").hideToIDE
  val skip          = PhasesSetting     ("-Yskip", "Skip")
  val Xsqueeze      = ChoiceSetting     ("-Ysqueeze", "if on, creates compact code in matching", List("on","on","off"), "on")
  val statistics    = BooleanSetting    ("-Ystatistics", "Print compiler statistics").hideToIDE
  val stop          = PhasesSetting     ("-Ystop", "Stop after phase")
  val Xwarndeadcode = BooleanSetting    ("-Ywarn-dead-code", "Emit warnings for dead code")

  val Xcasetags     = ChoiceSetting("-Ycasetags", "test integer tags for case classes", List("on","off"),
                                     /*default*/"off")

  /** scaladoc specific options */
  val pagebottom     = StringSetting    ("-bottom", "pagebottom", "Include bottom text for each page", "").dependsOn(doc)
  val doccharset     = StringSetting    ("-charset", "doccharset", "Charset for cross-platform viewing of generated documentation.", "").dependsOn(doc)
  val doctitle       = StringSetting    ("-doctitle", "doctitle", "Include title for the overview page", "Scala 2<br/>API Specification").dependsOn(doc)
  val pagefooter     = StringSetting    ("-footer", "pagefooter", "Include footer text for each page", "").dependsOn(doc)
  val pageheader     = StringSetting    ("-header", "pageheader", "Include header text for each page", "").dependsOn(doc)
  val linksource     = BooleanSetting   ("-linksource", "Generate source in HTML").hideToIDE.dependsOn(doc)
  val nocomment      = BooleanSetting   ("-nocomment", "Suppress description and tags, generate only declarations.").hideToIDE.dependsOn(doc)
  val stylesheetfile = StringSetting    ("-stylesheetfile", "stylesheetfile", "File to change style of the generated documentation", "style.css").dependsOn(doc)
  val pagetop        = StringSetting    ("-top", "pagetop", "Include top text for each page", "").dependsOn(doc)
  val windowtitle    = StringSetting    ("-windowtitle", "windowtitle", "Specify window title of generated HTML documentation", "Scala 2").dependsOn(doc)

  /** A list of all settings */
  def allSettings: List[Setting] = allsettings.reverse
  /** Disable a setting */
  def disable(s: Setting) = {
    allsettings = allsettings filter (s !=)
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

    def isStandard: Boolean = !isAdvanced && !isPrivate
    def isAdvanced: Boolean =
      (name startsWith "-X") && !(name eq "-X")
    def isPrivate: Boolean =
      (name == "-P") || ((name startsWith "-Y") && !(name eq "-Y"))
    def isDocOption: Boolean =
      !dependency.isEmpty && dependency.get._1 == doc

    // initialization
    allsettings = this :: allsettings
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
  }

  /** A setting represented by a string, (`default' unless set) */
  case class StringSetting(name: String, arg: String, descr: String, default: String)
  extends Setting(descr) {
    hideToIDE
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
      case arg :: rest if (arg.startsWith(nameColon)) =>
	val toadd = arg.substring(nameColon.length())
        if (toadd.length == 0) {
	  error("empty argument to " + nameColon)
	  args
	} else {
	  appendToValue(toadd)
	  rest
	}
      case _ => args
    }

    override def helpSyntax = name + ":<" + arg + ">"

    def unparse: List[String] =
      for (opt <- value)
	yield nameColon+opt
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
   *  phase names. This is not checked here, however.
   *  (the empty list, unless set)
   */
  case class PhasesSetting(name: String, descr: String)
  extends Setting(descr + " <phase>") { // (see -showphases)") {
    hideToIDE
    protected var v: List[String] = List()

    def value: List[String] = this.v
    def value_=(s: List[String]) { setByUser = true; this.v = s }

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

    def contains(phasename: String): Boolean =
      value exists (str => phasename startsWith str)

    def unparse: List[String] =
      (value.foldLeft[List[String]]
          (Nil)
          ((args, phase) =>
            List(name + ":" + phase) ::: args))
  }
}
