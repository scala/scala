/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.File
import io.AbstractFile
import util.SourceFile
import Settings._
import annotation.elidable

class Settings(errorFn: String => Unit) extends ScalacSettings {
  def this() = this(Console.println)

  // optionizes a system property
  private def syspropopt(name: String): Option[String] = onull(System.getProperty(name))
  private def sysenvopt(name: String): Option[String] = onull(System.getenv(name))

  // given any number of possible path segments, flattens down to a
  // :-separated style path
  private def concatPath(segments: Option[String]*): String =
    segments.toList.flatMap(x => x) mkString File.pathSeparator

  protected def classpathDefault =
    sysenvopt("CLASSPATH") getOrElse "."

  protected def bootclasspathDefault =
    concatPath(syspropopt("sun.boot.class.path"), guessedScalaBootClassPath)
    // syspropopt("sun.boot.class.path") getOrElse ""
    // XXX scala-library.jar was being added to both boot and regular classpath until 8/18/09
    // Removing from boot classpath caused build/quick/bin/scala to fail.
    // Note to self, figure out how/why the bootclasspath is tied up with the locker/quick/pack.

  protected def extdirsDefault =
    concatPath(syspropopt("java.ext.dirs"), guessedScalaExtDirs)

  protected def assemExtdirsDefault =
    concatPath(guessedScalaExtDirs)

  protected def pluginsDirDefault =
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

  override def hashCode() = allSettings.hashCode
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

    for (setting <- allSettings ; (dep, value) <- setting.dependency)
      if (!setting.isDefault && !hasValue(dep, value)) {
        errorFn("incomplete option " + setting.name + " (requires " + dep.name + ")")
        return false
      }

    true
  }


  /** A list pairing source directories with their output directory.
   *  This option is not available on the command line, but can be set by
   *  other tools (IDEs especially). The command line specifies a single
   *  output directory that is used for all source files, denoted by a
   *  '*' in this list.
   */
  lazy val outputDirs = new OutputDirs

  /**
   * Split command line parameters by space, properly process quoted parameter
   */
  def splitParams(line: String): List[String] = {
    def parse(from: Int, i: Int, args: List[String]): List[String] = {
      if (i < line.length) {
        line.charAt(i) match {
          case ' ' =>
            val args1 = fetchArg(from, i) :: args
            val j = skipS(i + 1)
            if (j >= 0) {
              parse(j, j, args1)
            } else args1
          case '"' =>
            val j = skipTillQuote(i + 1)
            if (j > 0) {
              parse(from, j + 1, args)
            } else {
              errorFn("Parameters '" + line + "' with unmatched quote at " + i + ".")
              Nil
            }
          case _ => parse(from, i + 1, args)
        }
      } else { // done
        if (i > from) {
          fetchArg(from, i) :: args
        } else args
      }
    }

    def fetchArg(from: Int, until: Int) = {
      if (line.charAt(from) == '"') {
        line.substring(from + 1, until - 1)
      } else {
        line.substring(from, until)
      }
    }

    def skipTillQuote(i: Int): Int = {
      if (i < line.length) {
        line.charAt(i) match {
          case '"' => i
          case _ => skipTillQuote(i + 1)
        }
      } else -1
    }

    def skipS(i: Int): Int = {
      if (i < line.length) {
        line.charAt(i) match {
          case ' ' => skipS(i + 1)
          case _ => i
        }
      } else -1
    }

    // begin split
    val j = skipS(0)
    if (j >= 0) {
      parse(j, j, Nil).reverse
    } else Nil
  }

  def parseParams(args: List[String]): List[String] = {
    // verify command exists and call setter
    def tryToSetIfExists(
      cmd: String,
      args: List[String],
      setter: (Setting) => (List[String] => Option[List[String]])
    ): Option[List[String]] =
      lookupSetting(cmd) match {
        case None       => errorFn("Parameter '" + cmd + "' is not recognised by Scalac.") ; None
        case Some(cmd)  =>
          val res = setter(cmd)(args)
          cmd.postSetHook()
          res
      }

    // if arg is of form -Xfoo:bar,baz,quux
    def parseColonArg(s: String): Option[List[String]] = {
      val idx = s indexWhere (_ == ':')
      val (p, args) = (s.substring(0, idx), s.substring(idx+1).split(",").toList)

      // any non-Nil return value means failure and we return s unmodified
      tryToSetIfExists(p, args, (s: Setting) => s.tryToSetColon _)
    }
    // if arg is of form -Dfoo=bar or -Dfoo (name = "-D")
    def isPropertyArg(s: String) = lookupSetting(s.substring(0, 2)) match {
      case Some(x: DefinesSetting)  => true
      case _                        => false
    }
    def parsePropertyArg(s: String): Option[List[String]] = {
      val (p, args) = (s.substring(0, 2), s.substring(2))

      tryToSetIfExists(p, List(args), (s: Setting) => s.tryToSetProperty _)
    }

    // if arg is of form -Xfoo or -Xfoo bar (name = "-Xfoo")
    def parseNormalArg(p: String, args: List[String]): Option[List[String]] =
      tryToSetIfExists(p, args, (s: Setting) => s.tryToSet _)

    def doArgs(args: List[String]): List[String] = {
      if (args.isEmpty) return Nil
      val arg :: rest = args
      if (arg == "") {
        // it looks like Ant passes "" sometimes
        rest
      }
      else if (!arg.startsWith("-")) {
        errorFn("Argument '" + arg + "' does not start with '-'.")
        args
      }
      else if (arg == "-") {
        errorFn("'-' is not a valid argument.")
        args
      }
      else
        // we dispatch differently based on the appearance of p:
        // 1) If it has a : it is presumed to be -Xfoo:bar,baz
        // 2) If the first two chars are the name of a command, -Dfoo=bar
        // 3) Otherwise, the whole string should be a command name
        //
        // Internally we use Option[List[String]] to discover error,
        // but the outside expects our arguments back unchanged on failure
        if (arg contains ":") parseColonArg(arg) match {
          case Some(_)  => rest
          case None     => args
        }
        else if (isPropertyArg(arg)) parsePropertyArg(arg) match {
          case Some(_)  => rest
          case None     => args
        }
        else parseNormalArg(arg, rest) match {
          case Some(xs) => xs
          case None     => args
        }
    }

    doArgs(args)
  }

  // checks both name and any available abbreviations
  def lookupSetting(cmd: String): Option[Setting] =
    settingSet.find(x => x.name == cmd || (x.abbreviations contains cmd))

  // The *Setting classes used to be case classes defined inside of Settings.
  // The choice of location was poor because it tied the type of each setting
  // to its enclosing instance, which broke equality, so I moved the class
  // definitions into the companion object.  The one benefit it was getting
  // out of this was using its knowledge of the enclosing instance to add
  // itself to the list of settings in the Setting constructor.  However,
  // this was dicey and not working predictably, as illustrated in the comment
  // in GenericRunnerSettings:
  //
  //   For some reason, "object defines extends Setting(...)"
  //   does not work here.  The object is present but the setting
  //   is not added to allsettings.
  //
  // To capture similar semantics, I created instance methods on setting
  // which call a factory method for the right kind of object and then add
  // the newly constructed instance to allsettings.  The constructors are
  // private to force all creation to go through these methods.
  //
  // The usage of case classes was becoming problematic (due to custom
  // equality, case class inheritance, and the need to control object
  // creation without a synthetic apply method getting in the way) and
  // it was providing little benefit, so they are no longer cases.

  // a wrapper for all Setting creators to keep our list up to date
  // and tell them how to announce errors
  private def add[T <: Setting](s: T): T = {
    s setErrorHandler errorFn
    allsettings += s
    s
  }

  /**
   *  The canonical creators for Setting objects.
   */
  import Function.{ tupled, untupled }
  import Setting._

  // A bit too clever, but I haven't found any other way to compose
  // functions with arity 2+ without having to annotate parameter types
  lazy val IntSetting          = untupled(tupled(sint _) andThen add[IntSetting])
  lazy val BooleanSetting      = untupled(tupled(bool _) andThen add[BooleanSetting])
  lazy val StringSetting       = untupled(tupled(str _) andThen add[StringSetting])
  lazy val MultiStringSetting  = untupled(tupled(multi _) andThen add[MultiStringSetting])
  lazy val ChoiceSetting       = untupled(tupled(choice _) andThen add[ChoiceSetting])
  lazy val DebugSetting        = untupled(tupled(sdebug _) andThen add[DebugSetting])
  lazy val PhasesSetting       = untupled(tupled(phase _) andThen add[PhasesSetting])
  lazy val DefinesSetting      = add(defines())
  lazy val OutputSetting       = untupled(tupled(output _) andThen add[OutputSetting])

  override def toString() =
    "Settings(\n%s)" format (settingSet filter (s => !s.isDefault) map ("  " + _ + "\n") mkString)
}

object Settings {
  // basically this is a value which remembers if it's been modified
  trait SettingValue {
    type T <: Any
    protected var v: T
    private var setByUser: Boolean = false
    def isDefault: Boolean = !setByUser
    def value: T = v
    def value_=(arg: T) = { setByUser = true ; v = arg }
    val choices : List[T] = Nil
  }

  /** A class for holding mappings from source directories to
   *  their output location. This functionality can be accessed
   *  only programmatically. The command line compiler uses a
   *  single output location, but tools may use this functionality
   *  to set output location per source directory.
   */
  class OutputDirs {
    /** Pairs of source directory - destination directory. */
    private var outputDirs: List[(AbstractFile, AbstractFile)] = Nil

    /** If this is not None, the output location where all
     *  classes should go.
     */
    private var singleOutDir: Option[AbstractFile] = None

    /** Add a destination directory for sources found under srcdir.
     *  Both directories should exits.
     */
    def add(srcDir: String, outDir: String): Unit =
      add(checkDir(AbstractFile.getDirectory(srcDir), srcDir),
          checkDir(AbstractFile.getDirectory(outDir), outDir))

    /** Check that dir is exists and is a directory. */
    private def checkDir(dir: AbstractFile, name: String): AbstractFile = {
      if ((dir eq null) || !dir.isDirectory)
        throw new FatalError(name + " does not exist or is not a directory")
      dir
    }

    /** Set the single output directory. From now on, all files will
     *  be dumped in there, regardless of previous calls to 'add'.
     */
    def setSingleOutput(outDir: String) {
      val dst = AbstractFile.getDirectory(outDir)
      setSingleOutput(checkDir(dst, outDir))
    }

    /** Set the single output directory. From now on, all files will
     *  be dumped in there, regardless of previous calls to 'add'.
     */
    def setSingleOutput(dir: AbstractFile) {
      singleOutDir = Some(dir)
    }

    def add(src: AbstractFile, dst: AbstractFile) {
      singleOutDir = None
      outputDirs ::= (src, dst)
    }

    /** Return the list of source-destination directory pairs. */
    def outputs: List[(AbstractFile, AbstractFile)] = outputDirs

    /** Return the output directory for the given file.
     */
    def outputDirFor(src: AbstractFile): AbstractFile = {
      def isBelow(srcDir: AbstractFile, outDir: AbstractFile) =
        src.path.startsWith(srcDir.path)

      singleOutDir match {
        case Some(d) => d
        case None =>
          (outputs find Function.tupled(isBelow)) match {
            case Some((_, d)) => d
            case _ =>
              throw new FatalError("Could not find an output directory for "
                                   + src.path + " in " + outputs)
          }
      }
    }
  }

  // The Setting companion object holds all the factory methods
  object Setting {
    def bool(name: String, descr: String) =
      new BooleanSetting(name, descr)

    def str(name: String, arg: String, descr: String, default: String) =
      new StringSetting(name, arg, descr, default)

    def sint(
      name: String,
      descr: String,
      default: Int,
      range: Option[(Int, Int)] = None,
      parser: String => Option[Int] = _ => None
    ) =
      new IntSetting(name, descr, default, range, parser)

    def multi(name: String, arg: String, descr: String) =
      new MultiStringSetting(name, arg, descr)

    def choice(name: String, descr: String, choices: List[String], default: String): ChoiceSetting =
      new ChoiceSetting(name, descr, choices, default)

    def sdebug(name: String, descr: String, choices: List[String], default: String, defaultEmpty: String) =
      new DebugSetting(name, descr, choices, default, defaultEmpty)

    def phase(name: String, descr: String) =
      new PhasesSetting(name, descr)

    def defines() = new DefinesSetting()

    def output(outputDirs: OutputDirs, default: String) =
      new OutputSetting(outputDirs, default)
  }

  implicit val SettingOrdering : Ordering[Setting] = Ordering.ordered;
  /** A base class for settings of all types.
   *  Subclasses each define a `value' field of the appropriate type.
   */
  abstract class Setting(descr: String) extends Ordered[Setting] with SettingValue {
    /** The name of the option as written on the command line, '-' included. */
    def name: String

    /** Error handling function, set after creation by enclosing Settings instance */
    private var _errorFn: String => Unit = _
    private[Settings] def setErrorHandler(e: String => Unit) = _errorFn = e
    def errorFn(msg: String) = _errorFn(msg)
    def errorAndValue[T](msg: String, x: T): T = { errorFn(msg) ; x }

    /** Will be called after this Setting is set, for any cases where the
     *  Setting wants to perform extra work. */
    private var _postSetHook: () => Unit = () => ()
    def postSetHook(): Unit = _postSetHook()
    def withPostSetHook(f: () => Unit): this.type = { _postSetHook = f ; this }

    /** After correct Setting has been selected, tryToSet is called with the
     *  remainder of the command line.  It consumes any applicable arguments and
     *  returns the unconsumed ones.
     */
    private[Settings] def tryToSet(args: List[String]): Option[List[String]]

    /** Commands which can take lists of arguments in form -Xfoo:bar,baz override
     *  this method and accept them as a list.  It returns List[String] for
     *  consistency with tryToSet, and should return its incoming arguments
     *  unmodified on failure, and Nil on success.
     */
    private[Settings] def tryToSetColon(args: List[String]): Option[List[String]] =
      errorAndValue("'" + name + "' does not accept multiple arguments", None)

    /** Commands which take properties in form -Dfoo=bar or -Dfoo
     */
    private[Settings] def tryToSetProperty(args: List[String]): Option[List[String]] =
      errorAndValue("'" + name + "' does not accept property style arguments", None)

    /**
     * Attempt to set from a properties file style property value.
     */
    def tryToSetFromPropertyValue(s : String) {
      tryToSet(s :: Nil)
    }

    /** The syntax defining this setting in a help string */
    private var _helpSyntax = name
    def helpSyntax: String = _helpSyntax
    def withHelpSyntax(s: String): this.type    = { _helpSyntax = s ; this }

    /** Abbreviations for this setting */
    private var _abbreviations: List[String] = Nil
    def abbreviations = _abbreviations
    def withAbbreviation(s: String): this.type  = { _abbreviations ++= List(s) ; this }

    /** A description of the purpose of this setting in a help string */
    def helpDescription = descr

    /** A list of Strings which can recreate this setting. */
    def unparse: List[String]

    /** Optional dependency on another setting */
    protected[Settings] var dependency: Option[(Setting, String)] = None
    def dependsOn(s: Setting, value: String): this.type = { dependency = Some((s, value)); this }
    def dependsOn(s: Setting): this.type = dependsOn(s, "")

    def isStandard:    Boolean = !isFscSpecific && !isAdvanced && !isPrivate && name != "-Y"
    def isFscSpecific: Boolean = (name == "-shutdown")
    def isAdvanced:    Boolean = (name startsWith "-X") && name != "-X"
    def isPrivate:     Boolean = (name == "-P") || ((name startsWith "-Y") && name != "-Y")

    // Ordered (so we can use TreeSet)
    def compare(that: Setting): Int = name compare that.name
    def compareLists[T <% Ordered[T]](xs: List[T], ys: List[T]): Boolean =
      xs.sortWith(_ < _) == ys.sortWith(_ < _)

    // Equality
    def eqValues: List[Any] = List(name, value)
    def isEq(other: Setting) = eqValues == other.eqValues
    override def hashCode() = name.hashCode
    override def toString() = "%s = %s".format(name, value)
  }

  /** A setting represented by an integer */
  class IntSetting private[Settings](
    val name: String,
    val descr: String,
    val default: Int,
    val range: Option[(Int, Int)],
    parser: String => Option[Int])
  extends Setting(descr) {
    type T = Int
    protected var v = default

    // not stable values!
    val IntMin = Int.MinValue
    val IntMax = Int.MaxValue
    def min = range map (_._1) getOrElse IntMin
    def max = range map (_._2) getOrElse IntMax

    override def value_=(s: Int) =
      if (isInputValid(s)) super.value_=(s) else errorMsg

    // Validate that min and max are consistent
    assert(min <= max)

    // Helper to validate an input
    private def isInputValid(k: Int): Boolean = (min <= k) && (k <= max)

    // Helper to generate a textual explaination of valid inputs
    private def getValidText: String = (min, max) match {
      case (IntMin, IntMax)   => "can be any integer"
      case (IntMin, x)        => "must be less than or equal to "+x
      case (x, IntMax)        => "must be greater than or equal to "+x
      case _                  => "must be between %d and %d".format(min, max)
    }

    // Ensure that the default value is actually valid
    assert(isInputValid(default))

    def parseArgument(x: String): Option[Int] = {
      parser(x) orElse {
        try   { Some(x.toInt) }
        catch { case _: NumberFormatException => None }
      }
    }

    def errorMsg = errorFn("invalid setting for -"+name+" "+getValidText)

    def tryToSet(args: List[String]) =
      if (args.isEmpty) errorAndValue("missing argument", None)
      else parseArgument(args.head) match {
        case Some(i)  => value = i ; Some(args.tail)
        case None     => errorMsg ; None
      }

    def unparse: List[String] =
      if (value == default) Nil
      else List(name, value.toString)

    override def equals(that: Any) = that match {
      case x: IntSetting => this isEq x
      case _            => false
    }
  }

  /** A setting represented by a boolean flag (false, unless set) */
  class BooleanSetting private[Settings](
    val name: String,
    val descr: String)
  extends Setting(descr) {
    type T = Boolean
    protected var v = false

    def tryToSet(args: List[String]) = { value = true ; Some(args) }
    def unparse: List[String] = if (value) List(name) else Nil
    override def tryToSetFromPropertyValue(s : String) {
      value = s.equalsIgnoreCase("true")
    }
    override def equals(that: Any) = that match {
      case x: BooleanSetting => this isEq x
      case _            => false
    }
  }

  /** A setting represented by a string, (`default' unless set) */
  class StringSetting private[Settings](
    val name: String,
    val arg: String,
    val descr: String,
    val default: String)
  extends Setting(descr) {
    type T = String
    protected var v = default

    def tryToSet(args: List[String]) = args match {
      case Nil      => errorAndValue("missing argument", None)
      case x :: xs  => value = x ; Some(xs)
    }
    def unparse: List[String] = if (value == default) Nil else List(name, value)

    withHelpSyntax(name + " <" + arg + ">")

    override def equals(that: Any) = that match {
      case x: StringSetting => this isEq x
      case _            => false
    }
  }

  /** Set the output directory. */
  class OutputSetting private[Settings](
    outputDirs: OutputDirs,
    default: String)
    extends StringSetting("-d", "directory", "Specify where to place generated class files", default) {
      value = default
      override def value_=(str: String) {
        super.value_=(str)
        outputDirs.setSingleOutput(str)
      }
  }

  /** A setting that accumulates all strings supplied to it,
   *  until it encounters one starting with a '-'. */
  class MultiStringSetting private[Settings](
    val name: String,
    val arg: String,
    val descr: String)
  extends Setting(descr) {
    type T = List[String]
    protected var v: List[String] = Nil
    def appendToValue(str: String) { value ++= List(str) }

    def tryToSet(args: List[String]) = {
      val (strings, rest) = args span (x => !x.startsWith("-"))
      strings foreach appendToValue

      Some(rest)
    }
    override def tryToSetColon(args: List[String]) = tryToSet(args)
    def unparse: List[String] = value map { name + ":" + _ }

    withHelpSyntax(name + ":<" + arg + ">")
    override def equals(that: Any) = that match {
      case x: MultiStringSetting => this isEq x
      case _            => false
    }
  }

  /** A setting represented by a string in a given set of <code>choices</code>,
   *  (<code>default</code> unless set).
   */
  class ChoiceSetting private[Settings](
    val name: String,
    val descr: String,
    override val choices: List[String],
    val default: String)
  extends Setting(descr + choices.mkString(" (", ",", ")")) {
    type T = String
    protected var v: String = default
    protected def argument: String = name.substring(1)

    def tryToSet(args: List[String]) = { value = default ; Some(args) }
    override def tryToSetColon(args: List[String]) = args match {
      case Nil                            => errorAndValue("missing " + argument, None)
      case List(x) if choices contains x  => value = x ; Some(Nil)
      case List(x)                        => errorAndValue("'" + x + "' is not a valid choice for '" + name + "'", None)
      case xs                             => errorAndValue("'" + name + "' does not accept multiple arguments.", None)
    }
    def unparse: List[String] =
      if (value == default) Nil else List(name + ":" + value)

    withHelpSyntax(name + ":<" + argument + ">")
    override def equals(that: Any) = that match {
      case x: ChoiceSetting => this isEq x
      case _            => false
    }
  }

  /** Same as ChoiceSetting but have a <code>level</code> int which tells the
   *  index of the selected choice. The <code>defaultEmpty</code> is used when
   *  this setting is used without specifying any of the available choices.
   */
  class DebugSetting private[Settings](
    name: String,
    descr: String,
    choices: List[String],
    default: String,
    val defaultEmpty: String)
  extends ChoiceSetting(name, descr, choices, default) {
    def indexOf[T](xs: List[T], e: T): Option[Int] = xs.indexOf(e) match {
      case -1 => None
      case x  => Some(x)
    }
    var level: Int = indexOf(choices, default).get

    override def value_=(choice: String) = {
      super.value_=(choice)
      level = indexOf(choices, choice).get
    }

    override def tryToSet(args: List[String]) =
      if (args.isEmpty) { value = defaultEmpty ; Some(Nil) }
      else super.tryToSet(args)
    override def equals(that: Any) = that match {
      case x: DebugSetting => this isEq x
      case _            => false
    }
  }

  /** A setting represented by a list of strings which should be prefixes of
   *  phase names. This is not checked here, however.  Alternatively the string
   *  "all" can be used to represent all phases.
   *  (the empty list, unless set)
   */
  class PhasesSetting private[Settings](
    val name: String,
    val descr: String)
  extends Setting(descr + " <phase> or \"all\"") {
    type T = List[String]
    protected var v: List[String] = Nil

    def tryToSet(args: List[String]) = errorAndValue("missing phase", None)
    override def tryToSetColon(args: List[String]) = args match {
      case Nil  => errorAndValue("missing phase", None)
      case xs   => value ++= xs ; Some(Nil)
    }
    // we slightly abuse the usual meaning of "contains" here by returning
    // true if our phase list contains "all", regardless of the incoming argument
    def contains(phasename: String): Boolean =
      doAllPhases || (value exists { phasename startsWith _ } )

    def doAllPhases() = value contains "all"
    def unparse: List[String] = value map { name + ":" + _ }

    override def equals(that: Any) = that match {
      case ps: PhasesSetting if name == ps.name =>
        (doAllPhases && ps.doAllPhases) || compareLists(value, ps.value)
      case _                                    => false
    }

    withHelpSyntax(name + ":<phase>")
  }

  /** A setting for a -D style property definition */
  class DefinesSetting private[Settings] extends Setting("set a Java property") {
    type T = List[(String, String)]
    protected var v: T = Nil
    def name = "-D"
    withHelpSyntax(name + "<prop>")

    // given foo=bar returns Some(foo, bar), or None if parse fails
    def parseArg(s: String): Option[(String, String)] = {
      if (s == "") return None
      val regexp = """^(.*)?=(.*)$""".r

      regexp.findAllIn(s).matchData.toList match {
        case Nil      => Some(s, "")
        case List(md) => md.subgroups match { case List(a,b) => Some(a,b) }
      }
    }

    def tryToSet(args: List[String]) =
      if (args.isEmpty) None
      else parseArg(args.head) match {
        case None         => None
        case Some((a, b)) => value ++= List((a, b)) ; Some(args.tail)
      }

    /** Apply the specified properties to the current JVM */
    def applyToCurrentJVM =
      value foreach { case (k, v) => System.getProperties.setProperty(k, v) }

    def unparse: List[String] =
      value map { case (k,v) => "-D" + k + (if (v == "") "" else "=" + v) }
    override def equals(that: Any) = that match {
      case x: DefinesSetting => this isEq x
      case _            => false
    }
  }

}

trait ScalacSettings {
  self: Settings =>

  import collection.immutable.TreeSet

  /** A list of all settings */
  protected var allsettings: Set[Setting] = TreeSet[Setting]()
  def settingSet: Set[Setting] = allsettings
  def allSettings: List[Setting] = settingSet.toList

  /** Disable a setting */
  def disable(s: Setting) = allsettings -= s

  /**
   *  Temporary Settings
   */
  val suppressVTWarn = BooleanSetting    ("-Ysuppress-vt-typer-warnings", "Suppress warnings from the typer when testing the virtual class encoding, NOT FOR FINAL!")

  /**
   *  Standard settings
   */
  // argfiles is only for the help message
  val argfiles      = BooleanSetting    ("@<file>", "A text file containing compiler arguments (options and source files)")
  val bootclasspath = StringSetting     ("-bootclasspath", "path", "Override location of bootstrap class files", bootclasspathDefault)
  val classpath     = StringSetting     ("-classpath", "path", "Specify where to find user class files", classpathDefault).withAbbreviation("-cp")
  val outdir        = OutputSetting     (outputDirs, ".")
  val dependenciesFile  = StringSetting ("-dependencyfile", "file", "Specify the file in which dependencies are tracked", ".scala_dependencies")
  val deprecation   = BooleanSetting    ("-deprecation", "Output source locations where deprecated APIs are used")
  val encoding      = StringSetting     ("-encoding", "encoding", "Specify character encoding used by source files", Properties.sourceEncoding)
  val explaintypes  = BooleanSetting    ("-explaintypes", "Explain type errors in more detail")
  val extdirs       = StringSetting     ("-extdirs", "dirs", "Override location of installed extensions", extdirsDefault)
  val debuginfo     = DebugSetting      ("-g", "Specify level of generated debugging info", List("none", "source", "line", "vars", "notailcalls"), "vars", "vars")
  val help          = BooleanSetting    ("-help", "Print a synopsis of standard options")
  val make          = ChoiceSetting     ("-make", "Specify recompilation detection strategy", List("all", "changed", "immediate", "transitive", "transitivenocp"), "all") .
                                          withHelpSyntax("-make:<strategy>")
  val nowarnings    = BooleanSetting    ("-nowarn", "Generate no warnings")
  val XO            = BooleanSetting    ("-optimise", "Generates faster bytecode by applying optimisations to the program").withAbbreviation("-optimize")
  val printLate     = BooleanSetting    ("-print", "Print program with all Scala-specific features removed")
  val sourcepath    = StringSetting     ("-sourcepath", "path", "Specify where to find input source files", "")
  val target        = ChoiceSetting     ("-target", "Specify for which target object files should be built", List("jvm-1.5", "msil"), "jvm-1.5")
  val unchecked     = BooleanSetting    ("-unchecked", "Enable detailed unchecked warnings")
  val uniqid        = BooleanSetting    ("-uniqid", "Print identifiers with unique names for debugging")
  val verbose       = BooleanSetting    ("-verbose", "Output messages about what the compiler is doing")
  val version       = BooleanSetting    ("-version", "Print product version and exit")

  /**
   * -X "Advanced" settings
   */
  val Xhelp         = BooleanSetting    ("-X", "Print a synopsis of advanced options")
  val assemname     = StringSetting     ("-Xassem-name", "file", "Name of the output assembly (only relevant with -target:msil)", "").dependsOn(target, "msil")
  val assemrefs     = StringSetting     ("-Xassem-path", "path", "List of assemblies referenced by the program (only relevant with -target:msil)", ".").dependsOn(target, "msil")
  val assemextdirs  = StringSetting     ("-Xassem-extdirs", "dirs", "List of directories containing assemblies, defaults to `lib'", assemExtdirsDefault).dependsOn(target, "msil")
  val sourcedir     = StringSetting     ("-Xsourcedir", "directory", "When -target:msil, the source folder structure is mirrored in output directory.", ".").dependsOn(target, "msil")
  val checkInit     = BooleanSetting    ("-Xcheckinit", "Add runtime checks on field accessors. Uninitialized accesses result in an exception being thrown.")
  val noassertions  = BooleanSetting    ("-Xdisable-assertions", "Generate no assertions and assumptions")
  val elideLevel    = IntSetting        ("-Xelide-level", "Generate calls to @elidable-marked methods only method priority is greater than argument.",
                                                elidable.ASSERTION, None, elidable.byName.get(_))
  val Xexperimental = BooleanSetting    ("-Xexperimental", "Enable experimental extensions")
  val noForwarders  = BooleanSetting    ("-Xno-forwarders", "Do not generate static forwarders in mirror classes")
  val future        = BooleanSetting    ("-Xfuture", "Turn on future language features")
  val genPhaseGraph = StringSetting     ("-Xgenerate-phase-graph", "file", "Generate the phase graphs (outputs .dot files) to fileX.dot", "")
  val XlogImplicits = BooleanSetting    ("-Xlog-implicits", "Show more info on why some implicits are not applicable")
  val nouescape     = BooleanSetting    ("-Xno-uescape", "Disables handling of \\u unicode escapes")
  val XnoVarargsConversion = BooleanSetting("-Xno-varargs-conversion", "disable varags conversion")
  val Xnojline      = BooleanSetting    ("-Xnojline", "Do not use JLine for editing")
  val plugin        = MultiStringSetting("-Xplugin", "file", "Load a plugin from a file")
  val disable       = MultiStringSetting("-Xplugin-disable", "plugin", "Disable a plugin")
  val showPlugins   = BooleanSetting    ("-Xplugin-list", "Print a synopsis of loaded plugins")
  val require       = MultiStringSetting("-Xplugin-require", "plugin", "Abort unless a plugin is available")
  val pluginsDir    = StringSetting     ("-Xpluginsdir", "path", "Location to find compiler plugins", pluginsDirDefault)
  val print         = PhasesSetting     ("-Xprint", "Print out program after")
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

  /**
   * -Y "Private" settings
   */
  val Yhelp         = BooleanSetting    ("-Y", "Print a synopsis of private options")
  val browse        = PhasesSetting     ("-Ybrowse", "Browse the abstract syntax tree after")
  val check         = PhasesSetting     ("-Ycheck", "Check the tree at the end of")
  val Xcloselim     = BooleanSetting    ("-Yclosure-elim", "Perform closure elimination")
  val Xcodebase     = StringSetting     ("-Ycodebase", "codebase", "Specify the URL containing the Scala libraries", "")
  val noCompletion  = BooleanSetting    ("-Yno-completion", "Disable tab-completion in the REPL")
  val Xdce          = BooleanSetting    ("-Ydead-code", "Perform dead code elimination")
  val debug         = BooleanSetting    ("-Ydebug", "Output debugging messages")
  val Xdetach       = BooleanSetting    ("-Ydetach", "Perform detaching of remote closures")
  // val doc           = BooleanSetting    ("-Ydoc", "Generate documentation")
  val inline        = BooleanSetting    ("-Yinline", "Perform inlining when possible")
  val Xlinearizer   = ChoiceSetting     ("-Ylinearizer", "Linearizer to use", List("normal", "dfs", "rpo", "dump"), "rpo") .
                                          withHelpSyntax("-Ylinearizer:<which>")
  val log           = PhasesSetting     ("-Ylog", "Log operations in")
  val Ynogenericsig = BooleanSetting    ("-Yno-generic-signatures", "Suppress generation of generic signatures for Java")
  val noimports     = BooleanSetting    ("-Yno-imports", "Compile without any implicit imports")
  val nopredefs     = BooleanSetting    ("-Yno-predefs", "Compile without any implicit predefined values")
  val Yrecursion    = IntSetting        ("-Yrecursion", "Recursion depth used when locking symbols", 0, Some(0, Int.MaxValue), _ => None)
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
  val Ytyperdebug   = BooleanSetting    ("-Ytyper-debug", "Trace all type assignements")
  val Ypmatdebug    = BooleanSetting    ("-Ypmat-debug", "Trace all pattern matcher activity.")
  val Ytailrec      = BooleanSetting    ("-Ytailrecommend", "Alert methods which would be tail-recursive if private or final.")
  val Yjenkins      = BooleanSetting    ("-Yjenkins-hashCodes", "Use jenkins hash algorithm for case class generated hashCodes.")

  // Warnings
  val Xwarninit     = BooleanSetting    ("-Xwarninit", "Warn about possible changes in initialization semantics")
  val Xchecknull    = BooleanSetting    ("-Xcheck-null", "Emit warning on selection of nullable reference")
  val Xwarndeadcode = BooleanSetting    ("-Ywarn-dead-code", "Emit warnings for dead code")
  val YwarnShadow   = BooleanSetting    ("-Ywarn-shadowing", "Emit warnings about possible variable shadowing.")
  val YwarnCatches  = BooleanSetting    ("-Ywarn-catches", "Emit warnings about catch blocks which catch everything.")
  val Xwarnings     = BooleanSetting    ("-Xstrict-warnings", "Emit warnings about lots of things.") .
                          withPostSetHook(() =>
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
