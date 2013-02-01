/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools
package nsc
package settings

import io.{ AbstractFile, Jar, Path, PlainFile, VirtualDirectory }
import scala.reflect.internal.util.StringOps
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.reflect.{ ClassTag, classTag }

/** A mutable Settings object.
 */
class MutableSettings(val errorFn: String => Unit)
              extends scala.reflect.internal.settings.MutableSettings
                 with AbsSettings
                 with ScalaSettings
                 with Mutable {
  type ResultOfTryToSet = List[String]

  def withErrorFn(errorFn: String => Unit): MutableSettings = {
    val settings = new MutableSettings(errorFn)
    copyInto(settings)
    settings
  }

  def copyInto(settings: MutableSettings) {
    allSettings foreach { thisSetting =>
      val otherSetting = settings.allSettings find { _.name == thisSetting.name }
      otherSetting foreach { otherSetting =>
        if (thisSetting.isSetByUser || otherSetting.isSetByUser) {
          otherSetting.value = thisSetting.value.asInstanceOf[otherSetting.T]
        }
      }
    }
  }

  /** Iterates over the arguments applying them to settings where applicable.
   *  Then verifies setting dependencies are met.
   *
   *  This temporarily takes a boolean indicating whether to keep
   *  processing if an argument is seen which is not a command line option.
   *  This is an expedience for the moment so that you can say
   *
   *    scalac -d /tmp foo.scala -optimise
   *
   *  while also allowing
   *
   *    scala Program opt opt
   *
   *  to get their arguments.
   *
   *  Returns (success, List of unprocessed arguments)
   */
  def processArguments(arguments: List[String], processAll: Boolean): (Boolean, List[String]) = {
    def loop(args: List[String], residualArgs: List[String]): (Boolean, List[String]) = args match {
      case Nil        =>
        (checkDependencies, residualArgs)
      case "--" :: xs =>
        (checkDependencies, xs)
      case x :: xs  =>
        val isOpt = x startsWith "-"
        if (isOpt) {
          val newArgs = parseParams(args)
          if (args eq newArgs) {
            errorFn(s"bad option: '$x'")
            (false, args)
          }
          // discard empties, sometimes they appear because of ant or etc.
          // but discard carefully, because an empty string is valid as an argument
          // to an option, e.g. -cp "" .  So we discard them only when they appear
          // in option position.
          else if (x == "") {
            loop(xs, residualArgs)
          }
          else lookupSetting(x) match {
            case Some(s) if s.shouldStopProcessing  => (checkDependencies, newArgs)
            case _                                  => loop(newArgs, residualArgs)
          }
        }
        else {
          if (processAll) loop(xs, residualArgs :+ x)
          else (checkDependencies, args)
        }
    }
    loop(arguments, Nil)
  }
  def processArgumentString(params: String) = processArguments(splitParams(params), true)

  /** Create a new Settings object, copying all user-set values.
   */
  def copy(): Settings = {
    val s = new Settings()
    s.processArguments(recreateArgs, true)
    s
  }

  /** A list pairing source directories with their output directory.
   *  This option is not available on the command line, but can be set by
   *  other tools (IDEs especially). The command line specifies a single
   *  output directory that is used for all source files, denoted by a
   *  '*' in this list.
   */
  lazy val outputDirs = new OutputDirs

  /** A list of settings which act based on prefix rather than an exact
   *  match.  This is basically -D and -J.
   */
  lazy val prefixSettings = allSettings collect { case x: PrefixSetting => x }

  /** Split the given line into parameters.
   */
  def splitParams(line: String) = cmd.Parser.tokenize(line, errorFn)

  /** Returns any unprocessed arguments.
   */
  protected def parseParams(args: List[String]): List[String] = {
    // verify command exists and call setter
    def tryToSetIfExists(
      cmd: String,
      args: List[String],
      setter: (Setting) => (List[String] => Option[List[String]])
    ): Option[List[String]] =
      lookupSetting(cmd) match {
        //case None       => errorFn("Parameter '" + cmd + "' is not recognised by Scalac.") ; None
        case None       => None //error reported in processArguments
        case Some(cmd)  => setter(cmd)(args)
      }

    // if arg is of form -Xfoo:bar,baz,quux
    def parseColonArg(s: String): Option[List[String]] = {
      val (p, args) = StringOps.splitWhere(s, _ == ':', true) getOrElse (return None)

      // any non-Nil return value means failure and we return s unmodified
      tryToSetIfExists(p, (args split ",").toList, (s: Setting) => s.tryToSetColon _)
    }

    // if arg is of form -Xfoo or -Xfoo bar (name = "-Xfoo")
    def parseNormalArg(p: String, args: List[String]): Option[List[String]] =
      tryToSetIfExists(p, args, (s: Setting) => s.tryToSet _)

    args match {
      case Nil          => Nil
      case arg :: rest  =>
        if (!arg.startsWith("-")) {
          errorFn("Argument '" + arg + "' does not start with '-'.")
          args
        }
        else if (arg == "-") {
          errorFn("'-' is not a valid argument.")
          args
        }
        else {
          // we dispatch differently based on the appearance of p:
          // 1) If it matches a prefix setting it is sent there directly.
          // 2) If it has a : it is presumed to be -Xfoo:bar,baz
          // 3) Otherwise, the whole string should be a command name
          //
          // Internally we use Option[List[String]] to discover error,
          // but the outside expects our arguments back unchanged on failure
          val prefix = prefixSettings find (_ respondsTo arg)
          if (prefix.isDefined) {
            prefix.get tryToSet args
            rest
          }
          else if (arg contains ":") parseColonArg(arg) match {
            case Some(_)  => rest
            case None     => args
          }
          else parseNormalArg(arg, rest) match {
            case Some(xs) => xs
            case None     => args
          }
        }
    }
  }

  /** Initializes these settings for embedded use by type `T`.
  * The class loader defining `T` should provide resources `app.class.path`
  * and `boot.class.path`.  These resources should contain the application
  * and boot classpaths in the same form as would be passed on the command line.*/
  def embeddedDefaults[T: ClassTag]: Unit =
    embeddedDefaults(classTag[T].runtimeClass.getClassLoader)

  /** Initializes these settings for embedded use by a class from the given class loader.
  * The class loader for `T` should provide resources `app.class.path`
  * and `boot.class.path`.  These resources should contain the application
  * and boot classpaths in the same form as would be passed on the command line.*/
  def embeddedDefaults(loader: ClassLoader) {
    explicitParentLoader = Option(loader) // for the Interpreter parentClassLoader
    getClasspath("app", loader) foreach { classpath.value = _ }
    getClasspath("boot", loader) foreach { bootclasspath append _ }
  }

  /** The parent loader to use for the interpreter.*/
  private[nsc] var explicitParentLoader: Option[ClassLoader] = None

  /** Retrieves the contents of resource "${id}.class.path" from `loader`
  * (wrapped in Some) or None if the resource does not exist.*/
  private def getClasspath(id: String, loader: ClassLoader): Option[String] =
    Option(loader).flatMap(ld => Option(ld.getResource(id + ".class.path"))).map { cp =>
       Source.fromURL(cp).mkString
    }

  // a wrapper for all Setting creators to keep our list up to date
  private def add[T <: Setting](s: T): T = {
    allSettings += s
    s
  }

  def BooleanSetting(name: String, descr: String) = add(new BooleanSetting(name, descr))
  def ChoiceSetting(name: String, helpArg: String, descr: String, choices: List[String], default: String) =
    add(new ChoiceSetting(name, helpArg, descr, choices, default))
  def IntSetting(name: String, descr: String, default: Int, range: Option[(Int, Int)], parser: String => Option[Int]) = add(new IntSetting(name, descr, default, range, parser))
  def MultiStringSetting(name: String, arg: String, descr: String) = add(new MultiStringSetting(name, arg, descr))
  def OutputSetting(outputDirs: OutputDirs, default: String) = add(new OutputSetting(outputDirs, default))
  def PhasesSetting(name: String, descr: String, default: String = "") = add(new PhasesSetting(name, descr, default))
  def StringSetting(name: String, arg: String, descr: String, default: String) = add(new StringSetting(name, arg, descr, default))
  def ScalaVersionSetting(name: String, arg: String, descr: String, default: ScalaVersion) = add(new ScalaVersionSetting(name, arg, descr, default))
  def PathSetting(name: String, descr: String, default: String): PathSetting = {
    val prepend = StringSetting(name + "/p", "", "", "").internalOnly()
    val append = StringSetting(name + "/a", "", "", "").internalOnly()

    add(new PathSetting(name, descr, default, prepend, append))
  }
  def PrefixSetting(name: String, prefix: String, descr: String): PrefixSetting = add(new PrefixSetting(name, prefix, descr))

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
    private def checkDir(dir: AbstractFile, name: String, allowJar: Boolean = false): AbstractFile = (
      if (dir != null && dir.isDirectory)
        dir
// was:      else if (allowJar && dir == null && Path.isJarOrZip(name, false))
      else if (allowJar && dir == null && Jar.isJarOrZip(name, false))
        new PlainFile(Path(name))
      else
        throw new FatalError(name + " does not exist or is not a directory")
    )

    /** Set the single output directory. From now on, all files will
     *  be dumped in there, regardless of previous calls to 'add'.
     */
    def setSingleOutput(outDir: String) {
      val dst = AbstractFile.getDirectory(outDir)
      setSingleOutput(checkDir(dst, outDir, true))
    }

    def getSingleOutput: Option[AbstractFile] = singleOutDir

    /** Set the single output directory. From now on, all files will
     *  be dumped in there, regardless of previous calls to 'add'.
     */
    def setSingleOutput(dir: AbstractFile) {
      singleOutDir = Some(dir)
    }

    def add(src: AbstractFile, dst: AbstractFile) {
      singleOutDir = None
      outputDirs ::= ((src, dst))
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
          (outputs find (isBelow _).tupled) match {
            case Some((_, d)) => d
            case _ =>
              throw new FatalError("Could not find an output directory for "
                                   + src.path + " in " + outputs)
          }
      }
    }

    /** Return the source file path(s) which correspond to the given
     *  classfile path and SourceFile attribute value, subject to the
     *  condition that source files are arranged in the filesystem
     *  according to Java package layout conventions.
     *
     *  The given classfile path must be contained in at least one of
     *  the specified output directories. If it does not then this
     *  method returns Nil.
     *
     *  Note that the source file is not required to exist, so assuming
     *  a valid classfile path this method will always return a list
     *  containing at least one element.
     *
     *  Also that if two or more source path elements target the same
     *  output directory there will be two or more candidate source file
     *  paths.
     */
    def srcFilesFor(classFile : AbstractFile, srcPath : String) : List[AbstractFile] = {
      def isBelow(srcDir: AbstractFile, outDir: AbstractFile) =
        classFile.path.startsWith(outDir.path)

      singleOutDir match {
        case Some(d) =>
          d match {
              case _: VirtualDirectory | _: io.ZipArchive => Nil
              case _                   => List(d.lookupPathUnchecked(srcPath, false))
          }
        case None =>
          (outputs filter (isBelow _).tupled) match {
            case Nil => Nil
            case matches => matches.map(_._1.lookupPathUnchecked(srcPath, false))
          }
      }
    }
  }

  /** A base class for settings of all types.
   *  Subclasses each define a `value` field of the appropriate type.
   */
  abstract class Setting(val name: String, val helpDescription: String) extends AbsSetting with SettingValue with Mutable {
    /** Will be called after this Setting is set for any extra work. */
    private var _postSetHook: this.type => Unit = (x: this.type) => ()
    override def postSetHook(): Unit = _postSetHook(this)
    def withPostSetHook(f: this.type => Unit): this.type = { _postSetHook = f ; this }

    /** The syntax defining this setting in a help string */
    private var _helpSyntax = name
    override def helpSyntax: String = _helpSyntax
    def withHelpSyntax(s: String): this.type    = { _helpSyntax = s ; this }

    /** Abbreviations for this setting */
    private var _abbreviations: List[String] = Nil
    override def abbreviations = _abbreviations
    def withAbbreviation(s: String): this.type  = { _abbreviations ++= List(s) ; this }

    /** Optional dependency on another setting */
    private var dependency: Option[(Setting, String)] = None
    override def dependencies = dependency.toList
    def dependsOn(s: Setting, value: String): this.type = { dependency = Some((s, value)); this }

    private var _deprecationMessage: Option[String] = None
    override def deprecationMessage = _deprecationMessage
    def withDeprecationMessage(msg: String): this.type = { _deprecationMessage = Some(msg) ; this }
  }

  /** A setting represented by an integer */
  class IntSetting private[nsc](
    name: String,
    descr: String,
    val default: Int,
    val range: Option[(Int, Int)],
    parser: String => Option[Int])
  extends Setting(name, descr) {
    type T = Int
    protected var v: Int = default
    override def value: Int = v

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

    // Helper to generate a textual explanation of valid inputs
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

    def errorMsg() = errorFn("invalid setting for -"+name+" "+getValidText)

    def tryToSet(args: List[String]) =
      if (args.isEmpty) errorAndValue("missing argument", None)
      else parseArgument(args.head) match {
        case Some(i)  => value = i ; Some(args.tail)
        case None     => errorMsg ; None
      }

    def unparse: List[String] =
      if (value == default) Nil
      else List(name, value.toString)

    withHelpSyntax(name + " <n>")
  }

  /** A setting represented by a boolean flag (false, unless set) */
  class BooleanSetting private[nsc](
    name: String,
    descr: String)
  extends Setting(name, descr) {
    type T = Boolean
    protected var v: Boolean = false
    override def value: Boolean = v

    def tryToSet(args: List[String]) = { value = true ; Some(args) }
    def unparse: List[String] = if (value) List(name) else Nil
    override def tryToSetFromPropertyValue(s : String) {
      value = s.equalsIgnoreCase("true")
    }
  }

  /** A special setting for accumulating arguments like -Dfoo=bar. */
  class PrefixSetting private[nsc](
    name: String,
    prefix: String,
    descr: String)
  extends Setting(name, descr) {
    type T = List[String]
    protected var v: T = Nil

    def tryToSet(args: List[String]) = args match {
      case x :: xs if x startsWith prefix =>
        v = v :+ x
        Some(xs)
      case _  =>
        None
    }
    override def respondsTo(token: String) = token startsWith prefix
    def unparse: List[String] = value
  }

  /** A setting represented by a string, (`default` unless set) */
  class StringSetting private[nsc](
    name: String,
    val arg: String,
    descr: String,
    val default: String)
  extends Setting(name, descr) {
    type T = String
    protected var v: T = default

    def tryToSet(args: List[String]) = args match {
      case Nil      => errorAndValue("missing argument", None)
      case x :: xs  => value = x ; Some(xs)
    }
    def unparse: List[String] = if (value == default) Nil else List(name, value)

    withHelpSyntax(name + " <" + arg + ">")
  }

  /** A setting represented by a Scala version, (`default` unless set) */
  class ScalaVersionSetting private[nsc](
    name: String,
    val arg: String,
    descr: String,
    default: ScalaVersion)
  extends Setting(name, descr) {
    import ScalaVersion._
    
    type T = ScalaVersion
    protected var v: T = NoScalaVersion

    override def tryToSet(args: List[String]) = {
      value = default
      Some(args)
    }
    
    override def tryToSetColon(args: List[String]) = args match {
      case Nil      => value = default; Some(Nil)
      case x :: xs  => value = ScalaVersion(x, errorFn) ; Some(xs)
    }
    
    override def tryToSetFromPropertyValue(s: String) = tryToSet(List(s))
    
    def unparse: List[String] = if (value == NoScalaVersion) Nil else List(s"${name}:${value.unparse}")

    withHelpSyntax(s"${name}:<${arg}>")
  }

  class PathSetting private[nsc](
    name: String,
    descr: String,
    default: String,
    prependPath: StringSetting,
    appendPath: StringSetting)
  extends StringSetting(name, "path", descr, default) {
    import util.ClassPath.join
    def prepend(s: String) = prependPath.value = join(s, prependPath.value)
    def append(s: String) = appendPath.value = join(appendPath.value, s)

    override def value = join(
      prependPath.value,
      super.value,
      appendPath.value
    )
  }

  /** Set the output directory. */
  class OutputSetting private[nsc](
    private[nsc] val outputDirs: OutputDirs,
    default: String)
    extends StringSetting("-d", "directory|jar", "destination for generated classfiles.", default) {
      value = default
      override def value_=(str: String) {
        super.value_=(str)
        try outputDirs.setSingleOutput(str)
        catch { case FatalError(msg) => errorFn(msg) }
      }
  }

  /** A setting that accumulates all strings supplied to it,
   *  until it encounters one starting with a '-'. */
  class MultiStringSetting private[nsc](
    name: String,
    val arg: String,
    descr: String)
  extends Setting(name, descr) {
    type T = List[String]
    protected var v: T = Nil
    def appendToValue(str: String) { value ++= List(str) }

    def tryToSet(args: List[String]) = {
      val (strings, rest) = args span (x => !x.startsWith("-"))
      strings foreach appendToValue

      Some(rest)
    }
    override def tryToSetColon(args: List[String]) = tryToSet(args)
    override def tryToSetFromPropertyValue(s: String) = tryToSet(s.trim.split(',').toList)
    def unparse: List[String] = value map (name + ":" + _)

    withHelpSyntax(name + ":<" + arg + ">")
  }

  /** A setting represented by a string in a given set of `choices`,
   *  (`default` unless set).
   */
  class ChoiceSetting private[nsc](
    name: String,
    helpArg: String,
    descr: String,
    override val choices: List[String],
    val default: String)
  extends Setting(name, descr + choices.mkString(" (", ",", ") default:" + default)) {
    type T = String
    protected var v: T = default
    def indexOfChoice: Int = choices indexOf value

    private def usageErrorMessage = {
      "Usage: %s:<%s>\n  where <%s> choices are %s (default: %s)\n".format(
        name, helpArg, helpArg, choices mkString ", ", default)
    }
    def tryToSet(args: List[String]) = errorAndValue(usageErrorMessage, None)

    override def tryToSetColon(args: List[String]) = args match {
      case Nil                            => errorAndValue(usageErrorMessage, None)
      case List(x) if choices contains x  => value = x ; Some(Nil)
      case List(x)                        => errorAndValue("'" + x + "' is not a valid choice for '" + name + "'", None)
      case xs                             => errorAndValue("'" + name + "' does not accept multiple arguments.", None)
    }
    def unparse: List[String] =
      if (value == default) Nil else List(name + ":" + value)
    override def tryToSetFromPropertyValue(s: String) = tryToSetColon(s::Nil)

    withHelpSyntax(name + ":<" + helpArg + ">")
  }

  private def mkPhasesHelp(descr: String, default: String) = {
    descr + " <phases>" + (
      if (default == "") "" else " (default: " + default + ")"
    )
  }

  /** A setting represented by a list of strings which should be prefixes of
   *  phase names. This is not checked here, however.  Alternatively the string
   *  `"all"` can be used to represent all phases.
   *  (the empty list, unless set)
   */
  class PhasesSetting private[nsc](
    name: String,
    descr: String,
    default: String
  ) extends Setting(name, mkPhasesHelp(descr, default)) {
    private[nsc] def this(name: String, descr: String) = this(name, descr, "")

    type T = List[String]
    protected var v: T = Nil
    override def value = if (v contains "all") List("all") else super.value
    private lazy val (numericValues, stringValues) =
      value filterNot (_ == "" ) partition (_ forall (ch => ch.isDigit || ch == '-'))

    /** A little ad-hoc parsing.  If a string is not the name of a phase, it can also be:
     *    a phase id: 5
     *    a phase id range: 5-10 (inclusive of both ends)
     *    a range with no start: -5 means up to and including 5
     *    a range with no end: 10- means 10 until completion.
     */
    private def stringToPhaseIdTest(s: String): Int => Boolean = (s indexOf '-') match {
      case -1  => (_ == s.toInt)
      case 0   => (_ <= s.tail.toInt)
      case idx =>
        if (s.last == '-') (_ >= s.init.toInt)
        else (s splitAt idx) match {
          case (s1, s2) => (id => id >= s1.toInt && id <= s2.tail.toInt)
        }
    }
    private lazy val phaseIdTest: Int => Boolean =
      (numericValues map stringToPhaseIdTest) match {
        case Nil    => _ => false
        case fns    => fns.reduceLeft((f1, f2) => id => f1(id) || f2(id))
      }

    def tryToSet(args: List[String]) =
      if (default == "") errorAndValue("missing phase", None)
      else { tryToSetColon(List(default)) ; Some(args) }

    override def tryToSetColon(args: List[String]) = args match {
      case Nil  => if (default == "") errorAndValue("missing phase", None) else tryToSetColon(List(default))
      case xs   => value = (value ++ xs).distinct.sorted ; Some(Nil)
    }
    // we slightly abuse the usual meaning of "contains" here by returning
    // true if our phase list contains "all", regardless of the incoming argument
    def contains(phName: String)     = doAllPhases || containsName(phName)
    def containsName(phName: String) = stringValues exists (phName startsWith _)
    def containsId(phaseId: Int)     = phaseIdTest(phaseId)
    def containsPhase(ph: Phase)     = contains(ph.name) || containsId(ph.id)

    def doAllPhases = stringValues contains "all"
    def unparse: List[String] = value map (name + ":" + _)

    withHelpSyntax(
      if (default == "") name + ":<phases>"
      else name + "[:phases]"
    )
  }
}
