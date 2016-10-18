/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools
package nsc
package settings

import io.{ AbstractFile, Jar, Path, PlainFile, VirtualDirectory }
import scala.collection.generic.Clearable
import scala.io.Source
import scala.reflect.internal.util.StringOps
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
      // discard empties, sometimes they appear because of ant or etc.
      // but discard carefully, because an empty string is valid as an argument
      // to an option, e.g. -cp "" .  So we discard them only when they appear
      // where an option should be, not where an argument to an option should be.
      case "" :: xs =>
        loop(xs, residualArgs)
      case x :: xs  =>
        if (x startsWith "-") {
          parseParams(args) match {
            case newArgs if newArgs eq args => errorFn(s"bad option: '$x'") ; (false, args)
            case newArgs                    => loop(newArgs, residualArgs)
          }
        }
        else if (processAll)
          loop(xs, residualArgs :+ x)
        else
          (checkDependencies, args)
    }
    loop(arguments, Nil)
  }
  def processArgumentString(params: String) = processArguments(splitParams(params), processAll = true)

  /** Create a new Settings object, copying all user-set values.
   */
  def copy(): Settings = {
    val s = new Settings()
    s.processArguments(recreateArgs, processAll = true)
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
  def splitParams(line: String) = cmd.CommandLineParser.tokenize(line, errorFn)

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

    // -Xfoo: clears Clearables
    def clearIfExists(cmd: String): Option[List[String]] = lookupSetting(cmd) match {
      case Some(c: Clearable) => c.clear() ; Some(Nil)
      case Some(s)            => s.errorAndValue(s"Missing argument to $cmd", None)
      case None               => None
    }

    // if arg is of form -Xfoo:bar,baz,quux
    // the entire arg is consumed, so return None for failure
    // any non-Nil return value means failure and we return s unmodified
    def parseColonArg(s: String): Option[List[String]] =
      if (s endsWith ":") {
        clearIfExists(s.init)
      } else {
        for {
          (p, args) <- StringOps.splitWhere(s, _ == ':', doDropIndex = true)
          rest      <- tryToSetIfExists(p, (args split ",").toList, (s: Setting) => s.tryToSetColon _)
        } yield rest
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
  def embeddedDefaults[T: ClassTag]: Unit = // called from sbt and repl
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
  def IntSetting(name: String, descr: String, default: Int, range: Option[(Int, Int)], parser: String => Option[Int]) =
    add(new IntSetting(name, descr, default, range, parser))
  def MultiStringSetting(name: String, arg: String, descr: String) = add(new MultiStringSetting(name, arg, descr))
  def MultiChoiceSetting[E <: MultiChoiceEnumeration](name: String, helpArg: String, descr: String, domain: E, default: Option[List[String]] = None) =
    add(new MultiChoiceSetting[E](name, helpArg, descr, domain, default))
  def OutputSetting(outputDirs: OutputDirs, default: String) = add(new OutputSetting(outputDirs, default))
  def PhasesSetting(name: String, descr: String, default: String = "") = add(new PhasesSetting(name, descr, default))
  def StringSetting(name: String, arg: String, descr: String, default: String) = add(new StringSetting(name, arg, descr, default))
  def ScalaVersionSetting(name: String, arg: String, descr: String, initial: ScalaVersion, default: Option[ScalaVersion] = None) =
    add(new ScalaVersionSetting(name, arg, descr, initial, default))
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
    def add(srcDir: String, outDir: String): Unit = // used in ide?
      add(checkDir(AbstractFile.getDirectory(srcDir), srcDir),
          checkDir(AbstractFile.getDirectory(outDir), outDir))

    /** Check that dir is exists and is a directory. */
    private def checkDir(dir: AbstractFile, name: String, allowJar: Boolean = false): AbstractFile = (
      if (dir != null && dir.isDirectory)
        dir
      else if (allowJar && dir == null && Jar.isJarOrZip(name, examineFile = false))
        new PlainFile(Path(name))
      else
        throw new FatalError(name + " does not exist or is not a directory")
    )

    /** Set the single output directory. From now on, all files will
     *  be dumped in there, regardless of previous calls to 'add'.
     */
    def setSingleOutput(outDir: String) {
      val dst = AbstractFile.getDirectory(outDir)
      setSingleOutput(checkDir(dst, outDir, allowJar = true))
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
              case _                   => List(d.lookupPathUnchecked(srcPath, directory = false))
          }
        case None =>
          (outputs filter (isBelow _).tupled) match {
            case Nil => Nil
            case matches => matches.map(_._1.lookupPathUnchecked(srcPath, directory = false))
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

  /** A setting represented by an integer. */
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
      if (isInputValid(s)) super.value_=(s) else errorMsg()

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
        case None     => errorMsg() ; None
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
    override def tryToSetFromPropertyValue(s : String) { // used from ide
      value = s.equalsIgnoreCase("true")
    }
    override def tryToSetColon(args: List[String]) = args match {
      case Nil     => tryToSet(Nil)
      case List(x) =>
        if (x.equalsIgnoreCase("true")) {
          value = true
          Some(Nil)
        } else if (x.equalsIgnoreCase("false")) {
          value = false
          Some(Nil)
        } else errorAndValue(s"'$x' is not a valid choice for '$name'", None)
      case _       => errorAndValue(s"'$name' accepts only one boolean value", None)
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

  /** A setting represented by a Scala version.
    * The `initial` value is used if the setting is not specified.
    * The `default` value is used if the option is specified without argument (e.g., `-Xmigration`).
    */
  class ScalaVersionSetting private[nsc](
    name: String,
    val arg: String,
    descr: String,
    initial: ScalaVersion,
    default: Option[ScalaVersion])
  extends Setting(name, descr) {
    type T = ScalaVersion
    protected var v: T = initial

    // This method is invoked if there are no colonated args. In this case the default value is
    // used. No arguments are consumed.
    override def tryToSet(args: List[String]) = {
      default match {
        case Some(d) => value = d
        case None => errorFn(s"$name requires an argument, the syntax is $helpSyntax")
      }
      Some(args)
    }

    override def tryToSetColon(args: List[String]) = args match {
      case x :: xs  => value = ScalaVersion(x, errorFn); Some(xs)
      case nil      => Some(nil)
    }

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

    override def isDefault = super.isDefault && prependPath.isDefault && appendPath.isDefault
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

  /**
   * Each [[MultiChoiceSetting]] takes a MultiChoiceEnumeration as domain. The enumeration may
   * use the Choice class to define values, or simply use the default `Value` constructor:
   *
   *     object SettingDomain extends MultiChoiceEnumeration { val arg1, arg2 = Value }
   *
   * Or
   *
   *     object SettingDomain extends MultiChoiceEnumeration {
   *       val arg1 = Choice("arg1", "help")
   *       val arg2 = Choice("arg2", "help")
   *     }
   *
   * Choices with a non-empty `expandsTo` enable other options. Note that expanding choices are
   * not present in the multiChoiceSetting.value set, only their expansion.
   */
  abstract class MultiChoiceEnumeration extends Enumeration {
    case class Choice(name: String, help: String = "", expandsTo: List[Choice] = Nil) extends Val(name)
  }

  /**
   * A Setting that collects string-valued settings from an enumerated domain.
   *  - These choices can be turned on or off: "-option:on,-off"
   *  - If an option is set both on and off, then the option is on
   *  - The choice "_" enables all choices that have not been explicitly disabled
   *
   *  Arguments can be provided in colonated or non-colonated mode, i.e. "-option a b" or
   *  "-option:a,b". Note that arguments starting with a "-" can only be provided in colonated mode,
   *  otherwise they are interpreted as a new option.
   *
   *  In non-colonated mode, the setting stops consuming arguments at the first non-choice,
   *  i.e. "-option a b c" only consumes "a" and "b" if "c" is not a valid choice.
   *
   *  @param name         command-line setting name, eg "-Xlint"
   *  @param helpArg      help description for the kind of arguments it takes, eg "warning"
   *  @param descr        description of the setting
   *  @param domain       enumeration of choices implementing MultiChoice, or the string value is
   *                      taken for the name
   *  @param default      If Some(args), the default options if none are provided. If None, an
   *                      error is printed if there are no arguments.
   */
  class MultiChoiceSetting[E <: MultiChoiceEnumeration] private[nsc](
    name: String,
    helpArg: String,
    descr: String,
    val domain: E,
    val default: Option[List[String]]
  ) extends Setting(name, s"$descr: `_' for all, `$name:help' to list") with Clearable {

    withHelpSyntax(s"$name:<_,$helpArg,-$helpArg>")

    object ChoiceOrVal {
      def unapply(a: domain.Value): Option[(String, String, List[domain.Choice])] = a match {
        case c: domain.Choice => Some((c.name, c.help, c.expandsTo))
        case v: domain.Value  => Some((v.toString, "", Nil))
      }
    }

    type T = domain.ValueSet
    protected var v: T = domain.ValueSet.empty

    // Explicitly enabled or disabled. Yeas may contain expanding options, nays may not.
    private var yeas = domain.ValueSet.empty
    private var nays = domain.ValueSet.empty

    // Asked for help
    private var sawHelp = false
    // Wildcard _ encountered
    private var sawAll  = false

    private def badChoice(s: String) = errorFn(s"'$s' is not a valid choice for '$name'")
    private def isChoice(s: String) = (s == "_") || (choices contains pos(s))

    private def pos(s: String) = s stripPrefix "-"
    private def isPos(s: String) = !(s startsWith "-")

    override val choices: List[String] = domain.values.toList map {
      case ChoiceOrVal(name, _, _) => name
    }

    def descriptions: List[String] = domain.values.toList map {
      case ChoiceOrVal(_, "", x :: xs) => "Enables the options "+ (x :: xs).map(_.name).mkString(", ")
      case ChoiceOrVal(_, descr, _)    => descr
      case _                           => ""
    }

    /** (Re)compute from current yeas, nays, wildcard status. */
    def compute() = {
      def simple(v: domain.Value) = v match {
        case ChoiceOrVal(_, _, Nil) => true
        case _ => false
      }

      /**
       * Expand an expanding option, if necessary recursively. Expanding options are not included in
       * the result (consistent with "_", which is not in `value` either).
       *
       * Note: by precondition, options in nays are not expanding, they can only be leaves.
       */
      def expand(vs: domain.ValueSet): domain.ValueSet = vs flatMap {
        case c @ ChoiceOrVal(_, _, Nil) => domain.ValueSet(c)
        case ChoiceOrVal(_, _, others)  => expand(domain.ValueSet(others: _*))
      }

      // yeas from _ or expansions are weak: an explicit nay will disable them
      val weakYeas = if (sawAll) domain.values filter simple else expand(yeas filterNot simple)
      value = (yeas filter simple) | (weakYeas &~ nays)
    }

    /** Add a named choice to the multichoice value. */
    def add(arg: String) = arg match {
      case _ if !isChoice(arg) =>
        badChoice(arg)
      case "_" =>
        sawAll = true
        compute()
      case _ if isPos(arg) =>
        yeas += domain withName arg
        compute()
      case _ =>
        val choice = domain withName pos(arg)
        choice match {
          case ChoiceOrVal(_, _, _ :: _) => errorFn(s"'${pos(arg)}' cannot be negated, it enables other arguments")
          case _ =>
        }
        nays += choice
        compute()
    }

    def tryToSet(args: List[String])                  = tryToSetArgs(args, halting = true)
    override def tryToSetColon(args: List[String])    = tryToSetArgs(args, halting = false)
    override def tryToSetFromPropertyValue(s: String) = tryToSet(s.trim.split(',').toList) // used from ide

    /** Try to set args, handling "help" and default.
     *  The "halting" parameter means args were "-option a b c -else" so halt
     *  on "-else" or other non-choice. Otherwise, args were "-option:a,b,c,d",
     *  so process all and report non-choices as errors.
     *  @param args args to process
     *  @param halting stop on non-arg
     */
    private def tryToSetArgs(args: List[String], halting: Boolean) = {
      val added = collection.mutable.ListBuffer.empty[String]

      def tryArg(arg: String) = arg match {
        case "help"           => sawHelp = true
        case s if isChoice(s) => added += s // this case also adds "_"
        case s                => badChoice(s)
      }
      def loop(args: List[String]): List[String] = args match {
        case arg :: _ if halting && (!isPos(arg) || !isChoice(arg)) => args
        case arg :: rest => tryArg(arg) ; loop(rest)
        case Nil         => Nil
      }
      val rest = loop(args)

      // if no arg consumed, use defaults or error; otherwise, add what they added
      if (rest.size == args.size) default match {
        case Some(defaults) => defaults foreach add
        case None => errorFn(s"'$name' requires an option. See '$name:help'.")
      } else {
        added foreach add
      }

      Some(rest)
    }

    def contains(choice: domain.Value): Boolean = value contains choice

    def isHelping: Boolean = sawHelp

    def help: String = {
      val describe: ((String, String)) => String = {
        val choiceWidth = choices.map(_.length).max + 1
        val formatStr   = s"  %-${choiceWidth}s %s"
        locally {
          case (choice, description) => formatStr.format(choice, description)
        }
      }
      val verboseDefault = default match {
        case Some("_" :: Nil) => Some("All choices are enabled by default." :: Nil)
        case _ => default
      }
      val orelse = verboseDefault.map(_.mkString(f"%nDefault: ", ", ", f"%n")).getOrElse("")
      choices.zipAll(descriptions, "", "").map(describe).mkString(f"${descr}%n", f"%n", orelse)
    }

    def clear(): Unit         = {
      v = domain.ValueSet.empty
      yeas = domain.ValueSet.empty
      nays = domain.ValueSet.empty
      sawAll = false
      sawHelp = false
    }
    def unparse: List[String] = value.toList map (s => s"$name:$s")
    def contains(s: String)   = domain.values.find(_.toString == s).exists(value.contains)
  }

  /** A setting that accumulates all strings supplied to it,
   *  until it encounters one starting with a '-'.
   */
  class MultiStringSetting private[nsc](
    name: String,
    val arg: String,
    descr: String)
  extends Setting(name, descr) with Clearable {
    type T = List[String]
    protected var v: T = Nil
    def appendToValue(str: String) = value ++= List(str)

    // try to set. halting means halt at first non-arg
    protected def tryToSetArgs(args: List[String], halting: Boolean) = {
      def loop(args: List[String]): List[String] = args match {
        case arg :: rest => if (halting && (arg startsWith "-")) args else { appendToValue(arg) ; loop(rest) }
        case Nil         => Nil
      }
      Some(loop(args))
    }
    def tryToSet(args: List[String])                  = tryToSetArgs(args, halting = true)
    override def tryToSetColon(args: List[String])    = tryToSetArgs(args, halting = false)
    override def tryToSetFromPropertyValue(s: String) = tryToSet(s.trim.split(',').toList) // used from ide

    def clear(): Unit         = (v = Nil)
    def unparse: List[String] = value map (name + ":" + _)
    def contains(s: String)   = value contains s

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

    private def usageErrorMessage = f"Usage: $name:<$helpArg>%n where <$helpArg> choices are ${choices mkString ", "} (default: $default)%n"

    def tryToSet(args: List[String]) = errorAndValue(usageErrorMessage, None)

    override def tryToSetColon(args: List[String]) = args match {
      case Nil                            => errorAndValue(usageErrorMessage, None)
      case List(x) if choices contains x  => value = x ; Some(Nil)
      case List(x)                        => errorAndValue("'" + x + "' is not a valid choice for '" + name + "'", None)
      case xs                             => errorAndValue("'" + name + "' does not accept multiple arguments.", None)
    }
    def unparse: List[String] =
      if (value == default) Nil else List(name + ":" + value)
    override def tryToSetFromPropertyValue(s: String) = tryToSetColon(s::Nil) // used from ide

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
  ) extends Setting(name, mkPhasesHelp(descr, default)) with Clearable {
    private[nsc] def this(name: String, descr: String) = this(name, descr, "")

    type T = List[String]
    private[this] var _v: T = Nil
    private[this] var _numbs: List[(Int,Int)] = Nil
    private[this] var _names: T = Nil
    //protected var v: T = Nil
    protected def v: T = _v
    protected def v_=(t: T): Unit = {
      // throws NumberFormat on bad range (like -5-6)
      def asRange(s: String): (Int,Int) = (s indexOf '-') match {
        case -1 => (s.toInt, s.toInt)
        case 0  => (-1, s.tail.toInt)
        case i if s.last == '-' => (s.init.toInt, Int.MaxValue)
        case i  => (s.take(i).toInt, s.drop(i+1).toInt)
      }
      val numsAndStrs = t filter (_.nonEmpty) partition (_ forall (ch => ch.isDigit || ch == '-'))
      _numbs = numsAndStrs._1 map asRange
      _names = numsAndStrs._2
      _v     = t
    }
    override def value = if (v contains "all") List("all") else super.value // i.e., v
    private def numericValues = _numbs
    private def stringValues  = _names
    private def phaseIdTest(i: Int): Boolean = numericValues exists (_ match {
      case (min, max) => min <= i && i <= max
    })

    def tryToSet(args: List[String]) =
      if (default == "") errorAndValue("missing phase", None)
      else tryToSetColon(List(default)) map (_ => args)

    override def tryToSetColon(args: List[String]) = try {
      args match {
        case Nil  => if (default == "") errorAndValue("missing phase", None)
                     else tryToSetColon(List(default))
        case xs   => value = (value ++ xs).distinct.sorted ; Some(Nil)
      }
    } catch { case _: NumberFormatException => None }

    def clear(): Unit = (v = Nil)

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

  /** Internal use - syntax enhancements. */
  protected class EnableSettings[T <: BooleanSetting](val s: T) {
    def enablingIfNotSetByUser(toEnable: List[BooleanSetting]): s.type = s withPostSetHook (_ => toEnable foreach (sett => if (!sett.isSetByUser) sett.value = s.value))
    def enabling(toEnable: List[BooleanSetting]): s.type = s withPostSetHook (_ => toEnable foreach (_.value = s.value))
    def disabling(toDisable: List[BooleanSetting]): s.type = s withPostSetHook (_ => toDisable foreach (_.value = !s.value))
    def andThen(f: s.T => Unit): s.type = s withPostSetHook (setting => f(setting.value))
  }
  import scala.language.implicitConversions
  protected implicit def installEnableSettings[T <: BooleanSetting](s: T): EnableSettings[T] = new EnableSettings(s)
}
