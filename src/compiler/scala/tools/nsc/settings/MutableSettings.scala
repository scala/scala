/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

// $Id$

package scala.tools
package nsc
package settings

import io.{AbstractFile, Path, PlainFile, VirtualDirectory}
import scala.annotation.tailrec
import scala.collection.mutable.Clearable
import scala.io.Source
import scala.reflect.internal.util.{SomeOfNil, StringOps}
import scala.reflect.{ClassTag, classTag}
import scala.sys.process.{Parser => CommandLineParser}

/** A mutable Settings object.
 */
class MutableSettings(val errorFn: String => Unit, val pathFactory: PathFactory)
              extends scala.reflect.internal.settings.MutableSettings
                 with AbsSettings
                 with ScalaSettings {
  def this(errorFn: String => Unit) = this(errorFn, DefaultPathFactory)
  type ResultOfTryToSet = List[String]

  def withErrorFn(errorFn: String => Unit): MutableSettings = {
    val settings = new MutableSettings(errorFn, pathFactory)
    copyInto(settings)
    settings
  }

  def copyInto(settings: MutableSettings): Unit = {
    allSettings.valuesIterator foreach { thisSetting =>
      val otherSetting = settings.allSettings.get(thisSetting.name)
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
    @tailrec
    def loop(args: List[String], residualArgs: List[String]): (Boolean, List[String]) = args match {
      case Nil                     => (checkDependencies, residualArgs)
      case "--" :: xs              => (checkDependencies, xs)
      // discard empties, sometimes they appear because of ant or etc.
      // but discard carefully, because an empty string is valid as an argument
      // to an option, e.g. -cp "" .  So we discard them only when they appear
      // where an option should be, not where an argument to an option should be.
      case "" :: xs                => loop(xs, residualArgs)
      case (x @ Optionlike()) :: _ =>
        parseParams(args) match {
          case newArgs if newArgs eq args => errorFn(s"bad option: '$x'") ; (false, args)
          case newArgs                    => loop(newArgs, residualArgs)
        }
      case x :: xs if processAll   => loop(xs, residualArgs :+ x)
      case _                       => (checkDependencies, args)
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

  /** A list pairing source directories with their respective output directory.
   *
   *  Tools may set outputDirs programmatically.
   *
   *  The `-d` commandline option sets a single directory for all sources.
   */
  lazy val outputDirs = new OutputDirs

  /** A list of settings which act based on prefix rather than an exact
   *  match.  This is basically -D and -J.
   */
  lazy val prefixSettings = allSettings.valuesIterator.collect { case x: PrefixSetting => x }.toList

  /** Split the given line into parameters.
   */
  def splitParams(line: String) = CommandLineParser.tokenize(line, errorFn)

  /** Returns any unprocessed arguments.
   */
  protected def parseParams(args: List[String]): List[String] = {
    // verify command exists and call setter
    def tryToSetIfExists(
      cmd: String,
      args: List[String],
      setter: (Setting) => (List[String] => Option[List[String]])
    ): Option[List[String]] =
      lookupSetting(cmd).flatMap(setter(_)(args))

    // -Xfoo: clears Clearables
    def clearIfExists(cmd: String): Option[List[String]] = lookupSetting(cmd) match {
      case Some(c: Clearable) => c.clear() ; SomeOfNil
      case Some(s)            => s.errorAndValue(s"Missing argument to $cmd", None)
      case None               => None
    }

    // if arg is of form -Xfoo:bar,baz,quux
    // the entire arg is consumed, so return None for failure
    // any non-Nil return value means failure and we return s unmodified
    def parseColonArg(s: String): Option[List[String]] =
      if (s endsWith ":")
        clearIfExists(s.init)
      else
        StringOps.splitWhere(s, _ == ':', doDropIndex = true).flatMap {
          // p:arg:a,b,c is taken as arg with selections a,b,c for a multichoice setting
          case (p, args) if args.contains(":") && lookupSetting(p).map(_.isInstanceOf[MultiChoiceSetting[_]]).getOrElse(false) => tryToSetIfExists(p, List(args), (s: Setting) => s.tryToSetColon(_))
          case (p, args) => tryToSetIfExists(p, args.split(",").toList, (s: Setting) => s.tryToSetColon(_))
        }

    // if arg is of form -Xfoo or -Xfoo bar (name = "-Xfoo")
    def parseNormalArg(p: String, args: List[String]): Option[List[String]] =
      tryToSetIfExists(p, args, (s: Setting) => s.tryToSet(_))

    args match {
      case Nil      => Nil
      case "-" :: _ => errorFn("'-' is not a valid argument.") ; args
      case (arg @ Optionlike()) :: rest  =>
        // we dispatch differently based on the appearance of p:
        // 1) If it matches a prefix setting it is sent there directly.
        // 2) If it has a : it is presumed to be -Xfoo:bar,baz
        // 3) Otherwise, the whole string should be a command name
        //
        // Internally we use Option[List[String]] to discover error,
        // but the outside expects our arguments back unchanged on failure
        val prefix = prefixSettings.find(_ respondsTo arg)
        prefix.map { setting => setting.tryToSet(args); rest }
        .orElse {
          if (arg contains ":") parseColonArg(arg).map(_ => rest)
          else parseNormalArg(arg, rest)
        }
        .getOrElse(args)
      case arg :: _ => errorFn(s"Argument '$arg' does not start with '-'.") ; args
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
  def embeddedDefaults(loader: ClassLoader): Unit = {
    explicitParentLoader = Option(loader) // for the Interpreter parentClassLoader
    getClasspath("app", loader) foreach { classpath.value = _ }
    getClasspath("boot", loader) foreach { bootclasspath append _ }
  }

  /** The parent loader to use for the interpreter.*/
  private[nsc] var explicitParentLoader: Option[ClassLoader] = None

  /** Retrieves the contents of resource "${id}.class.path" from `loader`
  * (wrapped in Some) or None if the resource does not exist.*/
  private def getClasspath(id: String, loader: ClassLoader): Option[String] =
    for {
      ld <- Option(loader)
      r  <- Option(ld.getResource(s"$id.class.path"))
    } yield Source.fromURL(r).mkString

  // a wrapper for all Setting creators to keep our list up to date
  private def add[T <: Setting](s: T): T = {
    allSettings(s.name) = s
    s
  }

  def BooleanSetting(name: String, descr: String, default: Boolean = false) = add(new BooleanSetting(name, descr, default))
  def ChoiceSetting(name: String, helpArg: String, descr: String, choices: List[String], default: String, choicesHelp: List[String] = Nil) =
    add(new ChoiceSetting(name, helpArg, descr, choices, default, choicesHelp))
  def IntSetting(name: String, descr: String, default: Int, range: Option[(Int, Int)], parser: String => Option[Int]) =
    add(new IntSetting(name, descr, default, range, parser))
  def MultiStringSetting(name: String, arg: String, descr: String, default: List[String] = Nil, helpText: Option[String] = None, prepend: Boolean = false) =
    add(new MultiStringSetting(name, arg, descr, default, helpText, prepend))
  def MultiChoiceSetting[E <: MultiChoiceEnumeration](name: String, helpArg: String, descr: String, domain: E, default: Option[List[String]] = None, helpText: Option[String] = None) =
    add(new MultiChoiceSetting[E](name, helpArg, descr, domain, default, helpText))
  def OutputSetting(default: String) = add(new OutputSetting(default))
  def PhasesSetting(name: String, descr: String, default: String = "") = add(new PhasesSetting(name, descr, default))
  def StringSetting(name: String, arg: String, descr: String, default: String = "", helpText: Option[String] = None) = add(new StringSetting(name, arg, descr, default, helpText))
  def ScalaVersionSetting(name: String, arg: String, descr: String, initial: ScalaVersion, default: Option[ScalaVersion] = None, helpText: Option[String] = None) =
    add(new ScalaVersionSetting(name, arg, descr, initial, default, helpText))
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

    /** Add a destination directory for sources found under `srcDir`.
     *  Both directories should exist.
     */
    // used in ide?
    def add(srcDir: String, outDir: String): Unit = {
      // Check that dir exists and is a directory.
      def checkDir(name: String): AbstractFile = {
        val dir = pathFactory.getDirectory(name)
        if (dir != null && dir.isDirectory) dir
        else throw new FatalError(s"$name does not exist or is not a directory")
      }
      add(checkDir(srcDir), checkDir(outDir))
    }

    /** Check either existing dir, or if not dir in path, a jar/zip which may not yet exist. */
    private def checkDirOrJar(name: String): AbstractFile = {
      val dir = pathFactory.getDirectory(name)
      if (dir != null && dir.isDirectory) dir
      else if (dir == null && Path.isExtensionJarOrZip(name)) new PlainFile(Path(name))
      else throw new FatalError(s"$name does not exist or is not a directory")
    }

    /** Set the single output directory. From now on, all files will
     *  be dumped in there, regardless of previous calls to 'add'.
     */
    def setSingleOutput(outDir: String): Unit = setSingleOutput(checkDirOrJar(outDir))

    def getSingleOutput: Option[AbstractFile] = singleOutDir

    /** Set the single output directory. From now on, all files will
     *  be dumped in there, regardless of previous calls to 'add'.
     */
    def setSingleOutput(dir: AbstractFile): Unit = singleOutDir = Some(dir)

    def add(src: AbstractFile, dst: AbstractFile): Unit = {
      singleOutDir = None
      outputDirs ::= ((src, dst))
    }

    /** Return the list of source-destination directory pairs. */
    def outputs: List[(AbstractFile, AbstractFile)] = outputDirs

    /** Return the output directory for the given file.
     */
    def outputDirFor(src: AbstractFile): AbstractFile =
      singleOutDir.getOrElse(outputs.find { case (srcDir, _) => src.path.startsWith(srcDir.path) } match {
        case Some((_, d)) => d
        case _ => throw new FatalError(s"Could not find an output directory for ${src.path} in ${outputs}")
      })

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
    def srcFilesFor(classFile : AbstractFile, srcPath : String) : List[AbstractFile] =
      singleOutDir match {
        case Some(_: VirtualDirectory | _: io.ZipArchive) => Nil
        case Some(d) => List(d.lookupPathUnchecked(srcPath, directory = false))
        case None =>
          outputs.filter { case (_, outDir) => classFile.path.startsWith(outDir.path) } match {
            case Nil => Nil
            case matches => matches.map(_._1.lookupPathUnchecked(srcPath, directory = false))
          }
      }
  }

  /** A base class for settings of all types.
   *  Subclasses each define a `value` field of the appropriate type.
   */
  abstract class Setting(val name: String, val helpDescription: String) extends AbsSetting with SettingValue {

    /** Will be called after this Setting is set for any extra work. */
    private[this] var _postSetHook: this.type => Unit = (_: this.type) => ()
    override def postSetHook(): Unit = _postSetHook(this)
    def withPostSetHook(f: this.type => Unit): this.type = { _postSetHook = f ; this }

    /** The syntax defining this setting in a help string */
    private[this] var _helpSyntax = name
    override def helpSyntax: String = _helpSyntax
    def withHelpSyntax(s: String): this.type    = { _helpSyntax = s ; this }

    /** Abbreviations for this setting */
    private[this] var _abbreviations: List[String] = Nil
    override def abbreviations = _abbreviations
    def withAbbreviation(s: String): this.type  = { _abbreviations ++= List(s) ; this }

    /** Optional dependency on another setting */
    private var dependency: Option[(Setting, String)] = None
    override def dependencies = dependency.toList
    def dependsOn(s: Setting, value: String): this.type = { dependency = Some((s, value)); this }

    private[this] var _deprecationMessage: Option[String] = None
    override def deprecationMessage = _deprecationMessage
    def withDeprecationMessage(msg: String): this.type = { _deprecationMessage = Some(msg) ; this }

    def reset(): Unit
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
      case (IntMin, x)        => f"must be less than or equal to $x%d"
      case (x, IntMax)        => f"must be greater than or equal to $x%d"
      case _                  => f"must be between $min%d and $max%d"
    }

    // Ensure that the default value is actually valid
    assert(isInputValid(default))

    def parseArgument(x: String): Option[Int] = parser(x) orElse x.toIntOption

    def errorMsg() = errorFn(s"invalid setting for $name $getValidText")

    def tryToSet(args: List[String]): Option[ResultOfTryToSet] =
      args match {
        case h :: rest =>
          parseArgument(h) match {
            case Some(i) => value = i; Some(rest)
            case None    => errorMsg(); None
          }
        case Nil => errorAndValue("missing argument", None)
      }
    def tryToSetColon(args: List[String]): Option[ResultOfTryToSet] =
      args match {
        case Nil | _ :: Nil => tryToSet(args)
        case _              => errorAndValue("too many arguments", None)
      }

    def unparse: List[String] =
      if (value == default) Nil
      else List(name, value.toString)

    withHelpSyntax(s"$name <n>")

    override def reset() = v = default
  }

  /** A setting that is a boolean flag, with default as specified. */
  class BooleanSetting private[nsc](name: String, descr: String, default: Boolean) extends Setting(name, s"$descr [$default]") {
    type T = Boolean
    protected var v: Boolean = default
    override def value: Boolean = v

    def tryToSet(args: List[String]) = { value = true ; Some(args) }
    def unparse: List[String] = if (value) List(name) else Nil
    // used from ide
    override def tryToSetFromPropertyValue(s : String): Unit = value = s.equalsIgnoreCase("true")
    override def tryToSetColon(args: List[String]) = args match {
      case Nil     => tryToSet(Nil)
      case List(x) =>
        if (x.equalsIgnoreCase("true")) {
          value = true
          SomeOfNil
        } else if (x.equalsIgnoreCase("false")) {
          value = false
          SomeOfNil
        } else errorAndValue(s"'$x' is not a valid choice for '$name'", None)
      case _       => errorAndValue(s"'$name' accepts only one boolean value", None)
    }
    override def reset() = {
      v = default
      setByUser = false
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
    def tryToSetColon(args: List[String]): Option[ResultOfTryToSet] = errorAndValue(s"bad argument for $name", None)
    override def respondsTo(token: String) = token startsWith prefix
    def unparse: List[String] = value
    override def reset() = v = Nil
  }

  /** A setting represented by a string, (`default` unless set) */
  class StringSetting private[nsc](
    name: String,
    val arg: String,
    descr: String,
    val default: String,
    helpText: Option[String])
  extends Setting(name, descr) {
    type T = String
    protected var v: T = default
    protected var sawHelp: Boolean = false

    withHelpSyntax(name + " <" + arg + ">")

    def tryToSet(args: List[String]) = args match {
      case Nil | Optionlike() :: _             => errorAndValue(s"missing argument for $name", None)
      case "help" :: rest if helpText.nonEmpty => sawHelp = true ; Some(rest)
      case h :: rest                           => value = h ; Some(rest)
    }
    def tryToSetColon(args: List[String]): Option[ResultOfTryToSet] =
      args match {
        case Nil | _ :: Nil => tryToSet(args)
        case _              => errorAndValue("too many arguments", None)
      }
    def unparse: List[String] = if (value == default) Nil else List(name, value)

    override def isHelping: Boolean = sawHelp

    override def help = helpText.get
    override def reset() = v = default
  }

  /** A setting represented by a Scala version.
    * The `initial` value is used if the setting is not specified.
    * The `default` value is used if the option is specified without argument (e.g., `-Xmigration`).
    */
  class ScalaVersionSetting private[nsc](
    name: String,
    val arg: String,
    descr: String,
    val initial: ScalaVersion,
    default: Option[ScalaVersion],
    helpText: Option[String])
  extends Setting(name, descr) {
    type T = ScalaVersion
    protected var v: T = initial
    protected var sawHelp: Boolean = false

    // This method is invoked if there are no colonated args. In this case the default value is
    // used. No arguments are consumed.
    def tryToSet(args: List[String]) = {
      default match {
        case Some(d) => value = d
        case None => errorFn(s"$name requires an argument, the syntax is $helpSyntax")
      }
      Some(args)
    }

    def tryToSetColon(args: List[String]) = args match {
      case "help" :: rest if helpText.nonEmpty => sawHelp = true; Some(rest)
      case x :: xs  => value = ScalaVersion(x, errorFn); Some(xs)
      case nil      => Some(nil)
    }

    def unparse: List[String] = if (value == NoScalaVersion) Nil else List(s"${name}:${value.unparse}")

    override def isHelping: Boolean = sawHelp

    override def help = helpText.get

    withHelpSyntax(s"${name}:<${arg}>")

    override def reset() = v = initial
  }

  class PathSetting private[nsc](
    name: String,
    descr: String,
    default: String,
    prependPath: StringSetting,
    appendPath: StringSetting)
  extends StringSetting(name, "path", descr, default, None) {
    import util.ClassPath.join
    def prepend(s: String) = prependPath.value = join(s, prependPath.value)
    def append(s: String) = appendPath.value = join(appendPath.value, s)

    override def isDefault = super.isDefault && prependPath.isDefault && appendPath.isDefault
    override def value = join(
      prependPath.value,
      super.value,
      appendPath.value
    )
    override def reset() = ()
  }

  /** Set the output directory for all sources. */
  class OutputSetting private[nsc](default: String) extends StringSetting("-d", "directory|jar", "Destination for generated artifacts.", default, None)

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
    case class Choice(name: String, help: String = "", expandsTo: List[Choice] = Nil, requiresSelections: Boolean = false) extends Val(name) {
      var selections: List[String] = Nil
    }
    def wildcardChoices: ValueSet = values.filter { case c: Choice => c.expandsTo.isEmpty case _ => true }
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
    val helpArg: String,
    descr: String,
    val domain: E,
    val default: Option[List[String]],
    val helpText: Option[String]
  ) extends Setting(name, descr) with Clearable {

    withHelpSyntax(s"$name:<${helpArg}s>")

    object ChoiceOrVal {
      def unapply(a: domain.Value): Some[(String, String, List[domain.Choice])] = a match {
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
    private def isChoice(s: String) = s == "_" || choices.contains(pos(s))
    private def choiceOf(s: String): domain.Choice = domain.withName(pos(s)).asInstanceOf[domain.Choice]

    private def pos(s: String) = s.stripPrefix("-")
    private def isPos(s: String) = !s.startsWith("-")

    override val choices: List[String] = domain.values.toList map {
      case ChoiceOrVal(name, _, _) => name
    }

    def descriptions: List[String] = domain.values.toList map {
      case ChoiceOrVal(_, "", x :: xs) => "Enables the options "+ (x :: xs).map(_.name).mkString(", ")
      case ChoiceOrVal(_, descr, _)    => descr
      case _                           => ""
    }

    /** (Re)compute from current yeas, nays, wildcard status. Assign option value. */
    def compute() = {
      def simple(v: domain.Value) = v match {
        case c: domain.Choice => c.expandsTo.isEmpty
        case _ => true
      }

      /*
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
      val weakYeas = if (sawAll) domain.wildcardChoices else expand(yeas.filterNot(simple))
      value = yeas.filter(simple) | (weakYeas &~ nays)
    }

    /** Add a named choice to the multichoice value. */
    def add(arg: String) = arg match {
      case _ if !isChoice(arg) =>
        badChoice(arg)
      case "_" =>
        sawAll = true
        compute()
      case _ if isPos(arg) =>
        yeas += domain.withName(arg)
        compute()
      case _ =>
        val choice = domain.withName(pos(arg))
        choice match {
          case ChoiceOrVal(_, _, _ :: _) => errorFn(s"'${pos(arg)}' cannot be negated, it enables other arguments")
          case _ =>
        }
        nays += choice
        compute()
    }

    // refine a choice with selections. -opt:inline:**
    def add(arg: String, selections: List[String]): Unit = {
      add(arg)
      domain.withName(arg).asInstanceOf[domain.Choice].selections ++= selections
    }

    def tryToSet(args: List[String])                  = tryToSetArgs(args, halting = true)
    def tryToSetColon(args: List[String])             = tryToSetArgs(args, halting = false)
    override def tryToSetFromPropertyValue(s: String) = tryToSet(s.trim.split(',').toList) // used from ide

    /** Try to set args, handling "help" and default.
     *  The "halting" parameter means args were "-option a b c -else" so halt
     *  on "-else" or other non-choice. Otherwise, args were "-option:a,b,c,d",
     *  so process all and report non-choices as errors.
     *
     *  If a choice is seen as colonated, then set the choice selections:
     *  "-option:choice:selection1,selection2"
     *
     *  @param args args to process
     *  @param halting stop on non-arg
     */
    private def tryToSetArgs(args: List[String], halting: Boolean) = {
      val colonnade = raw"([^:]+):(.*)".r
      var count = 0
      val rest = {
        @tailrec
        def loop(args: List[String]): List[String] = args match {
          case "help" :: rest =>
            sawHelp = true
            count += 1
            loop(rest)
          case arg :: rest =>
            val (argx, selections) = arg match {
              case colonnade(x, y) => (x, y)
              case _ => (arg, "")
            }
            if (halting && (!isPos(argx) || !isChoice(argx)))
              args
            else {
              if (isChoice(argx)) {
                if (selections.nonEmpty) add(argx, selections.split(",").toList)
                else if (argx != "_" && isPos(argx) && choiceOf(argx).requiresSelections) errorFn(s"'$argx' requires '$argx:<selection>'. See '$name:help'.")
                else add(argx)  // this case also adds "_"
                postSetHook()   // support -opt:l:method
              }
              else
                badChoice(argx)
              count += 1
              loop(rest)
            }
          case _ => Nil
        }
        loop(args)
      }

      // if no arg applied, use defaults or error; otherwise, add what they added
      if (count == 0)
        default match {
          case Some(defaults) => defaults.foreach(add)
          case None => errorFn(s"'$name' requires an option. See '$name:help'.")
        }
      Some(rest)
    }

    def contains(choice: domain.Value): Boolean = value.contains(choice)

    // programmatically.
    def enable(choice: domain.Value): Unit = { nays -= choice ; yeas += choice ; compute() }

    // programmatically. Disabling expanding option is otherwise disallowed.
    def disable(choice: domain.Value): Unit = { yeas -= choice ; nays += choice ; compute() }

    override def isHelping: Boolean = sawHelp

    override def help: String = {
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
      choices.zipAll(descriptions, "", "").map(describe).mkString(f"${helpText.getOrElse(descr)}%n", f"%n", orelse)
    }

    def clear(): Unit = {
      domain.values.foreach { case c: domain.Choice => c.selections = Nil ; case _ => }
      v = domain.ValueSet.empty
      yeas = domain.ValueSet.empty
      nays = domain.ValueSet.empty
      sawAll = false
      sawHelp = false
    }
    def unparse: List[String] = value.toList.map(s => s"$name:$s")
    def contains(s: String)   = domain.values.find(_.toString == s).exists(value.contains)
    override def reset() = clear()
  }

  /** A setting that accumulates all strings supplied to it,
   *  until it encounters one starting with a '-'.
   */
  class MultiStringSetting private[nsc](
    name: String,
    val arg: String,
    descr: String,
    default: List[String],
    helpText: Option[String],
    prepend: Boolean)
  extends Setting(name, descr) with Clearable {
    type T = List[String]
    protected var v: T = default
    protected var sawHelp: Boolean = false

    withHelpSyntax(name + ":<" + arg + ">")

    // try to set. halting means halt at first non-arg i.e. at next option
    protected def tryToSetArgs(args: List[String], halting: Boolean) = {
      @tailrec
      def loop(seen: List[String], args: List[String]): (List[String], List[String]) = args match {
        case Optionlike() :: _ if halting         => (seen, args)
        case "help" :: args if helpText.isDefined => sawHelp = true; loop(seen, args)
        case head :: args                         => loop(head :: seen, args)
        case Nil                                  => (seen, Nil)
      }
      val (seen, rest) = loop(Nil, args)
      if (prepend) value = value.prependedAll(seen)
      else value = value.appendedAll(seen.reverse)
      Some(rest)
    }
    def tryToSet(args: List[String])                  = tryToSetArgs(args, halting = true)
    def tryToSetColon(args: List[String])             = tryToSetArgs(args, halting = false)
    override def tryToSetFromPropertyValue(s: String) = tryToSet(s.trim.split(',').toList) // used from ide

    def clear(): Unit         = (v = Nil)
    def unparse: List[String] = value map (name + ":" + _)
    def contains(s: String)   = value contains s

    override def isHelping: Boolean = sawHelp

    override def help = helpText.get

    override def reset() = {
      v = default
      setByUser = false
    }
  }

  /** A setting represented by a string in a given set of `choices`,
   *  (`default` unless set).
   */
  class ChoiceSetting private[nsc](
    name: String,
    val helpArg: String,
    descr: String,
    override val choices: List[String],
    val default: String,
    val choicesHelp: List[String])
  extends Setting(name,
      if (choicesHelp.isEmpty) s"$descr ${choices.map(s => if (s == default) s"[$s]" else s).mkString("(", ",", ")")}"
      else s"$descr Default: `$default`, `help` to list choices."
  ) {
    type T = String
    protected var v: T = default
    def indexOfChoice: Int = choices indexOf value

    private[this] var _preSetHook: String => String = s => s
    def withPreSetHook(hook: String => String): this.type = { _preSetHook = hook ; this }

    private def choicesHelpMessage = if (choicesHelp.isEmpty) "" else {
      val choiceLength = choices.map(_.length).max + 1
      val formatStr = s"  %-${choiceLength}s %s%n"
      choices.zipAll(choicesHelp, "", "").map({
        case (choice, desc) => formatStr.format(choice, desc)
      }).mkString("")
    }
    private def usageErrorMessage = f"Usage: $name:<$helpArg> where <$helpArg> choices are ${choices mkString ", "} (default: $default).%n$choicesHelpMessage"

    private var sawHelp = false
    override def isHelping = sawHelp
    override def help = usageErrorMessage

    def tryToSet(args: List[String]) =
      args match {
        case Nil | Optionlike() :: _ => errorAndValue(usageErrorMessage, None)
        case arg :: rest             => tryToSetColon(List(arg)).map(_ => rest)
      }

    def tryToSetColon(args: List[String]) = args map _preSetHook match {
      case Nil                            => errorAndValue(usageErrorMessage, None)
      case List("help")                   => sawHelp = true; SomeOfNil
      case List(x) if choices contains x  => value = x ; SomeOfNil
      case List(x)                        => errorAndValue("'" + x + "' is not a valid choice for '" + name + "'", None)
      case _                              => errorAndValue("'" + name + "' does not accept multiple arguments.", None)
    }
    def unparse: List[String] =
      if (value == default) Nil else List(name + ":" + value)
    override def tryToSetFromPropertyValue(s: String) = tryToSetColon(s::Nil) // used from ide

    withHelpSyntax(name + ":<" + helpArg + ">")

    override def reset() = v = default
  }

  private def mkPhasesHelp(descr: String, default: String) = {
    descr + " <phases>" + (
      if (default == "") "" else " (default: " + default + ")"
    )
  }

  /** A setting represented by a list of strings which should be prefixes of
   *  phase names. This is not checked here, however.  Alternatively, underscore
   *  can be used to indicate all phases.
   */
  class PhasesSetting private[nsc](
    name: String,
    descr: String,
    val default: String
  ) extends Setting(name, mkPhasesHelp(descr, default)) with Clearable {
    private[nsc] def this(name: String, descr: String) = this(name, descr, "")

    type T = List[String]
    private[this] var _v: T = Nil
    private[this] var _numbs: List[(Int,Int)] = Nil
    private[this] var _names: T = Nil
    protected def v: T = _v
    protected def v_=(t: T): Unit = {
      // throws NumberFormat on bad range (like -5-6)
      def asRange(s: String): (Int,Int) = s.indexOf('-') match {
        case -1 => (s.toInt, s.toInt)
        case 0  => (-1, s.tail.toInt)
        case _ if s.last == '-' => (s.init.toInt, Int.MaxValue)
        case i  => (s.take(i).toInt, s.drop(i+1).toInt)
      }
      val numsAndStrs = t filter (_.nonEmpty) partition (_ forall (ch => ch.isDigit || ch == '-'))
      _numbs = numsAndStrs._1 map asRange
      _names = numsAndStrs._2
      _v     = t
    }
    override def value = if (v contains "_") List("_") else super.value // i.e., v
    private def numericValues = _numbs
    private def stringValues  = _names
    private def phaseIdTest(i: Int): Boolean = numericValues exists (_ match {
      case (min, max) => min <= i && i <= max
    })

    def tryToSet(args: List[String]) =
      if (default == "") errorAndValue("missing phase", None)
      else tryToSetColon(splitDefault) map (_ => args)

    private def splitDefault = default.split(',').toList

    def tryToSetColon(args: List[String]) = try {
      args match {
        case Nil  => if (default == "") errorAndValue("missing phase", None)
                     else tryToSetColon(splitDefault)
        case xs   => value = (value ++ xs).distinct.sorted ; SomeOfNil
      }
    } catch { case _: NumberFormatException => None }

    def clear(): Unit = (v = Nil)

    /* True if the named phase is selected.
     *
     * A setting value "_" or "all" selects all phases by name.
     */
    def contains(phName: String)     = doAllPhases || containsName(phName)
    /* True if the given phase name matches the selection, possibly as prefixed "~name". */
    def containsName(phName: String) = stringValues.exists(phName.startsWith(_))
    def containsId(phaseId: Int)     = phaseIdTest(phaseId)
    /* True if the phase is selected by name or "all", or by id, or by prefixed "~name". */
    def containsPhase(ph: Phase)     = contains(ph.name) || containsId(ph.id) || containsName(s"~${ph.name}") ||
      ph.next != null && containsName(s"~${ph.next.name}")  // null if called during construction

    def doAllPhases = stringValues.exists(s => s == "_" || s == "all")
    def unparse: List[String] = value.map(v => s"$name:$v")

    withHelpSyntax(
      if (default == "") name + ":<phases>"
      else name + "[:phases]"
    )

    override def reset() = clear()
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

private object Optionlike {
  def unapply(s: String): Boolean = s.startsWith("-") && s != "-"
}
