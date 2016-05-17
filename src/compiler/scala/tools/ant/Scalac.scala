/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant

import java.io.{File, PrintWriter, BufferedWriter, FileWriter}

import org.apache.tools.ant.{ BuildException, Project, AntClassLoader }
import org.apache.tools.ant.taskdefs.Java
import org.apache.tools.ant.types.{Path, Reference}
import org.apache.tools.ant.util.{FileUtils, GlobPatternMapper,
                                  SourceFileScanner, facade}
import org.apache.tools.ant.util.facade.{FacadeTaskHelper,
                                  ImplementationSpecificArgument}

import scala.tools.nsc.{Global, Settings, CompilerCommand}
import scala.tools.nsc.io.{Path => SPath}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}

/** An Ant task to compile with the new Scala compiler (NSC).
 *
 *  This task can take the following parameters as attributes:
 *  - `srcdir` (mandatory),
 *  - `srcref`,
 *  - `destdir`,
 *  - `classpath`,
 *  - `classpathref`,
 *  - `sourcepath`,
 *  - `sourcepathref`,
 *  - `bootclasspath`,
 *  - `bootclasspathref`,
 *  - `extdirs`,
 *  - `extdirsref`,
 *  - `argfile`,
 *  - `dependencyfile`,
 *  - `encoding`,
 *  - `target`,
 *  - `force`,
 *  - `fork`,
 *  - `logging`,
 *  - `logphase`,
 *  - `debuginfo`,
 *  - `addparams`,
 *  - `explaintypes`,
 *  - `deprecation`,
 *  - `nobootcp`,
 *  - `nowarn`,
 *  - `optimise`,
 *  - `unchecked`,
 *  - `usejavacp`,
 *  - `failonerror`,
 *  - `scalacdebugging`,
 *
 *  It also takes the following parameters as nested elements:
 *  - `src` (for `srcdir`),
 *  - `classpath`,
 *  - `sourcepath`,
 *  - `bootclasspath`,
 *  - `extdirs`,
 *  - `compilerarg`.
 *
 *  @author Gilles Dubochet, Stephane Micheloud
 */
class Scalac extends ScalaMatchingTask with ScalacShared {

  /** The unique Ant file utilities instance to use in this task. */
  private val fileUtils = FileUtils.getFileUtils()

/*============================================================================*\
**                             Ant user-properties                            **
\*============================================================================*/

  abstract class PermissibleValue {
    val values: List[String]
    def isPermissible(value: String): Boolean =
      (value == "") || values.exists(_.startsWith(value))
  }

  /** Defines valid values for the logging property. */
  object LoggingLevel extends PermissibleValue {
    val values = List("none", "verbose", "debug")
  }

  /** Defines valid values for properties that refer to compiler phases. */
  object CompilerPhase extends PermissibleValue {
    val values = List("namer", "typer", "pickler", "refchecks",
                      "uncurry", "tailcalls", "specialize", "explicitouter",
                      "erasure", "lazyvals", "lambdalift", "constructors",
                      "flatten", "mixin", "delambdafy", "cleanup", "icode", "inliner",
                      "closelim", "dce", "jvm", "terminal")
  }

  /** Defines valid values for the `target` property. */
  object Target extends PermissibleValue {
    val values = List("jvm-1.5", "jvm-1.6", "jvm-1.7", "jvm-1.8")
  }

  /** Defines valid values for the `deprecation` and `unchecked` properties. */
  object Flag extends PermissibleValue {
    val values = List("yes", "no", "on", "off", "true", "false")
    def toBoolean(flag: String) =
      if (flag == "yes" || flag == "on" || flag == "true") Some(true)
      else if (flag == "no" || flag == "off" || flag == "false") Some(false)
      else None
  }

  /** The directories that contain source files to compile. */
  protected var origin: Option[Path] = None
  /** The directory to put the compiled files in. */
  protected var destination: Option[File] = None

  /** The class path to use for this compilation. */
  protected var classpath: Option[Path] = None
  /** The source path to use for this compilation. */
  protected var sourcepath: Option[Path] = None
  /** The boot class path to use for this compilation. */
  protected var bootclasspath: Option[Path] = None
  /** The path to use when finding scalac - *only used for forking!*  */
  protected var compilerPath: Option[Path] = None
  /** The external extensions path to use for this compilation. */
  protected var extdirs: Option[Path] = None

  protected var argfile: Option[File] = None
  /** The dependency tracking file. */
  protected var dependencyfile: Option[File] = None
  /** The character encoding of the files to compile. */
  protected var encoding: Option[String] = None

  // the targeted backend
  protected var backend: Option[String] = None

  /** Whether to force compilation of all files or not. */
  protected var force: Boolean = false
  /** Whether to fork the execution of scalac */
  protected var fork : Boolean = false
  /** If forking, these are the arguments to the JVM */
  protected var jvmArgs : Option[String] = None
  /** How much logging output to print. Either none (default),
    * verbose or debug. */
  protected var logging: Option[String] = None
  /** Which compilation phases should be logged during compilation. */
  protected var logPhase: List[String] = Nil

  /** Instruct the compiler to generate debugging information */
  protected var debugInfo: Option[String] = None
  /** Instruct the compiler to use additional parameters */
  protected var addParams: String = ""
  /** Instruct the compiler to explain type errors in more detail. */
  protected var explaintypes: Option[Boolean] = None
  /** Instruct the compiler to generate deprecation information. */
  protected var deprecation: Option[Boolean] = None
  /** Instruct the compiler to not use the boot classpath for the scala jars. */
  protected var nobootcp: Option[Boolean] = None
  /** Instruct the compiler to generate no warnings. */
  protected var nowarn: Option[Boolean] = None
  /** Instruct the compiler to run optimizations. */
  protected var optimise: Option[Boolean] = None
  /** Instruct the compiler to generate unchecked information. */
  protected var unchecked: Option[Boolean] = None
  /** Instruct the compiler to use `java.class.path` in classpath resolution. */
  protected var usejavacp: Option[Boolean] = None
  /** Indicates whether compilation errors will fail the build; defaults to true. */
  protected var failonerror: Boolean = true

  /** Prints out the files being compiled by the scalac ant task
   *  (not only the number of files). */
  protected var scalacDebugging: Boolean = false

  /** Encapsulates implementation of specific command line arguments. */
  protected var scalacCompilerArgs = new FacadeTaskHelper("compilerarg")

  /** Helpers */
  private def setOrAppend(old: Option[Path], arg: Path): Option[Path] = old match {
    case Some(x)  => x append arg ; Some(x)
    case None     => Some(arg)
  }
  private def pathAsList(p: Option[Path], name: String): List[File] = p match {
    case None     => buildError("Member '" + name + "' is empty.")
    case Some(x)  => x.list.toList map nameToFile
  }
  private def createNewPath(getter: () => Option[Path], setter: (Option[Path]) => Unit) = {
    if (getter().isEmpty)
      setter(Some(new Path(getProject)))

    getter().get.createPath()
  }

  private def plural(xs: List[Any]) = if (xs.size > 1) "s" else ""
  private def plural(x: Int) = if (x > 1) "s" else ""

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/


  /** Sets the `srcdir` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `origin`. */
  def setSrcdir(input: Path) {
    origin = setOrAppend(origin, input)
  }

  /** Sets the `origin` as a nested src Ant parameter.
   *  @return An origin path to be configured. */
  def createSrc(): Path = createNewPath(origin _, p => origin = p)

  /** Sets the `origin` as an external reference Ant parameter.
   *  @param input A reference to an origin path. */
  def setSrcref(input: Reference) =
    createSrc().setRefid(input)

  /** Sets the `destdir` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `destination`. */
  def setDestdir(input: File) { destination = Some(input) }

  /** Sets the `classpath` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `classpath`. */
  def setClasspath(input: Path) {
    classpath = setOrAppend(classpath, input)
  }
  /** Sets the `compilerPath` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `compilerPath`. */
  def setCompilerPath(input: Path) {
    compilerPath = setOrAppend(compilerPath, input)
  }

  def createCompilerPath: Path = createNewPath(compilerPath _, p => compilerPath = p)

  /** Sets the `compilerpathref` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `compilerpathref`. */
  def setCompilerPathRef(input: Reference) {
    createCompilerPath.setRefid(input)
  }

  /** Sets the `classpath` as a nested classpath Ant parameter.
   *  @return A class path to be configured. */
  def createClasspath(): Path = createNewPath(classpath _, p => classpath = p)

  /** Sets the `classpath` as an external reference Ant parameter.
   *  @param input A reference to a class path. */
  def setClasspathref(input: Reference) {
    createClasspath().setRefid(input)
  }

  /** Sets the `sourcepath` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `sourcepath`. */
  def setSourcepath(input: Path) {
    sourcepath = setOrAppend(sourcepath, input)
  }

  /** Sets the `sourcepath` as a nested sourcepath Ant parameter.
   *  @return A source path to be configured. */
  def createSourcepath(): Path = createNewPath(sourcepath _, p => sourcepath = p)

  /** Sets the `sourcepath` as an external reference Ant parameter.
   *  @param input A reference to a source path. */
  def setSourcepathref(input: Reference) {
    createSourcepath().setRefid(input)
  }

  /** Sets the boot classpath attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value of `bootclasspath`. */
  def setBootclasspath(input: Path) {
    bootclasspath = setOrAppend(bootclasspath, input)
  }

  /** Sets the `bootclasspath` as a nested bootclasspath Ant parameter.
   *  @return A source path to be configured. */
  def createBootclasspath(): Path = createNewPath(bootclasspath _, p => bootclasspath = p)

  /** Sets the `bootclasspath` as an external reference Ant
   *  parameter.
   *  @param input A reference to a source path. */
  def setBootclasspathref(input: Reference) =
    createBootclasspath().setRefid(input)

  /** Sets the external extensions path attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `extdirs`. */
  def setExtdirs(input: Path) {
    extdirs = setOrAppend(extdirs, input)
  }

  /** Sets the `extdirs` as a nested extdirs Ant parameter.
   *  @return An extensions path to be configured. */
  def createExtdirs(): Path = createNewPath(extdirs _, p => extdirs = p)

  /** Sets the `extdirs` as an external reference Ant parameter.
   *  @param input A reference to an extensions path. */
  def setExtdirsref(input: Reference) =
    createExtdirs().setRefid(input)

  /** Sets the `argfile` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `argfile`. */
  def setArgfile(input: File) {
    argfile = Some(input)
  }

  /** Sets the `dependencyfile` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `dependencyfile`. */
  def setDependencyfile(input: File) {
    dependencyfile = Some(input)
  }

  /** Sets the `encoding` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `encoding`. */
  def setEncoding(input: String) {
    encoding = Some(input)
  }

  /** Sets the `target` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value for `target`. */
  def setTarget(input: String): Unit =
    if (Target.isPermissible(input)) backend = Some(input)
    else buildError("Unknown target '" + input + "'")

  /** Sets the `force` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value for `force`. */
  def setForce(input: Boolean) { force = input }

  /** Sets the `fork` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value for `fork`. */
  def setFork(input : Boolean) { fork = input }
  /**
   * Sets the `jvmargs` attribute.  Used by [[http://ant.apache.org Ant]].
   * @param input The value for `jvmargs`
   */
  def setJvmargs(input : String) {
    jvmArgs = Some(input)
  }

  /** Sets the logging level attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value for `logging`. */
  def setLogging(input: String) {
    if (LoggingLevel.isPermissible(input)) logging = Some(input)
    else buildError("Logging level '" + input + "' does not exist.")
  }

  /** Sets the `logphase` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value for `logPhase`. */
  def setLogPhase(input: String) {
    logPhase = input.split(",").toList.flatMap { s: String =>
      val st = s.trim()
      if (CompilerPhase.isPermissible(st))
        (if (input != "") List(st) else Nil)
      else {
        buildError("Phase " + st + " in log does not exist.")
      }
    }
  }

  /** Set the `debug` info attribute.
   *  @param input The value for `debug`. */
  def setDebuginfo(input: String) { debugInfo = Some(input) }

  /** Set the `addparams` info attribute.
   *  @param input The value for `addparams`. */
  def setAddparams(input: String) { addParams = input }

  /** Set the `explaintypes` info attribute.
   *  @param input One of the flags `yes/no` or `on/off`. */
  def setExplaintypes(input: String) {
    explaintypes = Flag toBoolean input orElse buildError("Unknown explaintypes flag '" + input + "'")
  }

  /** Set the `deprecation` info attribute.
   *  @param input One of the flags `yes/no` or `on/off`. */
  def setDeprecation(input: String) {
    deprecation = Flag toBoolean input orElse buildError("Unknown deprecation flag '" + input + "'")
  }

  /** Set the `nobootcp` info attribute.
   *  @param input One of the flags `yes/no` or `on/off`. */
  def setNobootcp(input: String) {
    nobootcp = Flag toBoolean input orElse buildError("Unknown nobootcp flag '" + input + "'")
  }

  /** Set the `nowarn` info attribute.
   *  @param input One of the flags `yes/no` or `on/off`. */
  def setNowarn(input: String) {
    nowarn = Flag toBoolean input orElse buildError("Unknown nowarn flag '" + input + "'")
  }

  /** Set the `optimise` info attribute.
   *  @param input One of the flags `yes/no` or `on/off`. */
  def setOptimise(input: String) {
    optimise = Flag toBoolean input orElse buildError("Unknown optimisation flag '" + input + "'")
  }

  /** Set the `unchecked` info attribute.
   *  @param input One of the flags `yes/no` or `on/off`. */
  def setUnchecked(input: String) {
    unchecked = Flag toBoolean input orElse buildError("Unknown unchecked flag '" + input + "'")
  }

  /** Set the `usejavacp` info attribute.
   *  @param input One of the flags `yes/no` or `on/off`. */
  def setUsejavacp(input: String) {
    usejavacp = Flag toBoolean input orElse buildError("Unknown usejavacp flag '" + input + "'")
  }

  /** Sets the `failonerror` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value for `failonerror`. */
  def setFailonerror(input: Boolean) { failonerror = input }

  /** Set the `scalacdebugging` info attribute. If set to
   *  `'''true'''`, the scalac ant task will print out the filenames
   *  being compiled.
   *  @param input The specified flag */
  def setScalacdebugging(input: Boolean) { scalacDebugging = input }

  /** Sets the `compilerarg` as a nested compilerarg Ant parameter.
   *  @return A compiler argument to be configured. */
  def createCompilerArg(): ImplementationSpecificArgument = {
    val arg = new ImplementationSpecificArgument()
    scalacCompilerArgs addImplementationArgument arg
    arg
  }

/*============================================================================*\
**                             Properties getters                             **
\*============================================================================*/

  /** Gets the value of the `classpath` attribute in a
   *  Scala-friendly form.
   *  @return The class path as a list of files. */
  protected def getClasspath: List[File] = pathAsList(classpath, "classpath")

  /** Gets the value of the `origin` attribute in a
   *  Scala-friendly form.
   *  @return The origin path as a list of files. */
  protected def getOrigin: List[File] = pathAsList(origin, "origin")

  /** Gets the value of the `destination` attribute in a
   *  Scala-friendly form.
   *  @return The destination as a file. */
  protected def getDestination: File =
    if (destination.isEmpty) buildError("Member 'destination' is empty.")
    else existing(getProject resolveFile destination.get.toString)

  /** Gets the value of the `sourcepath` attribute in a
   *  Scala-friendly form.
   *  @return The source path as a list of files. */
  protected def getSourcepath: List[File] = pathAsList(sourcepath, "sourcepath")

  /** Gets the value of the `bootclasspath` attribute in a
   *  Scala-friendly form.
   *  @return The boot class path as a list of files. */
  protected def getBootclasspath: List[File] = pathAsList(bootclasspath, "bootclasspath")

  /** Gets the value of the `extdirs` attribute in a
   *  Scala-friendly form.
   *  @return The extensions path as a list of files. */
  protected def getExtdirs: List[File] = pathAsList(extdirs, "extdirs")

/*============================================================================*\
**                       Compilation and support methods                      **
\*============================================================================*/

  /** Transforms a string name into a file relative to the provided base
   *  directory.
   *  @param base A file pointing to the location relative to which the name
   *              will be resolved.
   *  @param name A relative or absolute path to the file as a string.
   *  @return     A file created from the name and the base file. */
  protected def nameToFile(base: File)(name: String): File =
    existing(fileUtils.resolveFile(base, name))

  /** Transforms a string name into a file relative to the build root
   *  directory.
   *  @param name A relative or absolute path to the file as a string.
   *  @return     A file created from the name. */
  protected def nameToFile(name: String): File =
    existing(getProject resolveFile name)

  /** Tests if a file exists and prints a warning in case it doesn't. Always
   *  returns the file, even if it doesn't exist.
   *  @param file A file to test for existence.
   *  @return     The same file. */
  protected def existing(file: File): File = {
    if (!file.exists)
      log("Element '" + file.toString + "' does not exist.",
          Project.MSG_WARN)
    file
  }

  /** Transforms a path into a Scalac-readable string.
   *  @param path A path to convert.
   *  @return     A string-representation of the path like `a.jar:b.jar`. */
  protected def asString(path: List[File]): String =
    path.map(asString) mkString File.pathSeparator

  /** Transforms a file into a Scalac-readable string.
   *  @param file A file to convert.
   *  @return     A string-representation of the file like `/x/k/a.scala`. */
  protected def asString(file: File): String =
    file.getAbsolutePath()

/*============================================================================*\
**                      Hooks for variants of Scala                           **
\*============================================================================*/

  protected def newSettings(error: String=>Unit): Settings =
    new Settings(error)

  protected def newGlobal(settings: Settings, reporter: Reporter) =
    Global(settings, reporter)

/*============================================================================*\
**                           The big execute method                           **
\*============================================================================*/

  /** Initializes settings and source files */
  protected def initialize: (Settings, List[File], Boolean) = {
    if (scalacDebugging)
      log("Base directory is `%s`".format(SPath("").normalize))

    // Tests if all mandatory attributes are set and valid.
    if (origin.isEmpty) buildError("Attribute 'srcdir' is not set.")
    if (!destination.isEmpty && !destination.get.isDirectory())
      buildError("Attribute 'destdir' does not refer to an existing directory.")
    if (destination.isEmpty) destination = Some(getOrigin.head)

    val mapper = new GlobPatternMapper()
    mapper setTo "*.class"
    mapper setFrom "*.scala"

    var javaOnly = true

    def getOriginFiles(originDir: File) = {
      val includedFiles = getDirectoryScanner(originDir).getIncludedFiles
      val javaFiles = includedFiles filter (_ endsWith ".java")
      val scalaFiles = {
        val xs = includedFiles filter (_ endsWith ".scala")
        if (force) xs
        else new SourceFileScanner(this).restrict(xs, originDir, destination.get, mapper)
      }

      javaOnly = javaOnly && (scalaFiles.length == 0)
      val list = (scalaFiles ++ javaFiles).toList

      if (scalacDebugging && !list.isEmpty)
        log("Compiling source file%s: %s to %s".format(
          plural(list),
          list.mkString(", "),
          getDestination.toString
        ))
      else if (!list.isEmpty) {
        val str =
          if (javaFiles.isEmpty) "%d source file%s".format(list.length, plural(list))
          else "%d scala and %d java source files".format(scalaFiles.length, javaFiles.length)
        log("Compiling %s to %s".format(str, getDestination.toString))
      }
      else log("No files selected for compilation", Project.MSG_VERBOSE)

      list
    }

    // Scans source directories to build up a compile lists.
    // If force is false, only files were the .class file in destination is
    // older than the .scala file will be used.
    val sourceFiles: List[File] =
      for (originDir <- getOrigin ; originFile <- getOriginFiles(originDir)) yield {
        log(originFile, Project.MSG_DEBUG)
        nameToFile(originDir)(originFile)
      }

    // Builds-up the compilation settings for Scalac with the existing Ant
    // parameters.
    val settings = newSettings(buildError)
    settings.outdir.value = asString(destination.get)
    if (!classpath.isEmpty)
      settings.classpath.value = asString(getClasspath)
    if (!sourcepath.isEmpty)
      settings.sourcepath.value = asString(getSourcepath)
    if (!bootclasspath.isEmpty)
      settings.bootclasspath.value = asString(getBootclasspath)
    if (!extdirs.isEmpty) settings.extdirs.value = asString(getExtdirs)
    if (!dependencyfile.isEmpty)
      settings.dependencyfile.value = asString(dependencyfile.get)
    if (!encoding.isEmpty) settings.encoding.value = encoding.get
    if (!backend.isEmpty) settings.target.value = backend.get
    if (!logging.isEmpty && logging.get == "verbose")
      settings.verbose.value = true
    else if (!logging.isEmpty && logging.get == "debug") {
      settings.verbose.value = true
      settings.debug.value = true
    }
    if (!logPhase.isEmpty) settings.log.value = logPhase
    if (!debugInfo.isEmpty) settings.debuginfo.value = debugInfo.get
    if (!explaintypes.isEmpty) settings.explaintypes.value = explaintypes.get
    if (!deprecation.isEmpty) settings.deprecation.value = deprecation.get
    if (!nobootcp.isEmpty) settings.nobootcp.value = nobootcp.get
    if (!nowarn.isEmpty) settings.nowarn.value = nowarn.get
    if (!optimise.isEmpty) settings.optimise.value = optimise.get
    if (!unchecked.isEmpty) settings.unchecked.value = unchecked.get
    if (!usejavacp.isEmpty) settings.usejavacp.value = usejavacp.get

    val jvmargs = scalacCompilerArgs.getArgs filter (_ startsWith "-J")
    if (!jvmargs.isEmpty) settings.jvmargs.value = jvmargs.toList
    val defines = scalacCompilerArgs.getArgs filter (_ startsWith "-D")
    if (!defines.isEmpty) settings.defines.value = defines.toList

    log("Scalac params = '" + addParams + "'", Project.MSG_DEBUG)

    // let CompilerCommand processes all params
    val command = new CompilerCommand(settings.splitParams(addParams), settings)

    // resolve dependenciesFile path from project's basedir, so <ant antfile ...> call from other project works.
    // the dependenciesFile may be relative path to basedir or absolute path, in either case, the following code
    // will return correct answer.
    command.settings.dependenciesFile.value match {
      case "none" =>
      case x =>
        val depFilePath = SPath(x)
        command.settings.dependenciesFile.value = SPath(getProject.getBaseDir).normalize.resolve(depFilePath).path
    }

    (command.settings, sourceFiles, javaOnly)
  }

  override def execute() {
    val (settings, sourceFiles, javaOnly) = initialize
    if (sourceFiles.isEmpty || javaOnly)
      return

    if (fork) executeFork(settings, sourceFiles)  // TODO - Error
    else executeInternal(settings, sourceFiles)
  }

  protected def executeFork(settings: Settings, sourceFiles: List[File]) {
      val java = new Java(this)
      java setFork true
      // using 'setLine' creates multiple arguments out of a space-separated string
      jvmArgs foreach { java.createJvmarg() setLine _ }

      // use user-provided path or retrieve from classloader
      // TODO - Allow user to override the compiler classpath
      val scalacPath: Path = {
        val path = new Path(getProject)
        if (compilerPath.isDefined) path add compilerPath.get
        else getClass.getClassLoader match {
          case cl: AntClassLoader => path add new Path(getProject, cl.getClasspath)
          case _                  => buildError("Cannot determine default classpath for scalac, please specify one!")
        }
        path
      }

      java setClasspath scalacPath
      java setClassname MainClass

      // Write all settings to a temporary file
      def writeSettings(): File = {
        def escapeArgument(arg : String) = if (arg matches ".*\\s.*") '"' + arg + '"' else arg
        val file = File.createTempFile("scalac-ant-",".args")
        file.deleteOnExit()
        val out = new PrintWriter(new BufferedWriter(new FileWriter(file)))

        try {
          for (setting <- settings.visibleSettings ; arg <- setting.unparse)
            out println escapeArgument(arg)
          for (file <- sourceFiles)
            out println escapeArgument(file.getAbsolutePath)
        }
        finally out.close()

        file
      }
      val res = execWithArgFiles(java, List(writeSettings().getAbsolutePath))
      if (failonerror && res != 0)
        buildError("Compilation failed because of an internal compiler error;"+
              " see the error output for details.")
  }

  /** Performs the compilation. */
  protected def executeInternal(settings: Settings, sourceFiles : List[File]) {
    val reporter = new ConsoleReporter(settings)
    val compiler = newGlobal(settings, reporter)  // compiles the actual code

    try new compiler.Run compile (sourceFiles map (_.toString))
    catch {
      case ex: Throwable =>
        ex.printStackTrace()
        val msg = if (ex.getMessage == null) "no error message provided" else ex.getMessage
        buildError("Compile failed because of an internal compiler error (" + msg + "); see the error output for details.")
    }

    reporter.printSummary()
    if (reporter.hasErrors) {
      val msg = "Compile failed with %d error%s; see the compiler error output for details.".format(
        reporter.ERROR.count, plural(reporter.ERROR.count))
      if (failonerror) buildError(msg) else log(msg)
    }
    else if (reporter.WARNING.count > 0)
      log("Compile succeeded with %d warning%s; see the compiler output for details.".format(
        reporter.WARNING.count, plural(reporter.WARNING.count)))
  }
}
