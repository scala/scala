/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.ant

import java.io.File

import org.apache.tools.ant.{BuildException, Project}
import org.apache.tools.ant.taskdefs.{MatchingTask,Java}
import org.apache.tools.ant.types.{Path, Reference, FileSet}
import org.apache.tools.ant.util.{FileUtils, GlobPatternMapper,
                                  SourceFileScanner}

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}

/** <p>
 *    An Ant task to compile with the new Scala compiler (NSC).
 *  </p>
 *  <p>
 *    This task can take the following parameters as attributes:
 *  </p>
 *  <ul style="font-family:Courier;">
 *    <li>srcdir (mandatory),</li>
 *    <li>srcref,</li>
 *    <li>destdir,</li>
 *    <li>classpath,</li>
 *    <li>classpathref,</li>
 *    <li>sourcepath,</li>
 *    <li>sourcepathref,</li>
 *    <li>bootclasspath,</li>
 *    <li>bootclasspathref,</li>
 *    <li>extdirs,</li>
 *    <li>extdirsref,</li>
 *    <li>encoding,</li>
 *    <li>target,</li>
 *    <li>force,</li>
 *    <li>fork,</li>
 *    <li>logging,</li>
 *    <li>logphase,</li>
 *    <li>debuginfo,</li>
 *    <li>addparams,</li>
 *    <li>scalacdebugging,</li>
 *    <li>deprecation,</li>
 *    <li>optimise,</li>
 *    <li>unchecked,</li>
 *    <li>failonerror,</li>
 *    <li>scalacdebugging,</li>
 *    <li>assemname,</li>
 *    <li>assemrefs.</li>
 *  </ul>
 *  <p>
 *    It also takes the following parameters as nested elements:
 *  </p>
 *  <ul>
 *    <li>src (for srcdir),</li>
 *    <li>classpath,</li>
 *    <li>sourcepath,</li>
 *    <li>bootclasspath,</li>
 *    <li>extdirs.</li>
 * </ul>
 *
 * @author Gilles Dubochet, Stephane Micheloud
 */
class Scalac extends MatchingTask {

  /** The unique Ant file utilities instance to use in this task. */
  private val fileUtils = FileUtils.newFileUtils()

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
    val values = List("namer", "typer", "pickler", "uncurry", "tailcalls",
                      "transmatch", "explicitouter", "erasure", "lambdalift",
                      "flatten", "constructors", "mixin", "icode", "jvm",
                      "terminal")
  }

  /** Defines valid values for the <code>target</code> property. */
  object Target extends PermissibleValue {
    val values = List("jvm-1.5", "msil", "cldc")
  }

  /** Defines valid values for the <code>deprecation</code> and
   *  <code>unchecked</code> properties. */
  object Flag extends PermissibleValue {
    val values = List("yes", "no", "on", "off")
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

  /** The character encoding of the files to compile. */
  protected var encoding: Option[String] = None

  // the targetted backend
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
  /** Instruct the compiler to generate deprecation information. */
  protected var deprecation: Option[Boolean] = None
  /** Instruct the compiler to run optimizations. */
  protected var optimise: Option[Boolean] = None
  /** Instruct the compiler to generate unchecked information. */
  protected var unchecked: Option[Boolean] = None
  /** Indicates whether compilation errors will fail the build; defaults to true. */
  protected var failonerror: Boolean = true

  // Name of the output assembly (only relevant with -target:msil)
  protected var assemname: Option[String] = None
  // List of assemblies referenced by the program (only relevant with -target:msil)
  protected var assemrefs: Option[String] = None

  /** Prints out the files being compiled by the scalac ant task
   *  (not only the number of files). */
  protected var scalacDebugging: Boolean = false

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  /** Sets the srcdir attribute. Used by Ant.
   *  @param input The value of <code>origin</code>. */
  def setSrcdir(input: Path) {
    if (origin.isEmpty) origin = Some(input)
    else origin.get.append(input)
  }

  /** Sets the <code>origin</code> as a nested src Ant parameter.
   *  @return An origin path to be configured. */
  def createSrc(): Path = {
    if (origin.isEmpty) origin = Some(new Path(getProject()))
    origin.get.createPath()
  }

  /** Sets the <code>origin</code> as an external reference Ant parameter.
   *  @param input A reference to an origin path. */
  def setSrcref(input: Reference) =
    createSrc().setRefid(input)

  /** Sets the <code>destdir</code> attribute. Used by Ant.
   *  @param input The value of <code>destination</code>. */
  def setDestdir(input: File) { destination = Some(input) }

  /** Sets the <code>classpath</code> attribute. Used by Ant.
   *  @param input The value of <code>classpath</code>. */
  def setClasspath(input: Path) {
    if (classpath.isEmpty) classpath = Some(input)
    else classpath.get.append(input)
  }
  /** Sets the <code>compilerPath</code> attribute. Used by Ant.
   *  @param input The value of <code>compilerPath</code>. */
  def setCompilerPath(input : Path) {
    if(compilerPath.isEmpty) compilerPath = Some(input)
    else compilerPath.get.append(input)
  }

  def createCompilerPath: Path = {
    if (compilerPath.isEmpty) compilerPath = Some(new Path(getProject()))
    compilerPath.get.createPath()
  }
  /** Sets the <code>compilerpathref</code> attribute. Used by Ant.
   *  @param input The value of <code>compilerpathref</code>. */
  def setCompilerPathRef(input: Reference) {
    createCompilerPath.setRefid(input)
  }

  /** Sets the <code>classpath</code> as a nested classpath Ant parameter.
   *  @return A class path to be configured. */
  def createClasspath(): Path = {
    if (classpath.isEmpty) classpath = Some(new Path(getProject()))
    classpath.get.createPath()
  }

  /** Sets the <code>classpath</code> as an external reference Ant parameter.
   *  @param input A reference to a class path. */
  def setClasspathref(input: Reference) {
    createClasspath().setRefid(input)
  }

  /** Sets the <code>sourcepath</code> attribute. Used by Ant.
   *  @param input The value of <code>sourcepath</code>. */
  def setSourcepath(input: Path) {
    if (sourcepath.isEmpty) sourcepath = Some(input)
    else sourcepath.get.append(input)
  }

  /** Sets the <code>sourcepath</code> as a nested sourcepath Ant parameter.
   *  @return A source path to be configured. */
  def createSourcepath(): Path = {
    if (sourcepath.isEmpty) sourcepath = Some(new Path(getProject()))
    sourcepath.get.createPath()
  }

  /** Sets the <code>sourcepath</code> as an external reference Ant parameter.
   *  @param input A reference to a source path. */
  def setSourcepathref(input: Reference) {
    createSourcepath().setRefid(input)
  }

  /** Sets the boot classpath attribute. Used by Ant.
   *
   *  @param input The value of <code>bootclasspath</code>. */
  def setBootclasspath(input: Path) {
    if (bootclasspath.isEmpty) bootclasspath = Some(input)
    else bootclasspath.get.append(input)
  }

  /** Sets the <code>bootclasspath</code> as a nested sourcepath Ant
   *  parameter.
   *  @return A source path to be configured. */
  def createBootclasspath(): Path = {
    if (bootclasspath.isEmpty) bootclasspath = Some(new Path(getProject()))
    bootclasspath.get.createPath()
  }

  /** Sets the <code>bootclasspath</code> as an external reference Ant
   *  parameter.
   *  @param input A reference to a source path. */
  def setBootclasspathref(input: Reference) =
    createBootclasspath().setRefid(input)

  /** Sets the external extensions path attribute. Used by Ant.
   *  @param input The value of <code>extdirs</code>. */
  def setExtdirs(input: Path) =
    if (extdirs.isEmpty) extdirs = Some(input)
    else extdirs.get.append(input)

  /** Sets the <code>extdirs</code> as a nested sourcepath Ant parameter.
   *  @return An extensions path to be configured. */
  def createExtdirs(): Path = {
    if (extdirs.isEmpty) extdirs = Some(new Path(getProject()))
    extdirs.get.createPath()
  }

  /** Sets the <code>extdirs</code> as an external reference Ant parameter.
   *  @param input A reference to an extensions path. */
  def setExtdirsref(input: Reference) =
    createExtdirs().setRefid(input)

  /** Sets the <code>encoding</code> attribute. Used by Ant.
   *  @param input The value of <code>encoding</code>. */
  def setEncoding(input: String): Unit =
    encoding = Some(input)

  /** Sets the <code>target</code> attribute. Used by Ant.
   *  @param input The value for <code>target</code>. */
  def setTarget(input: String): Unit =
    if (Target.isPermissible(input)) backend = Some(input)
    else error("Unknown target '" + input + "'")

  /** Sets the <code>force</code> attribute. Used by Ant.
   *  @param input The value for <code>force</code>. */
  def setForce(input: Boolean) { force = input }

  /** Sets the <code>fork</code> attribute. Used by Ant.
   *  @param input The value for <code>fork</code>. */
  def setFork(input : Boolean) { fork = input }
  /**
   * Sets the <code>jvmargs</code> attribute.  Used by Ant.
   * @param input The value for <code>jvmargs</code>
   */
  def setJvmargs(input : String) {
    jvmArgs = Some(input)
  }

  /** Sets the logging level attribute. Used by Ant.
   *  @param input The value for <code>logging</code>. */
  def setLogging(input: String) {
    if (LoggingLevel.isPermissible(input)) logging = Some(input)
    else error("Logging level '" + input + "' does not exist.")
  }

  /** Sets the <code>logphase</code> attribute. Used by Ant.
   *  @param input The value for <code>logPhase</code>. */
  def setLogPhase(input: String) {
    logPhase = List.fromArray(input.split(",")).flatMap { s: String =>
      val st = s.trim()
      if (CompilerPhase.isPermissible(st))
        (if (input != "") List(st) else Nil)
      else {
        error("Phase " + st + " in log does not exist.")
        Nil
      }
    }
  }

  /** Set the <code>debug</code> info attribute.
   *  @param input The value for <code>debug</code>. */
  def setDebuginfo(input: String) { debugInfo = Some(input) }

  /** Set the <code>addparams</code> info attribute.
   *  @param input The value for <code>addparams</code>. */
  def setAddparams(input: String) { addParams = input }

  /** Set the <code>deprecation</code> info attribute.
   *  @param input One of the flags <code>yes/no</code> or <code>on/off</code>. */
  def setDeprecation(input: String) {
    if (Flag.isPermissible(input))
      deprecation = Some("yes" == input || "on" == input)
    else
      error("Unknown deprecation flag '" + input + "'")
  }

  /** Set the <code>optimise</code> info attribute.
   *  @param input One of the flags <code>yes/no</code> or <code>on/off</code>. */
  def setOptimise(input: String) {
    if (Flag.isPermissible(input))
      optimise = Some("yes" == input || "on" == input)
    else
      error("Unknown optimisation flag '" + input + "'")
  }

  /** Set the <code>unchecked</code> info attribute.
   *  @param input One of the flags <code>yes/no</code> or <code>on/off</code>. */
  def setUnchecked(input: String) {
    if (Flag.isPermissible(input))
      unchecked = Some("yes" == input || "on" == input)
    else
      error("Unknown unchecked flag '" + input + "'")
  }

  /** Sets the <code>force</code> attribute. Used by Ant.
   *  @param input The value for <code>force</code>. */
  def setFailonerror(input: Boolean) { failonerror = input }

  /** Set the <code>scalacdebugging</code> info attribute. If set to
   *  <code>true</code>, the scalac ant task will print out the filenames
   *  being compiled.
   *  @param input The specified flag */
  def setScalacdebugging(input: Boolean) { scalacDebugging = input }

  def setAssemname(input: String) { assemname = Some(input) }

  def setAssemrefs(input: String) { assemrefs = Some(input) }

/*============================================================================*\
**                             Properties getters                             **
\*============================================================================*/

  /** Gets the value of the <code>classpath</code> attribute in a
   *  Scala-friendly form.
   *  @return The class path as a list of files. */
  protected def getClasspath: List[File] =
    if (classpath.isEmpty) error("Member 'classpath' is empty.")
    else List.fromArray(classpath.get.list()).map(nameToFile)

  /** Gets the value of the <code>origin</code> attribute in a
   *  Scala-friendly form.
   *  @return The origin path as a list of files. */
  protected def getOrigin: List[File] =
    if (origin.isEmpty) error("Member 'origin' is empty.")
    else List.fromArray(origin.get.list()).map(nameToFile)

  /** Gets the value of the <code>destination</code> attribute in a
   *  Scala-friendly form.
   *  @return The destination as a file. */
  protected def getDestination: File =
    if (destination.isEmpty) error("Member 'destination' is empty.")
    else existing(getProject().resolveFile(destination.get.toString))

  /** Gets the value of the <code>sourcepath</code> attribute in a
   *  Scala-friendly form.
   *  @return The source path as a list of files. */
  protected def getSourcepath: List[File] =
    if (sourcepath.isEmpty) error("Member 'sourcepath' is empty.")
    else List.fromArray(sourcepath.get.list()).map(nameToFile)

  /** Gets the value of the <code>bootclasspath</code> attribute in a
   *  Scala-friendly form.
   *  @return The boot class path as a list of files. */
  protected def getBootclasspath: List[File] =
    if (bootclasspath.isEmpty) error("Member 'bootclasspath' is empty.")
    else List.fromArray(bootclasspath.get.list()).map(nameToFile)

  /** Gets the value of the <code>extdirs</code> attribute in a
   *  Scala-friendly form.
   *  @return The extensions path as a list of files. */
  protected def getExtdirs: List[File] =
    if (extdirs.isEmpty) error("Member 'extdirs' is empty.")
    else List.fromArray(extdirs.get.list()).map(nameToFile)

/*============================================================================*\
**                       Compilation and support methods                      **
\*============================================================================*/

  /** This is forwarding method to circumvent bug #281 in Scala 2. Remove when
   *  bug has been corrected. */
  override protected def getDirectoryScanner(baseDir: File) =
    super.getDirectoryScanner(baseDir)

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
    existing(getProject().resolveFile(name))

  /** Tests if a file exists and prints a warning in case it doesn't. Always
   *  returns the file, even if it doesn't exist.
   *  @param file A file to test for existance.
   *  @return     The same file. */
  protected def existing(file: File): File = {
    if (!file.exists())
      log("Element '" + file.toString + "' does not exist.",
          Project.MSG_WARN)
    file
  }

  /** Transforms a path into a Scalac-readable string.
   *  @param path A path to convert.
   *  @return     A string-representation of the path like <code>a.jar:b.jar</code>. */
  protected def asString(path: List[File]): String =
    path.map(asString).mkString(File.pathSeparator)

  /** Transforms a file into a Scalac-readable string.
   *  @param path A file to convert.
   *  @return     A string-representation of the file like <code>/x/k/a.scala</code>. */
  protected def asString(file: File): String =
    file.getAbsolutePath()

  /** Generates a build error. Error location will be the current task in the
   *  ant file.
   *  @param message         A message describing the error.
   *  @throws BuildException A build error exception thrown in every case. */
  protected def error(message: String): Nothing =
    throw new BuildException(message, getLocation())


/*============================================================================*\
**                      Hooks for variants of Scala                           **
\*============================================================================*/

  protected def newSettings(error: String=>Unit): Settings =
    new Settings(error)
  protected def newGlobal(settings: Settings, reporter: Reporter) =
    new Global(settings, reporter)


/*============================================================================*\
**                           The big execute method                           **
\*============================================================================*/

  /** Initializes settings and source files */
  protected def initialize: (Settings, List[File], Boolean) = {
    // Tests if all mandatory attributes are set and valid.
    if (origin.isEmpty) error("Attribute 'srcdir' is not set.")
    if (getOrigin.isEmpty) error("Attribute 'srcdir' is not set.")
    if (!destination.isEmpty && !destination.get.isDirectory())
      error("Attribute 'destdir' does not refer to an existing directory.")
    if (destination.isEmpty) destination = Some(getOrigin.head)

    val mapper = new GlobPatternMapper()
    mapper.setTo("*.class")
    mapper.setFrom("*.scala")

    var javaOnly = true

    // Scans source directories to build up a compile lists.
    // If force is false, only files were the .class file in destination is
    // older than the .scala file will be used.
    val sourceFiles: List[File] =
      for {
        val originDir <- getOrigin
        val originFiles <- {
          val includedFiles = getDirectoryScanner(originDir).getIncludedFiles()
          var scalaFiles = includedFiles.filter(_.endsWith(".scala"))
          val javaFiles = includedFiles.filter(_.endsWith(".java"))
          if (!force) {
            scalaFiles = new SourceFileScanner(this).
            restrict(scalaFiles, originDir, destination.get, mapper)
          }
          javaOnly = javaOnly && (scalaFiles.length == 0)
          val list = scalaFiles.toList ::: javaFiles.toList
          if (scalacDebugging && list.length > 0)
            log(
              list.mkString(
                "Compiling source file" +
                (if (list.length > 1) "s: " else ": "),
                ", ",
                " "
              ) + "to " + getDestination.toString
            )
          else if (list.length > 0)
            log(
              "Compiling " + (
                if (javaFiles.length > 0)
                  (scalaFiles.length +" scala and "+ javaFiles.length +" java source files")
                else
                  (list.length +" source file"+ (if (list.length > 1) "s" else ""))
              ) +" to "+ getDestination.toString
            )
          else
            log("No files selected for compilation", Project.MSG_VERBOSE)
          list
        }
      }
      yield {
        log(originFiles, Project.MSG_DEBUG)
        nameToFile(originDir)(originFiles)
      }

    // Builds-up the compilation settings for Scalac with the existing Ant
    // parameters.
    val settings = newSettings(error)
    settings.outdir.value = asString(destination.get)
    if (!classpath.isEmpty)
      settings.classpath.value = asString(getClasspath)
    if (!sourcepath.isEmpty)
      settings.sourcepath.value = asString(getSourcepath)
    else if (origin.get.size() > 0)
      settings.sourcepath.value = origin.get.list()(0)
    if (!bootclasspath.isEmpty)
      settings.bootclasspath.value = asString(getBootclasspath)
    if (!extdirs.isEmpty) settings.extdirs.value = asString(getExtdirs)
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
    if (!deprecation.isEmpty) settings.deprecation.value = deprecation.get
    if (!optimise.isEmpty) settings.XO.value = optimise.get
    if (!unchecked.isEmpty) settings.unchecked.value = unchecked.get

    if (!assemname.isEmpty) settings.assemname.value = assemname.get
    if (!assemrefs.isEmpty) settings.assemrefs.value = assemrefs.get

    log("Scalac params = '" + addParams + "'", Project.MSG_DEBUG)
    settings.parseParams(addParams)

    (settings, sourceFiles, javaOnly)
  }


  override def execute() {
    val (settings, sourceFiles, javaOnly) = initialize
    if (sourceFiles.isEmpty || javaOnly) {
      return
    }
    if(fork) {
      //TODO - Error
      executeFork(settings, sourceFiles)
    } else {
      executeInternal(settings, sourceFiles)
    }
  }
  protected def executeFork(settings: Settings, sourceFiles : List[File]) {
      val java = new Java(this) // set this as owner
      java.setFork(true)
      // using 'setLine' creates multiple arguments out of a space-separated string
      for(args <- jvmArgs) {
        java.createJvmarg().setLine(args)
      }


      //Determine the path for scalac!
      val scalacPath = {
        val path = new Path(getProject)
        compilerPath match {
          case Some(p) =>
            //Use the user provided path
            path.add(p)
          case None =>
	        //Pull classpath off our classloader
	        //TODO - Allow user to override the compiler classpath
	        import _root_.org.apache.tools.ant.AntClassLoader
	        val classLoader = getClass.getClassLoader
	        if(classLoader.isInstanceOf[AntClassLoader]) {
	          path.add(new Path(getProject, classLoader.asInstanceOf[AntClassLoader].getClasspath))
	        } else {
	          throw new BuildException("Cannot determine default classpath for sclac, please specify one!")
	        }
        }
        path
      }
      java.setClasspath(scalacPath)

      java.setClassname("scala.tools.nsc.Main")
      //if (!timeout.isEmpty) java.setTimeout(timeout.get)


      //TODO - Determine when to pass args in file due to argument limits
      for ( setting <- settings.allSettings;
        arg <- setting.unparse
      ){
        java.createArg().setValue(arg)
      }



      for (file <- sourceFiles)
        java.createArg().setFile(file)

      log(java.getCommandLine.getCommandline.mkString("", " ", ""), Project.MSG_VERBOSE)
      val res = java.executeJava()
      if (failonerror && res != 0)
        error("Compilation failed because of an internal compiler error;"+
              " see the error output for details.")

  }
  /** Performs the compilation. */
  protected def executeInternal(settings: Settings, sourceFiles : List[File]) {


    val reporter = new ConsoleReporter(settings)

    // Compiles the actual code
    val compiler = newGlobal(settings, reporter)
    try {
      (new compiler.Run).compile(sourceFiles.map (_.toString))
     }
     catch {
      case exception: Throwable if (exception.getMessage ne null) =>
        exception.printStackTrace()
        error("Compile failed because of an internal compiler error (" +
          exception.getMessage + "); see the error output for details.")
      case exception =>
        exception.printStackTrace()
        error("Compile failed because of an internal compiler error " +
              "(no error message provided); see the error output for details.")
    }
    reporter.printSummary()
    if (reporter.hasErrors) {
      val msg =
        "Compile failed with " +
        reporter.ERROR.count + " error" +
        (if (reporter.ERROR.count > 1) "s" else "") +
        "; see the compiler error output for details."
      if (failonerror) error(msg) else log(msg)
    }
    else if (reporter.WARNING.count > 0)
      log(
          "Compile suceeded with " +
          reporter.WARNING.count + " warning" +
          (if (reporter.WARNING.count > 1) "s" else "") +
          "; see the compiler output for details.")
  }

}
