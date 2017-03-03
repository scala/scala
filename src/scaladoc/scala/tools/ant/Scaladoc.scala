/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant

import java.io.File

import org.apache.tools.ant.Project
import org.apache.tools.ant.types.{Path, Reference}
import org.apache.tools.ant.util.{FileUtils, GlobPatternMapper}

import scala.tools.nsc.ScalaDocReporter
import scala.tools.nsc.doc.Settings

/** An Ant task to document Scala code.
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
 *  - `encoding`,
 *  - `doctitle`,
 *  - `header`,
 *  - `footer`,
 *  - `top`,
 *  - `bottom`,
 *  - `addparams`,
 *  - `deprecation`,
 *  - `docgenerator`,
 *  - `docrootcontent`,
 *  - `unchecked`,
 *  - `nofail`,
 *  - `skipPackages`.
 *
 *  It also takes the following parameters as nested elements:
 *  - `src` (for srcdir),
 *  - `classpath`,
 *  - `sourcepath`,
 *  - `bootclasspath`,
 *  - `extdirs`.
 *
 *  @author Gilles Dubochet, Stephane Micheloud
 */
class Scaladoc extends ScalaMatchingTask {

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

  /** Defines valid values for the `deprecation` and
   *  `unchecked` properties.
   */
  object Flag extends PermissibleValue {
    val values = List("yes", "no", "on", "off")
    def getBooleanValue(value: String, flagName: String): Boolean =
      if (Flag.isPermissible(value))
        ("yes".equals(value) || "on".equals(value))
      else
        buildError("Unknown " + flagName + " flag '" + value + "'")
  }

  /** The directories that contain source files to compile. */
  private var origin: Option[Path] = None
  /** The directory to put the compiled files in. */
  private var destination: Option[File] = None

  /** The class path to use for this compilation. */
  private var classpath: Option[Path] = None
  /** The source path to use for this compilation. */
  private var sourcepath: Option[Path] = None
  /** The boot class path to use for this compilation. */
  private var bootclasspath: Option[Path] = None
  /** The external extensions path to use for this compilation. */
  private var extdirs: Option[Path] = None

  /** The character encoding of the files to compile. */
  private var encoding: Option[String] = None

  /** The fully qualified name of a doclet class, which will be used to generate the documentation. */
  private var docgenerator: Option[String] = None

  /** The file from which the documentation content of the root package will be taken */
  private var docrootcontent: Option[File] = None

  /** The document title of the generated HTML documentation. */
  private var doctitle: Option[String] = None

  /** The document footer of the generated HTML documentation. */
  private var docfooter: Option[String] = None

  /** The document version, to be added to the title. */
  private var docversion: Option[String] = None

  /** Instruct the compiler to generate links to sources */
  private var docsourceurl: Option[String] = None

  /** Point scaladoc at uncompilable sources. */
  private var docUncompilable: Option[String] = None

  /** Instruct the compiler to use additional parameters */
  private var addParams: String = ""

  /** Instruct the compiler to generate deprecation information. */
  private var deprecation: Boolean = false

  /** Instruct the compiler to generate unchecked information. */
  private var unchecked: Boolean = false

  /** Instruct the ant task not to fail in the event of errors */
  private var nofail: Boolean = false

  /** Instruct the scaladoc tool to document implicit conversions */
  private var docImplicits: Boolean = false

  /** Instruct the scaladoc tool to document all (including impossible) implicit conversions */
  private var docImplicitsShowAll: Boolean = false

  /** Instruct the scaladoc tool to output implicits debugging information */
  private var docImplicitsDebug: Boolean = false

  /** Instruct the scaladoc tool to create diagrams */
  private var docDiagrams: Boolean = false

  /** Instruct the scaladoc tool to output diagram creation debugging information */
  private var docDiagramsDebug: Boolean = false

  /** Instruct the scaladoc tool to use the binary given to create diagrams */
  private var docDiagramsDotPath: Option[String] = None

  /** Instruct the scaladoc to produce textual ouput from html pages, for easy diff-ing */
  private var docRawOutput: Boolean = false

  /** Instruct the scaladoc not to generate prefixes */
  private var docNoPrefixes: Boolean = false

  /** Instruct the scaladoc tool to group similar functions together */
  private var docGroups: Boolean = false

  /** Instruct the scaladoc tool to skip certain packages */
  private var docSkipPackages: String = ""

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  /** Sets the `srcdir` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value of `origin`.
   */
  def setSrcdir(input: Path) {
    if (origin.isEmpty) origin = Some(input)
    else origin.get.append(input)
  }

  /** Sets the `origin` as a nested src Ant parameter.
   *
   *  @return An origin path to be configured.
   */
  def createSrc(): Path = {
    if (origin.isEmpty) origin = Some(new Path(getProject))
    origin.get.createPath()
  }

  /** Sets the `origin` as an external reference Ant parameter.
   *
   *  @param input A reference to an origin path.
   */
  def setSrcref(input: Reference) {
    createSrc().setRefid(input)
  }

  /** Sets the `destdir` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value of `destination`.
   */
  def setDestdir(input: File) {
    destination = Some(input)
  }

  /** Sets the `classpath` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value of `classpath`.
   */
  def setClasspath(input: Path) {
    if (classpath.isEmpty) classpath = Some(input)
    else classpath.get.append(input)
  }

  /** Sets the `classpath` as a nested classpath Ant parameter.
   *
   *  @return A class path to be configured.
   */
  def createClasspath(): Path = {
    if (classpath.isEmpty) classpath = Some(new Path(getProject))
    classpath.get.createPath()
  }

  /** Sets the `classpath` as an external reference Ant parameter.
   *
   *  @param input A reference to a class path.
   */
  def setClasspathref(input: Reference) =
    createClasspath().setRefid(input)

  /** Sets the `sourcepath` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value of `sourcepath`.
   */
  def setSourcepath(input: Path) =
    if (sourcepath.isEmpty) sourcepath = Some(input)
    else sourcepath.get.append(input)

  /** Sets the `sourcepath` as a nested sourcepath Ant parameter.
   *
   *  @return A source path to be configured.
   */
  def createSourcepath(): Path = {
    if (sourcepath.isEmpty) sourcepath = Some(new Path(getProject))
    sourcepath.get.createPath()
  }

  /** Sets the `sourcepath` as an external reference Ant parameter.
   *
   *  @param input A reference to a source path.
   */
  def setSourcepathref(input: Reference) =
    createSourcepath().setRefid(input)

  /** Sets the `bootclasspath` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value of `bootclasspath`.
   */
  def setBootclasspath(input: Path) =
    if (bootclasspath.isEmpty) bootclasspath = Some(input)
    else bootclasspath.get.append(input)

  /** Sets the `bootclasspath` as a nested `sourcepath` Ant parameter.
   *
   *  @return A source path to be configured.
   */
  def createBootclasspath(): Path = {
    if (bootclasspath.isEmpty) bootclasspath = Some(new Path(getProject))
    bootclasspath.get.createPath()
  }

  /** Sets the `bootclasspath` as an external reference Ant parameter.
   *
   *  @param input A reference to a source path.
   */
  def setBootclasspathref(input: Reference) {
    createBootclasspath().setRefid(input)
  }

  /** Sets the external extensions path attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value of `extdirs`.
   */
  def setExtdirs(input: Path) {
    if (extdirs.isEmpty) extdirs = Some(input)
    else extdirs.get.append(input)
  }

  /** Sets the `extdirs` as a nested sourcepath Ant parameter.
   *
   *  @return An extensions path to be configured.
   */
  def createExtdirs(): Path = {
    if (extdirs.isEmpty) extdirs = Some(new Path(getProject))
    extdirs.get.createPath()
  }

  /** Sets the `extdirs` as an external reference Ant parameter.
   *
   *  @param input A reference to an extensions path.
   */
  def setExtdirsref(input: Reference) {
    createExtdirs().setRefid(input)
  }

  /** Sets the `encoding` attribute. Used by Ant.
   *
   *  @param input The value of `encoding`.
   */
  def setEncoding(input: String) {
    encoding = Some(input)
  }

  /** Sets the `docgenerator` attribute.
   *
   *  @param input A fully qualified class name of a doclet.
   */
  def setDocgenerator(input: String) {
    docgenerator = Some(input)
  }

  /**
   * Sets the `docrootcontent` attribute.
   *
   * @param input The file from which the documentation content of the root
   * package will be taken.
   */
  def setDocrootcontent(input : File) {
    docrootcontent = Some(input)
  }

  /** Sets the `docversion` attribute.
   *
   *  @param input The value of `docversion`.
   */
  def setDocversion(input: String) {
    docversion = Some(input)
  }

  /** Sets the `docsourceurl` attribute.
   *
   *  @param input The value of `docsourceurl`.
   */
  def setDocsourceurl(input: String) {
    docsourceurl = Some(input)
  }

  /** Sets the `doctitle` attribute.
   *
   *  @param input The value of `doctitle`.
   */
  def setDoctitle(input: String) {
    doctitle = Some(input)
  }

  /** Sets the `docfooter` attribute.
   *
   *  @param input The value of `docfooter`.
   */
  def setDocfooter(input: String) {
    docfooter = Some(input)
  }

  /** Set the `addparams` info attribute.
   *
   *  @param input The value for `addparams`.
   */
  def setAddparams(input: String) {
    addParams = input
  }

  /** Set the `deprecation` info attribute.
   *
   *  @param input One of the flags `yes/no` or `on/off`.
   */
  def setDeprecation(input: String) {
    if (Flag.isPermissible(input))
      deprecation = "yes".equals(input) || "on".equals(input)
    else
      buildError("Unknown deprecation flag '" + input + "'")
  }

  /** Set the `unchecked` info attribute.
   *
   *  @param input One of the flags `yes/no` or `on/off`.
   */
  def setUnchecked(input: String) {
    if (Flag.isPermissible(input))
      unchecked = "yes".equals(input) || "on".equals(input)
    else
      buildError("Unknown unchecked flag '" + input + "'")
  }

  def setDocUncompilable(input: String) {
    docUncompilable = Some(input)
  }

  /** Set the `nofail` info attribute.
   *
   *  @param input One of the flags `yes/no` or `on/off`. Default if no/off.
   */
  def setNoFail(input: String) =
      nofail = Flag.getBooleanValue(input, "nofail")

  /** Set the `implicits` info attribute.
   *  @param input One of the flags `yes/no` or `on/off`. Default if no/off. */
  def setImplicits(input: String) =
    docImplicits = Flag.getBooleanValue(input, "implicits")

  /** Set the `implicitsShowAll` info attribute to enable scaladoc to show all implicits, including those impossible to
   *  convert to from the default scope
   *  @param input One of the flags `yes/no` or `on/off`. Default if no/off. */
  def setImplicitsShowAll(input: String) =
    docImplicitsShowAll = Flag.getBooleanValue(input, "implicitsShowAll")

  /** Set the `implicitsDebug` info attribute so scaladoc outputs implicit conversion debug information
   *  @param input One of the flags `yes/no` or `on/off`. Default if no/off. */
  def setImplicitsDebug(input: String) =
    docImplicitsDebug = Flag.getBooleanValue(input, "implicitsDebug")

  /** Set the `diagrams` bit so Scaladoc adds diagrams to the documentation
   *  @param input One of the flags `yes/no` or `on/off`. Default if no/off. */
  def setDiagrams(input: String) =
    docDiagrams = Flag.getBooleanValue(input, "diagrams")

  /** Set the `diagramsDebug` bit so Scaladoc outputs diagram building debug information
   *  @param input One of the flags `yes/no` or `on/off`. Default if no/off. */
  def setDiagramsDebug(input: String) =
    docDiagramsDebug = Flag.getBooleanValue(input, "diagramsDebug")

  /** Set the `diagramsDotPath` attribute to the path where graphviz dot can be found (including the binary file name,
   *  eg: /usr/bin/dot) */
  def setDiagramsDotPath(input: String) =
    docDiagramsDotPath = Some(input)

  /** Set the `rawOutput` bit so Scaladoc also outputs text from each html file
   *  @param input One of the flags `yes/no` or `on/off`. Default if no/off. */
  def setRawOutput(input: String) =
    docRawOutput = Flag.getBooleanValue(input, "rawOutput")

  /** Set the `noPrefixes` bit to prevent Scaladoc from generating prefixes in
   *  front of types -- may lead to confusion, but significantly speeds up the generation.
   *  @param input One of the flags `yes/no` or `on/off`. Default if no/off. */
  def setNoPrefixes(input: String) =
    docNoPrefixes = Flag.getBooleanValue(input, "noPrefixes")

  /** Instruct the scaladoc tool to group similar functions together */
  def setGroups(input: String) =
    docGroups = Flag.getBooleanValue(input, "groups")

  /** Instruct the scaladoc tool to skip certain packages.
   *  @param input A colon-delimited list of fully qualified package names that will be skipped from scaladoc.
   */
  def setSkipPackages(input: String) =
    docSkipPackages = input

/*============================================================================*\
**                             Properties getters                             **
\*============================================================================*/

  /** Gets the value of the `classpath` attribute in a
   *  Scala-friendly form.
   *
   *  @return The class path as a list of files.
   */
  private def getClasspath: List[File] =
    if (classpath.isEmpty) buildError("Member 'classpath' is empty.")
    else classpath.get.list().toList map nameToFile

  /** Gets the value of the `origin` attribute in a Scala-friendly
   *  form.
   *
   *  @return The origin path as a list of files.
   */
  private def getOrigin: List[File] =
    if (origin.isEmpty) buildError("Member 'origin' is empty.")
    else origin.get.list().toList map nameToFile

  /** Gets the value of the `destination` attribute in a
   *  Scala-friendly form.
   *
   *  @return The destination as a file.
   */
  private def getDestination: File =
    if (destination.isEmpty) buildError("Member 'destination' is empty.")
    else existing(getProject resolveFile destination.get.toString)

  /** Gets the value of the `sourcepath` attribute in a
   *  Scala-friendly form.
   *
   *  @return The source path as a list of files.
   */
  private def getSourcepath: List[File] =
    if (sourcepath.isEmpty) buildError("Member 'sourcepath' is empty.")
    else sourcepath.get.list().toList map nameToFile

  /** Gets the value of the `bootclasspath` attribute in a
   *  Scala-friendly form.
   *
   *  @return The boot class path as a list of files.
   */
  private def getBootclasspath: List[File] =
    if (bootclasspath.isEmpty) buildError("Member 'bootclasspath' is empty.")
    else bootclasspath.get.list().toList map nameToFile

  /** Gets the value of the `extdirs` attribute in a
   *  Scala-friendly form.
   *
   *  @return The extensions path as a list of files.
   */
  private def getExtdirs: List[File] =
    if (extdirs.isEmpty) buildError("Member 'extdirs' is empty.")
    else extdirs.get.list().toList map nameToFile

/*============================================================================*\
**                       Compilation and support methods                      **
\*============================================================================*/

  /** This is forwarding method to circumvent bug #281 in Scala 2. Remove when
   *  bug has been corrected.
   */
  override protected def getDirectoryScanner(baseDir: java.io.File) =
    super.getDirectoryScanner(baseDir)

  /** Transforms a string name into a file relative to the provided base
   *  directory.
   *
   *  @param base A file pointing to the location relative to which the name
   *              will be resolved.
   *  @param name A relative or absolute path to the file as a string.
   *  @return     A file created from the name and the base file.
   */
  private def nameToFile(base: File)(name: String): File =
    existing(fileUtils.resolveFile(base, name))

  /** Transforms a string name into a file relative to the build root
   *  directory.
   *
   *  @param name A relative or absolute path to the file as a string.
   *  @return     A file created from the name.
   */
  private def nameToFile(name: String): File =
    existing(getProject resolveFile name)

  /** Tests if a file exists and prints a warning in case it doesn't. Always
   *  returns the file, even if it doesn't exist.
   *
   *  @param file A file to test for existence.
   *  @return     The same file.
   */
  private def existing(file: File): File = {
    if (!file.exists())
      log("Element '" + file.toString + "' does not exist.",
          Project.MSG_WARN)
    file
  }

  /** Transforms a path into a Scalac-readable string.
   *
   *  @param path A path to convert.
   *  @return     A string-representation of the path like `a.jar:b.jar`.
   */
  private def asString(path: List[File]): String =
    path.map(asString).mkString("", File.pathSeparator, "")

  /** Transforms a file into a Scalac-readable string.
   *
   *  @param file A file to convert.
   *  @return     A string-representation of the file like `/x/k/a.scala`.
   */
  private def asString(file: File): String =
    file.getAbsolutePath()

/*============================================================================*\
**                           The big execute method                           **
\*============================================================================*/

  /** Initializes settings and source files */
  protected def initialize: Tuple2[Settings, List[File]] = {
    // Tests if all mandatory attributes are set and valid.
    if (origin.isEmpty) buildError("Attribute 'srcdir' is not set.")
    if (getOrigin.isEmpty) buildError("Attribute 'srcdir' is not set.")
    if (!destination.isEmpty && !destination.get.isDirectory())
      buildError("Attribute 'destdir' does not refer to an existing directory.")
    if (destination.isEmpty) destination = Some(getOrigin.head)

    val mapper = new GlobPatternMapper()
    mapper setTo "*.html"
    mapper setFrom "*.scala"

    // Scans source directories to build up a compile lists.
    // If force is false, only files were the .class file in destination is
    // older than the .scala file will be used.
    val sourceFiles: List[File] =
      for {
        originDir <- getOrigin
        originFile <- {
          val includedFiles =
            getDirectoryScanner(originDir).getIncludedFiles()
          val list = includedFiles.toList
          if (list.length > 0)
            log(
              "Documenting " + list.length + " source file" +
              (if (list.length > 1) "s" else "") +
              (" to " + getDestination.toString)
            )
          else
            log("No files selected for documentation", Project.MSG_VERBOSE)

          list
        }
      } yield {
        log(originFile, Project.MSG_DEBUG)
        nameToFile(originDir)(originFile)
      }

    def decodeEscapes(s: String): String = {
      // In Ant script characters '<' and '>' must be encoded when
      // used in attribute values, e.g. for attributes "doctitle", "header", ..
      // in task Scaladoc you may write:
      //   doctitle="&lt;div&gt;Scala&lt;/div&gt;"
      // so we have to decode them here.
      s.replaceAll("&lt;", "<").replaceAll("&gt;",">")
       .replaceAll("&amp;", "&").replaceAll("&quot;", "\"")
    }

    // Builds-up the compilation settings for Scalac with the existing Ant
    // parameters.
    val docSettings = new Settings(buildError)
    docSettings.outdir.value = asString(destination.get)
    if (!classpath.isEmpty)
      docSettings.classpath.value = asString(getClasspath)
    if (!sourcepath.isEmpty)
      docSettings.sourcepath.value = asString(getSourcepath)
    /*else if (origin.get.size() > 0)
      settings.sourcepath.value = origin.get.list()(0)*/
    if (!bootclasspath.isEmpty)
      docSettings.bootclasspath.value = asString(getBootclasspath)
    if (!extdirs.isEmpty) docSettings.extdirs.value = asString(getExtdirs)
    if (!encoding.isEmpty) docSettings.encoding.value = encoding.get
    if (!doctitle.isEmpty) docSettings.doctitle.value = decodeEscapes(doctitle.get)
    if (!docfooter.isEmpty) docSettings.docfooter.value = decodeEscapes(docfooter.get)
    if (!docversion.isEmpty) docSettings.docversion.value = decodeEscapes(docversion.get)
    if (!docsourceurl.isEmpty) docSettings.docsourceurl.value = decodeEscapes(docsourceurl.get)
    if (!docUncompilable.isEmpty) docSettings.docUncompilable.value = decodeEscapes(docUncompilable.get)

    docSettings.deprecation.value = deprecation
    docSettings.unchecked.value = unchecked
    docSettings.docImplicits.value = docImplicits
    docSettings.docImplicitsDebug.value = docImplicitsDebug
    docSettings.docImplicitsShowAll.value = docImplicitsShowAll
    docSettings.docDiagrams.value = docDiagrams
    docSettings.docDiagramsDebug.value = docDiagramsDebug
    docSettings.docRawOutput.value = docRawOutput
    docSettings.docNoPrefixes.value = docNoPrefixes
    docSettings.docGroups.value = docGroups
    docSettings.docSkipPackages.value = docSkipPackages
    if(!docDiagramsDotPath.isEmpty) docSettings.docDiagramsDotPath.value = docDiagramsDotPath.get

    if (!docgenerator.isEmpty) docSettings.docgenerator.value = docgenerator.get
    if (!docrootcontent.isEmpty) docSettings.docRootContent.value = docrootcontent.get.getAbsolutePath()
    log("Scaladoc params = '" + addParams + "'", Project.MSG_DEBUG)

    docSettings processArgumentString addParams
    (docSettings, sourceFiles)
  }

  def safeBuildError(message: String): Unit = if (nofail) log(message) else buildError(message)

  /** Performs the compilation. */
  override def execute() = {
    val (docSettings, sourceFiles) = initialize
    val reporter = new ScalaDocReporter(docSettings)
    try {
      val docProcessor = new scala.tools.nsc.doc.DocFactory(reporter, docSettings)
      docProcessor.document(sourceFiles.map (_.toString))
      if (reporter.ERROR.count > 0)
        safeBuildError(
          "Document failed with " +
          reporter.ERROR.count + " error" +
          (if (reporter.ERROR.count > 1) "s" else "") +
          "; see the documenter error output for details.")
      else if (reporter.WARNING.count > 0)
        log(
          "Document succeeded with " +
          reporter.WARNING.count + " warning" +
          (if (reporter.WARNING.count > 1) "s" else "") +
          "; see the documenter output for details.")
      reporter.printSummary()
    } catch {
      case exception: Throwable =>
        exception.printStackTrace()
        val msg = Option(exception.getMessage) getOrElse "no error message provided"
        safeBuildError(s"Document failed because of an internal documenter error ($msg); see the error output for details.")
    }
  }
}
