/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools.ant

import java.io.File

import org.apache.tools.ant.{BuildException, Project}
import org.apache.tools.ant.types.{Path, Reference}
import org.apache.tools.ant.util.{FileUtils, GlobPatternMapper}

import scala.tools.nsc.Global
import scala.tools.nsc.doc.Settings
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}

/** <p>
 *    An Ant task to document Scala code.
 *  </p>
 *  <p>
 *    This task can take the following parameters as attributes:
 *  </p>
 *  <ul>
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
 *    <li>doctitle,</li>
 *    <li>header,</li>
 *    <li>footer,</li>
 *    <li>top,</li>
 *    <li>bottom,</li>
 *    <li>addparams,</li>
 *    <li>deprecation,</li>
 *    <li>docgenerator,</li>
 *    <li>unchecked.</li>
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
 *  </ul>
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

  /** Defines valid values for the <code>deprecation</code> and
   *  <code>unchecked</code> properties.
   */
  object Flag extends PermissibleValue {
    val values = List("yes", "no", "on", "off")
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

  /** The document title of the generated HTML documentation. */
  private var doctitle: Option[String] = None

  /** The document version, to be added to the title. */
  private var docversion: Option[String] = None

  /** Instruct the compiler to generate links to sources */
  private var docsourceurl: Option[String] = None

  /** Instruct the compiler to use additional parameters */
  private var addParams: String = ""

  /** Instruct the compiler to generate deprecation information. */
  private var deprecation: Boolean = false

  /** Instruct the compiler to generate unchecked information. */
  private var unchecked: Boolean = false

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  /** Sets the <code>srcdir</code> attribute. Used by Ant.
   *
   *  @param input The value of <code>origin</code>.
   */
  def setSrcdir(input: Path) {
    if (origin.isEmpty) origin = Some(input)
    else origin.get.append(input)
  }

  /** Sets the <code>origin</code> as a nested src Ant parameter.
   *
   *  @return An origin path to be configured.
   */
  def createSrc(): Path = {
    if (origin.isEmpty) origin = Some(new Path(getProject()))
    origin.get.createPath()
  }

  /** Sets the <code>origin</code> as an external reference Ant parameter.
   *
   *  @param input A reference to an origin path.
   */
  def setSrcref(input: Reference) {
    createSrc().setRefid(input)
  }

  /** Sets the <code>destdir</code> attribute. Used by Ant.
   *
   *  @param input The value of <code>destination</code>.
   */
  def setDestdir(input: File) {
    destination = Some(input)
  }

  /** Sets the <code>classpath</code> attribute. Used by Ant.
   *
   *  @param input The value of <code>classpath</code>.
   */
  def setClasspath(input: Path) {
    if (classpath.isEmpty) classpath = Some(input)
    else classpath.get.append(input)
  }

  /** Sets the <code>classpath</code> as a nested classpath Ant parameter.
   *
   *  @return A class path to be configured.
   */
  def createClasspath(): Path = {
    if (classpath.isEmpty) classpath = Some(new Path(getProject()))
    classpath.get.createPath()
  }

  /** Sets the <code>classpath</code> as an external reference Ant parameter.
   *
   *  @param input A reference to a class path.
   */
  def setClasspathref(input: Reference) =
    createClasspath().setRefid(input)

  /** Sets the <code>sourcepath</code> attribute. Used by Ant.
   *
   *  @param input The value of <code>sourcepath</code>.
   */
  def setSourcepath(input: Path) =
    if (sourcepath.isEmpty) sourcepath = Some(input)
    else sourcepath.get.append(input)

  /** Sets the <code>sourcepath</code> as a nested sourcepath Ant parameter.
   *
   *  @return A source path to be configured.
   */
  def createSourcepath(): Path = {
    if (sourcepath.isEmpty) sourcepath = Some(new Path(getProject()))
    sourcepath.get.createPath()
  }

  /** Sets the <code>sourcepath</code> as an external reference Ant parameter.
   *
   *  @param input A reference to a source path.
   */
  def setSourcepathref(input: Reference) =
    createSourcepath().setRefid(input)

  /** Sets the <code>bootclasspath</code> attribute. Used by Ant.
   *
   *  @param input The value of <code>bootclasspath</code>.
   */
  def setBootclasspath(input: Path) =
    if (bootclasspath.isEmpty) bootclasspath = Some(input)
    else bootclasspath.get.append(input)

  /** Sets the <code>bootclasspath</code> as a nested sourcepath Ant
   *  parameter.
   *
   *  @return A source path to be configured.
   */
  def createBootclasspath(): Path = {
    if (bootclasspath.isEmpty) bootclasspath = Some(new Path(getProject()))
    bootclasspath.get.createPath()
  }

  /** Sets the <code>bootclasspath</code> as an external reference Ant
   *  parameter.
   *
   *  @param input A reference to a source path.
   */
  def setBootclasspathref(input: Reference) {
    createBootclasspath().setRefid(input)
  }

  /** Sets the external extensions path attribute. Used by Ant.
   *
   *  @param input The value of <code>extdirs</code>.
   */
  def setExtdirs(input: Path) {
    if (extdirs.isEmpty) extdirs = Some(input)
    else extdirs.get.append(input)
  }

  /** Sets the <code>extdirs</code> as a nested sourcepath Ant parameter.
   *
   *  @return An extensions path to be configured.
   */
  def createExtdirs(): Path = {
    if (extdirs.isEmpty) extdirs = Some(new Path(getProject()))
    extdirs.get.createPath()
  }

  /** Sets the <code>extdirs</code> as an external reference Ant parameter.
   *
   *  @param input A reference to an extensions path.
   */
  def setExtdirsref(input: Reference) {
    createExtdirs().setRefid(input)
  }

  /** Sets the <code>encoding</code> attribute. Used by Ant.
   *
   *  @param input The value of <code>encoding</code>.
   */
  def setEncoding(input: String) {
    encoding = Some(input)
  }

  /** Sets the <code>docgenerator</code> attribute.
   *
   *  @param input A fully qualified class name of a doclet.
   */
  def setDocgenerator(input: String) {
    docgenerator = Some(input)
  }

  /** Sets the <code>docversion</code> attribute.
   *
   *  @param input The value of <code>docversion</code>.
   */
  def setDocversion(input: String) {
    docversion = Some(input)
  }

  /** Sets the <code>docsourceurl</code> attribute.
   *
   *  @param input The value of <code>docsourceurl</code>.
   */
  def setDocsourceurl(input: String) {
    docsourceurl = Some(input)
  }

  /** Sets the <code>doctitle</code> attribute.
   *
   *  @param input The value of <code>doctitle</code>.
   */
  def setDoctitle(input: String) {
    doctitle = Some(input)
  }

  /** Set the <code>addparams</code> info attribute.
   *
   *  @param input The value for <code>addparams</code>.
   */
  def setAddparams(input: String) {
    addParams = input
  }

  /** Set the <code>deprecation</code> info attribute.
   *
   *  @param input One of the flags <code>yes/no</code> or <code>on/off</code>.
   */
  def setDeprecation(input: String) {
    if (Flag.isPermissible(input))
      deprecation = "yes".equals(input) || "on".equals(input)
    else
      buildError("Unknown deprecation flag '" + input + "'")
  }

  /** Set the <code>unchecked</code> info attribute.
   *
   *  @param input One of the flags <code>yes/no</code> or <code>on/off</code>.
   */
  def setUnchecked(input: String) {
    if (Flag.isPermissible(input))
      unchecked = "yes".equals(input) || "on".equals(input)
    else
      buildError("Unknown unchecked flag '" + input + "'")
  }

/*============================================================================*\
**                             Properties getters                             **
\*============================================================================*/

  /** Gets the value of the <code>classpath</code> attribute in a
   *  Scala-friendly form.
   *
   *  @return The class path as a list of files.
   */
  private def getClasspath: List[File] =
    if (classpath.isEmpty) buildError("Member 'classpath' is empty.")
    else classpath.get.list().toList.map(nameToFile)

  /** Gets the value of the <code>origin</code> attribute in a Scala-friendly
   *  form.
   *
   *  @return The origin path as a list of files.
   */
  private def getOrigin: List[File] =
    if (origin.isEmpty) buildError("Member 'origin' is empty.")
    else origin.get.list().toList.map(nameToFile)

  /** Gets the value of the <code>destination</code> attribute in a
   *  Scala-friendly form.
   *
   *  @return The destination as a file.
   */
  private def getDestination: File =
    if (destination.isEmpty) buildError("Member 'destination' is empty.")
    else existing(getProject().resolveFile(destination.get.toString))

  /** Gets the value of the <code>sourcepath</code> attribute in a
   *  Scala-friendly form.
   *
   *  @return The source path as a list of files.
   */
  private def getSourcepath: List[File] =
    if (sourcepath.isEmpty) buildError("Member 'sourcepath' is empty.")
    else sourcepath.get.list().toList.map(nameToFile)

  /** Gets the value of the <code>bootclasspath</code> attribute in a
   *  Scala-friendly form.
   *
   *  @return The boot class path as a list of files.
   */
  private def getBootclasspath: List[File] =
    if (bootclasspath.isEmpty) buildError("Member 'bootclasspath' is empty.")
    else bootclasspath.get.list().toList.map(nameToFile)

  /** Gets the value of the <code>extdirs</code> attribute in a
   *  Scala-friendly form.
   *
   *  @return The extensions path as a list of files.
   */
  private def getExtdirs: List[File] =
    if (extdirs.isEmpty) buildError("Member 'extdirs' is empty.")
    else extdirs.get.list().toList.map(nameToFile)

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
    existing(getProject().resolveFile(name))

  /** Tests if a file exists and prints a warning in case it doesn't. Always
   *  returns the file, even if it doesn't exist.
   *
   *  @param file A file to test for existance.
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
   *  @return     A string-representation of the path like <code>a.jar:b.jar</code>.
   */
  private def asString(path: List[File]): String =
    path.map(asString).mkString("", File.pathSeparator, "")

  /** Transforms a file into a Scalac-readable string.
   *
   *  @param path A file to convert.
   *  @return     A string-representation of the file like <code>/x/k/a.scala</code>.
   */
  private def asString(file: File): String =
    file.getAbsolutePath()

/*============================================================================*\
**                           The big execute method                           **
\*============================================================================*/

  /** Initializes settings and source files */
  protected def initialize: Pair[Settings, List[File]] = {
    // Tests if all mandatory attributes are set and valid.
    if (origin.isEmpty) buildError("Attribute 'srcdir' is not set.")
    if (getOrigin.isEmpty) buildError("Attribute 'srcdir' is not set.")
    if (!destination.isEmpty && !destination.get.isDirectory())
      buildError("Attribute 'destdir' does not refer to an existing directory.")
    if (destination.isEmpty) destination = Some(getOrigin.head)

    val mapper = new GlobPatternMapper()
    mapper.setTo("*.html")
    mapper.setFrom("*.scala")

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
    if (!docversion.isEmpty) docSettings.docversion.value = decodeEscapes(docversion.get)
    if (!docsourceurl.isEmpty) docSettings.docsourceurl.value =decodeEscapes(docsourceurl.get)
    docSettings.deprecation.value = deprecation
    docSettings.unchecked.value = unchecked
    if (!docgenerator.isEmpty) docSettings.docgenerator.value = docgenerator.get
    log("Scaladoc params = '" + addParams + "'", Project.MSG_DEBUG)

    docSettings processArgumentString addParams
    Pair(docSettings, sourceFiles)
  }

  /** Performs the compilation. */
  override def execute() = {
    val Pair(docSettings, sourceFiles) = initialize
    val reporter = new ConsoleReporter(docSettings)
    try {
      val docProcessor = new scala.tools.nsc.doc.DocFactory(reporter, docSettings)
      docProcessor.document(sourceFiles.map (_.toString))
      if (reporter.ERROR.count > 0)
        buildError(
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
      case exception: Throwable if exception.getMessage ne null =>
        exception.printStackTrace()
        buildError("Document failed because of an internal documenter error (" +
          exception.getMessage + "); see the error output for details.")
      case exception =>
        exception.printStackTrace()
        buildError("Document failed because of an internal documenter error " +
          "(no error message provided); see the error output for details.")
    }
  }

}
