/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.ant

import java.io.{File, InputStream, FileWriter}

import org.apache.tools.ant.BuildException
import org.apache.tools.ant.taskdefs.MatchingTask
import org.apache.tools.ant.types.{Path, Reference}

/** <p>
 *    An Ant task that generates a shell or batch script to execute a
 *    Scala program.
 *    This task can take the following parameters as attributes:
 *  </p><ul>
 *  <li>file (mandatory),</li>
 *  <li>class (mandatory),</li>
 *  <li>platforms,</li>
 *  <li>classpath,</li>
 *  <li>properties,</li>
 *  <li>javaflags,</li>
 *  <li>toolflags.</li></ul>
 *
 * @author  Gilles Dubochet
 * @version 1.1
 */
class ScalaTool extends MatchingTask {

  private def emptyPath = new Path(getProject)

/*============================================================================*\
**                             Ant user-properties                            **
\*============================================================================*/

  abstract class PermissibleValue {
    val values: List[String]
    def isPermissible(value: String): Boolean =
      (value == "") || values.exists(_.startsWith(value))
  }

  /** Defines valid values for the platforms property. */
  object Platforms extends PermissibleValue {
    val values = List("unix", "windows")
  }

  /** The path to the exec script file. ".bat" will be appended for the
    * Windows BAT file, if generated. */
  private var file: Option[File] = None

  /** The main class to run. */
  private var mainClass: Option[String] = None

  /** Supported platforms for the script. Either "unix" or "windows".
    * Defaults to both. */
  private var platforms: List[String] = List("unix", "windows")

  /** An (optional) path to all JARs that this script depend on. Paths must be
    * relative to the scala home directory. If not set, all JAR archives and
    * folders in "lib/" are automatically added. */
  private var classpath: Option[Path] = None

  /** Comma-separated Java system properties to pass to the JRE. Properties
    * are formated as name=value. Properties scala.home, scala.tool.name and
    * scala.tool.version are always set. */
  private var properties: List[(String, String)] = Nil

  /** Additional flags passed to the JRE ("java [javaFlags] class"). */
  private var javaFlags: String = ""

  /** Additional flags passed to the tool ("java class [toolFlags]"). Can only
    * be set when a main class is defined. */
  private var toolFlags: String = ""

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  /** Sets the file attribute. */
  def setFile(input: File) =
    file = Some(input)

  /** Sets the main class attribute. */
  def setClass(input: String) =
    mainClass = Some(input)

  /** Sets the platforms attribute. */
  def setPlatforms(input: String) = {
    platforms = List.fromArray(input.split(",")).flatMap { s: String =>
      val st = s.trim
      if (Platforms.isPermissible(st))
        (if (input != "") List(st) else Nil)
      else {
        error("Platform " + st + " does not exist.")
        Nil
      }
    }
  }

  /** Sets the classpath with which to run the tool. */
  def setClassPath(input: Path): Unit =
    if (classpath.isEmpty)
      classpath = Some(input)
    else
      classpath.get.append(input)
  def createClassPath: Path = {
    if (classpath.isEmpty) classpath = Some(emptyPath)
    classpath.get.createPath()
  }
  def setClassPathRef(input: Reference): Unit =
    createClassPath.setRefid(input)

  /** Sets JVM properties that will be set whilst running the tool. */
  def setProperties(input: String) = {
    properties = List.fromArray(input.split(",")).flatMap { s: String =>
      val st = s.trim
      val stArray = st.split("=", 2)
      if (stArray.length == 2) {
        if (input != "") List(Pair(stArray(0), stArray(1))) else Nil
      }
      else
        error("Property " + st + " is not formatted properly.")
    }
  }

  /** Sets flags to be passed to the Java interpreter. */
  def setJavaflags(input: String) =
    javaFlags = input.trim

  /** Sets flags to be passed to the tool. */
  def setToolflags(input: String) =
    toolFlags = input.trim

/*============================================================================*\
**                             Properties getters                             **
\*============================================================================*/

    /** Gets the value of the classpath attribute in a Scala-friendly form.
      * @returns The class path as a list of files. */
    private def getUnixclasspath: String =
      classpath.getOrElse(emptyPath).list.mkString("", ":", "")

    /** Gets the value of the classpath attribute in a Scala-friendly form.
      * @returns The class path as a list of files. */
    private def getWinclasspath: String =
      classpath.getOrElse(emptyPath).list.map(_.replace('/', '\\')).
                mkString("", ";", "")

    private def getProperties: String =
      properties.map({
        case Pair(name,value) => "-D" + name + "=\"" + value + "\""
      }).mkString("", " ", "")

/*============================================================================*\
**                       Compilation and support methods                      **
\*============================================================================*/

    /** Generates a build error. Error location will be the current task in the
      * ant file.
      * @param message A message describing the error.
      * @throws BuildException A build error exception thrown in every case. */
    private def error(message: String): Nothing =
      throw new BuildException(message, getLocation())

    // XXX encoding and generalize
    private def getResourceAsCharStream(clazz: Class[_], resource: String): Stream[Char] = {
      val stream = clazz.getClassLoader() getResourceAsStream resource
      if (stream == null) Stream.empty
      else Stream continually stream.read() takeWhile (_ != -1) map (_.asInstanceOf[Char])
    }

    private def readAndPatchResource(resource: String, tokens: Map[String, String]): String = {
      val chars = getResourceAsCharStream(this.getClass, resource).elements
      val builder = new StringBuilder()

      while (chars.hasNext) {
        val char = chars.next
        if (char == '@') {
          var char = chars.next
          val token = new StringBuilder()
          while (chars.hasNext && char != '@') {
            token.append(char)
            char = chars.next
          }
          if (tokens.contains(token.toString))
            builder.append(tokens(token.toString))
          else if (token.toString == "")
            builder.append('@')
          else
            builder.append("@" + token.toString + "@")
        } else builder.append(char)
      }
      builder.toString
    }

    private def writeFile(file: File, content: String) =
      if (file.exists() && !file.canWrite())
        error("File " + file + " is not writable")
      else {
        val writer = new FileWriter(file, false)
        writer.write(content)
        writer.close()
      }

/*============================================================================*\
**                           The big execute method                           **
\*============================================================================*/

  /** Performs the tool creation. */
  override def execute() = {
    // Tests if all mandatory attributes are set and valid.
    if (file.isEmpty) error("Attribute 'file' is not set.")
    if (mainClass.isEmpty) error("Main class must be set.")
    val resourceRoot = "scala/tools/ant/templates/"
    val patches = Map (
      ("class", mainClass.get),
      ("properties", getProperties),
      ("javaflags", javaFlags),
      ("toolflags", toolFlags)
    )
    if (platforms.contains("unix")) {
      val unixPatches = patches + (("classpath", getUnixclasspath))
      val unixTemplateResource = resourceRoot + "tool-unix.tmpl"
      val unixTemplate = readAndPatchResource(unixTemplateResource, unixPatches)
      writeFile(file.get, unixTemplate)
    }
    if (platforms.contains("windows")) {
      val winPatches = patches + (("classpath", getWinclasspath))
      val winTemplateResource = resourceRoot + "tool-windows.tmpl"
      val winTemplate = readAndPatchResource(winTemplateResource, winPatches)
      writeFile(new File(file.get.getAbsolutePath() + ".bat"), winTemplate)
    }
  }

}
