/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant

import java.io.{File, FileWriter}
import org.apache.tools.ant.types.{Path, Reference}

/** An Ant task that generates a shell or batch script to execute a
 *  Scala program.
 *
 *  This task can take the following parameters as attributes:
 *  - `file` (mandatory),
 *  - `class` (mandatory),
 *  - `platforms`,
 *  - `classpath`,
 *  - `properties`,
 *  - `javaflags`,
 *  - `toolflags`.
 *
 * @author  Gilles Dubochet
 * @version 1.1
 */
class ScalaTool extends ScalaMatchingTask {

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

  /** The path to the exec script file. `".bat"` will be appended for the
    * Windows BAT file, if generated. */
  private var file: Option[File] = None

  /** The main class to run. */
  private var mainClass: Option[String] = None

  /** Supported platforms for the script. Either `"unix"` or `"windows"`.
    * Defaults to both. */
  private var platforms: List[String] = List("unix", "windows")

  /** An (optional) path to all JARs that this script depend on. Paths must be
    * relative to the scala home directory. If not set, all JAR archives and
    * folders in `"lib/"` are automatically added. */
  private var classpath: List[String] = Nil

  /** An (optional) path to JARs that this script depends on relative to the
    * ant project's `basedir`. */
  private var classpathPath: Path = emptyPath

  /** Comma-separated Java system properties to pass to the JRE. Properties
    * are formatted as `name=value`. Properties `scala.home`, `scala.tool.name`
    * and `scala.tool.version` are always set. */
  private var properties: List[(String, String)] = Nil

  /** Additional flags passed to the JRE (`"java [javaFlags] class"`). */
  private var javaFlags: String = ""

  /** Additional flags passed to the tool (`"java class [toolFlags]"`).
    * Can only be set when a main class is defined. */
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
    platforms = input.split(",").toList.flatMap { s: String =>
      val st = s.trim
      if (Platforms.isPermissible(st))
        (if (input != "") List(st) else Nil)
      else {
        buildError("Platform " + st + " does not exist.")
      }
    }
  }

  /** Sets the classpath with which to run the tool.
   *
   *  Note that this mechanism of setting the classpath is generally preferred
   *  for general purpose scripts, as this does not assume all elements are
   *  relative to the Ant `basedir`.  Additionally, the platform specific
   *  demarcation of any script variables (e.g. `${SCALA_HOME}` or
   * `%SCALA_HOME%`) can be specified in a platform independent way (e.g.
   * `@SCALA_HOME@`) and automatically translated for you.
   */
  def setClassPath(input: String) {
    classpath = classpath ::: input.split(",").toList
  }

  /**
   * A special method that allows ant classpath path definitions to be nested
   * within this ant task.
   */
  def createClassPath: Path = classpathPath.createPath()

  /**
   * Adds an Ant Path reference to the tool's classpath.
   * Note that all entries in the path must exist either relative to the project
   * basedir or with an absolute path to a file in the filesystem.  As a result,
   * this is not a mechanism for setting the classpath for more general use scripts.
   */
  def setClassPathRef(input: Reference) {
    val tmpPath = emptyPath
    tmpPath.setRefid(input)
    classpath = classpath ::: tmpPath.list.toList
  }

  /** Sets JVM properties that will be set whilst running the tool. */
  def setProperties(input: String) = {
    properties = input.split(",").toList.flatMap { s: String =>
      val st = s.trim
      val stArray = st.split("=", 2)
      if (stArray.length == 2) {
        if (input != "") List((stArray(0), stArray(1))) else Nil
      }
      else
        buildError("Property " + st + " is not formatted properly.")
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
      * @return The class path as a list of files. */
    private def getUnixclasspath: String =
      transposeVariableMarkup(classpath.mkString("", ":", "").replace('\\', '/'), "${", "}")

    /** Gets the value of the classpath attribute in a Scala-friendly form.
      * @return The class path as a list of files. */
    private def getWinclasspath: String =
      transposeVariableMarkup(classpath.mkString("", ";", "").replace('/', '\\'), "%", "%")

    private def getProperties: String =
      properties.map({
        case (name,value) => "-D" + name + "=\"" + value + "\""
      }).mkString("", " ", "")

/*============================================================================*\
**                       Compilation and support methods                      **
\*============================================================================*/

    // XXX encoding and generalize
    private def getResourceAsCharStream(clazz: Class[_], resource: String): Stream[Char] = {
      val stream = clazz.getClassLoader() getResourceAsStream resource
      if (stream == null) Stream.empty
      else Stream continually stream.read() takeWhile (_ != -1) map (_.asInstanceOf[Char])
    }

    // Converts a variable like @SCALA_HOME@ to ${SCALA_HOME} when pre = "${" and post = "}"
    private def transposeVariableMarkup(text: String, pre: String, post: String) : String = {
      val chars = scala.io.Source.fromString(text)
      val builder = new StringBuilder()

      while (chars.hasNext) {
        val char = chars.next()
        if (char == '@') {
          var char = chars.next()
          val token = new StringBuilder()
          while (chars.hasNext && char != '@') {
            token.append(char)
            char = chars.next()
          }
          if (token.toString == "")
            builder.append('@')
          else
            builder.append(pre + token.toString + post)
        } else builder.append(char)
      }
      builder.toString
    }

    private def readAndPatchResource(resource: String, tokens: Map[String, String]): String = {
      val chars = getResourceAsCharStream(this.getClass, resource).iterator
      val builder = new StringBuilder()

      while (chars.hasNext) {
        val char = chars.next()
        if (char == '@') {
          var char = chars.next()
          val token = new StringBuilder()
          while (chars.hasNext && char != '@') {
            token.append(char)
            char = chars.next()
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
        buildError("File " + file + " is not writable")
      else {
        val writer = new FileWriter(file, false)
        writer write content
        writer.close()
      }

/*============================================================================*\
**                           The big execute method                           **
\*============================================================================*/

  /** Performs the tool creation. */
  override def execute() = {
    // Tests if all mandatory attributes are set and valid.
    if (file.isEmpty) buildError("Attribute 'file' is not set.")
    if (mainClass.isEmpty) buildError("Main class must be set.")
    val resourceRoot = "scala/tools/ant/templates/"
    val patches = Map (
      ("class", mainClass.get),
      ("properties", getProperties),
      ("javaflags", javaFlags),
      ("toolflags", toolFlags)
    )
    // Consolidate Paths into classpath
    classpath = classpath ::: classpathPath.list.toList
    // Generate the scripts
    if (platforms contains "unix") {
      val unixPatches = patches + (("classpath", getUnixclasspath))
      val unixTemplateResource = resourceRoot + "tool-unix.tmpl"
      val unixTemplate = readAndPatchResource(unixTemplateResource, unixPatches)
      writeFile(file.get, unixTemplate)
    }
    if (platforms contains "windows") {
      val winPatches = patches + (("classpath", getWinclasspath))
      val winTemplateResource = resourceRoot + "tool-windows.tmpl"
      val winTemplate = readAndPatchResource(winTemplateResource, winPatches)
      writeFile(new File(file.get.getAbsolutePath() + ".bat"), winTemplate)
    }
  }

}
