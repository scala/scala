/*    __  ______________                                                      *\
**   /  |/ / ____/ ____/                                                      **
**  / | | /___  / /___                                                        **
** /_/|__/_____/_____/ Copyright 2005-2006 LAMP/EPFL                          **
**
** $Id$
\*                                                                            */

package scala.tools.ant {

  import scala.collection.immutable.{Map, ListMap}

  import java.io.{File, InputStream, FileWriter}
  import java.net.{URL, URLClassLoader}
  import java.util.{ArrayList, Vector}

  import org.apache.tools.ant.{AntClassLoader, BuildException,
                               DirectoryScanner, Project}
  import org.apache.tools.ant.taskdefs.MatchingTask
  import org.apache.tools.ant.types.Path
  import org.apache.tools.ant.util.{FileUtils, GlobPatternMapper,
                                    SourceFileScanner}
  import org.apache.tools.ant.types.{EnumeratedAttribute, Reference}

  /** An Ant task that generates a SH or BAT script to execute a Scala program.
    * This task can take the following parameters as attributes:<ul>
    *  <li>file (mandatory),</li>
    *  <li>name,</li>
    *  <li>class (mandatory),</li>
    *  <li>platforms,</li>
    *  <li>version,</li>
    *  <li>copyright,</li>
    *  <li>classpath,</li>
    *  <li>properties,</li>
    *  <li>javaflags,</li>
    *  <li>toolflags,</li>
    *  <li>genericfile.</li></ul>
    *
    * @author Gilles Dubochet */
  class ScalaTool extends MatchingTask {

    /** The unique Ant file utilities instance to use in this task. */
    private val fileUtils = FileUtils.newFileUtils()

/******************************************************************************\
**                             Ant user-properties                            **
\******************************************************************************/

    abstract class PermissibleValue {
      val values: List[String]
      def isPermissible(value: String): Boolean =
        (value == "") || values.exists(.startsWith(value))
    }

    /** Defines valid values for the platforms property. */
    object Platforms extends PermissibleValue {
      val values = List("unix", "windows")
    }

    /** The path to the exec script file. ".bat" will be appended for the
      * Windows BAT file, if generated. */
    private var file: Option[File] = None
    /** The name of this tool. By default this is equal to the file name. */
    private var name: Option[String] = None
    /** The main class to run. */
    private var mainClass: Option[String] = None
    /** Supported platforms for the script. Either "unix" or "windows". Defaults
      * to both. */
    private var platforms: List[String] = Nil
    /** The optional version number. If set, when "-version" is passed to the
      * script, this value will be printed. */
    private var version: String = ""
    /** The optional copyright notice, that will be printed in the script. */
    private var copyright: String = "This file is copyrighted by its owner"
    /** An (optional) path to all JARs that this script depend on. Paths must be
      * relative to the scala home directory. If not set, all JAR archives in
      * "lib/" are automatically added. */
    private var classpath: List[String] = Nil
    /** Comma-separated Java system properties to pass to the JRE. Properties
      * are formated as name=value. Properties scala.home and scala.tool.name
      * are always set. */
    private var properties: List[Pair[String,String]] = Nil
    /** Additional flags passed to the JRE ("java [javaFlags] class"). */
    private var javaFlags: String = ""
    /** Additional flags passed to the tool ("java class [toolFlags]"). */
    private var toolFlags: String = ""
    /** The path to the generic runtime script file. ".bat" will be appended for
      * the Windows BAT file, if generated. If not set, no generic runtime
      * script will be generated. */
    private var genericFile: Option[File] = None

/******************************************************************************\
**                             Properties setters                             **
\******************************************************************************/

    /** Sets the file attribute. Used by Ant.
      * @param input The value of <code>file</code>. */
    def setFile(input: File) =
      file = Some(input)

    /** Sets the file attribute. Used by Ant.
      * @param input The value of <code>file</code>. */
    def setName(input: String) =
      name = Some(input)

    /** Sets the main class attribute. Used by Ant.
      * @param input The value of <code>mainClass</code>. */
    def setClass(input: String) =
      mainClass = Some(input)

    /** Sets the platforms attribute. Used by Ant.
      * @param input The value for <code>platforms</code>. */
    def setPlatforms(input: String) = {
      platforms = List.fromArray(input.split(",")).flatMap(s: String => {
        val st = s.trim()
        if (Platforms.isPermissible(st))
          (if (input != "") List(st) else Nil)
        else {
          error("Platform " + st + " does not exist.")
          Nil
        }
      })
    }

    /** Sets the version attribute. Used by Ant.
      * @param input The value of <code>version</code>. */
    def setVersion(input: String) =
      version = input

    /** Sets the copyright attribute. Used by Ant.
      * @param input The value of <code>copyright</code>. */
    def setCopyright(input: String) =
      copyright = input

    /** Sets the classpath attribute. Used by Ant.
      * @param input The value of <code>classpath</code>. */
    def setClasspath(input: String) =
      classpath = classpath :::
	(List.fromArray(input.split(":")).map(p => "#PREFIX#/" + p))

    /** Sets the properties attribute. Used by Ant.
      * @param input The value for <code>properties</code>. */
    def setProperties(input: String) = {
      properties = List.fromArray(input.split(",")).flatMap(s: String => {
        val st = s.trim(); val stArray = st.split("=", 2)
        if (stArray.length == 2) {
          if (input != "") List(Pair(stArray(0), stArray(1))) else Nil
        } else error("Property " + st + " does not conform to specification.")
      })
    }

    /** Sets the version attribute. Used by Ant.
      * @param input The value of <code>version</code>. */
    def setJavaflags(input: String) =
      javaFlags = input

    /** Sets the version attribute. Used by Ant.
      * @param input The value of <code>version</code>. */
    def setToolflags(input: String) =
      toolFlags = input

    /** Sets the version attribute. Used by Ant.
      * @param input The value of <code>version</code>. */
    def setGenericfile(input: File) =
      genericFile = Some(input)

/******************************************************************************\
**                             Properties getters                             **
\******************************************************************************/

    /** Gets the value of the file attribute in a Scala-friendly form.
      * @returns The file as a file. */
    private def getFile: File =
      if (file.isEmpty) error("Member 'file' is empty.")
      else getProject().resolveFile(file.get.toString())

    /** Gets the value of the classpath attribute in a Scala-friendly form.
      * @returns The class path as a list of files. */
    private def getUnixClasspath: String =
      classpath.mkString("", ":", "")

    /** Gets the value of the classpath attribute in a Scala-friendly form.
      * @returns The class path as a list of files. */
    private def getWinClasspath: String =
      classpath.map(.replace('/', '\\')).
                mkString("", ";", "")

    /** Gets the value of the classpath attribute in a Scala-friendly form.
      * @returns The class path as a list of files. */
    private def getProperties: String =
      properties.map({
        case Pair(name,value) => "-D" + name + "=\"" + value + "\""
      }).mkString("", " ", "")

    /** Gets the value of the file attribute in a Scala-friendly form.
      * @returns The file as a file. */
    private def getGenericFile: File =
      if (genericFile.isEmpty) error("Member 'file' is empty.")
      else getProject().resolveFile(genericFile.get.toString())

/******************************************************************************\
**                       Compilation and support methods                      **
\******************************************************************************/

    /** Generates a build error. Error location will be the current task in the
      * ant file.
      * @param message A message describing the error.
      * @throws BuildException A build error exception thrown in every case. */
    private def error(message: String): All =
      throw new BuildException(message, getLocation())

    private def readResource(resource: String,
                             tokens: Map[String, String]
    ): String = {
      val chars = new Iterator[Char] {
        private val stream =
          this.getClass().getClassLoader().getResourceAsStream(resource)
        private def readStream(): Char = stream.read().asInstanceOf[Char]
        private var buf: Char = readStream()
        def hasNext: Boolean = (buf != (-1.).asInstanceOf[Char])
        def next: Char = {
          val bufbuf = buf
          buf = readStream()
          bufbuf
        }
      }
      val builder = new StringBuffer()
      while (chars.hasNext) {
        val char = chars.next
        if (char == '@') {
          var char = chars.next
          val token = new StringBuffer()
          while (chars.hasNext && char != '@') {
            token.append(char)
            char = chars.next
          }
          if (tokens.contains(token.toString()))
            builder.append(tokens(token.toString()))
          else if (token.toString() == "")
            builder.append('@')
          else
            builder.append("@" + token.toString() + "@")
        } else builder.append(char)
      }
      builder.toString()
    }

    private def writeFile(file: File, content: String) =
      if (file.exists() && !file.canWrite())
        error("File " + file + " is not writable")
      else {
        val writer = new FileWriter(file, false)
        writer.write(content)
        writer.close()
      }

    private def expandUnixVar(vars: Map[String,String]): Map[String,String] =
      vars.map((key:String,vari:String) => vari.
        replaceAll("#([^#]*)#", "\\$$1")
      )

    private def expandWinVar(vars: Map[String,String]): Map[String,String] =
      vars.map((key:String,vari:String) => vari.
        replaceAll("#([^#]*)#", "%_$1%")
      )


/******************************************************************************\
**                           The big execute method                           **
\******************************************************************************/

    /** Performs the compilation. */
    override def execute() = {
      // Tests if all mandatory attributes are set and valid.
      if (file.isEmpty) error("Attribute 'file' is not set.")
      if (name.isEmpty) name = Some(file.get.getName())
      if (mainClass.isEmpty) error("Attribute 'mainclass' is not set.")
      if (platforms.isEmpty) platforms = Platforms.values
      // Gets the input streams for the script templates.
      val resourceRoot = "scala/tools/ant/templates/"
      val patches = ListMap.Empty.
        update("name", name.get).
        update("class", mainClass.get).
        update("version", version).
        update("copyright", copyright).
        update("properties", getProperties).
        update("javaflags", javaFlags).
        update("toolflags", toolFlags)
      if (platforms.contains("unix")) {
        val unixPatches = expandUnixVar(
          patches.update("classpath", getUnixClasspath))
        val unixTemplateResource = resourceRoot + "tool-unix.tmpl"
        val unixTemplate = readResource(unixTemplateResource, unixPatches)
        writeFile(getFile, unixTemplate)
        if (!genericFile.isEmpty) {
          val unixTemplateResource = resourceRoot + "generic-unix.tmpl"
          val unixTemplate = readResource(unixTemplateResource, unixPatches)
          writeFile(getGenericFile, unixTemplate)
        }
      }
      if (platforms.contains("windows")) {
        val winPatches = expandWinVar(
          patches.update("classpath", getWinClasspath))
        val winTemplateResource = resourceRoot + "tool-windows.tmpl"
        val winTemplate = readResource(winTemplateResource, winPatches)
        writeFile(new File(getFile.getAbsolutePath() + ".bat"), winTemplate)
        if (!genericFile.isEmpty) {
          val winTemplateResource = resourceRoot + "generic-windows.tmpl"
          val winTemplate = readResource(winTemplateResource, winPatches)
          writeFile(new File(getGenericFile.getAbsolutePath() + ".bat"),
                    winTemplate)
        }
      }
    }

  }

}
