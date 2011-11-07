import java.io.{FileInputStream, File, InputStream, FileWriter}
import sbt._
import scala.io._

/**
 * Create the scala binaries
 * Based on scala.tools.ant.ScalaTool
 * @author Grégory Moix (for the sbt adaptation)
 */
trait ScalaTools {
  self: BasicLayer =>

  lazy val templatesLocation = compilerConfig.srcDir/ "scala" / "tools" / "ant" / "templates"
  lazy val unixTemplate = templatesLocation / "tool-unix.tmpl"
  lazy val winTemplate = templatesLocation / "tool-windows.tmpl"


  // XXX encoding and generalize
  private def getResourceAsCharStream(resource: Path): Stream[Char] = {
    val stream = new FileInputStream(resource.asFile)
    def streamReader(): Stream[Char] = stream.read match {
      case -1 => Stream.empty
      case value => Stream.cons(value.asInstanceOf[Char], streamReader())

    }
    if (stream == null) {
      log.debug("Stream was null")
      Stream.empty
    }

    //else Stream continually stream.read() takeWhile (_ != -1) map (_.asInstanceOf[Char])  // Does not work in scala 2.7.7
    else streamReader
  }


  // Converts a variable like @SCALA_HOME@ to ${SCALA_HOME} when pre = "${" and post = "}"
  private def transposeVariableMarkup(text: String, pre: String, post: String) : String = {
    val chars = Source.fromString(text)
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
        if (token.toString == "")
          builder.append('@')
        else
          builder.append(pre + token.toString + post)
      } else builder.append(char)
    }
    builder.toString
  }

  private def readAndPatchResource(resource: Path, tokens: Map[String, String]): String = {
    val chars = getResourceAsCharStream(resource).elements
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

  private def writeFile(file: File, content: String, makeExecutable: Boolean): Option[String] =
    if (file.exists() && !file.canWrite())
      Some("File " + file + " is not writable")
    else {
      val writer = new FileWriter(file, false)
      writer.write(content)
      writer.close()
      file.setExecutable(makeExecutable)
      None
    }

  /** Gets the value of the classpath attribute in a Scala-friendly form.
   * @return The class path as a list of files. */
  private def getUnixclasspath(classpath: List[String]): String =
    transposeVariableMarkup(classpath.mkString("", ":", "").replace('\\', '/'), "${", "}")

  /** Gets the value of the classpath attribute in a Scala-friendly form.
   * @return The class path as a list of files. */
  private def getWinclasspath(classpath: List[String]): String =
    transposeVariableMarkup(classpath.mkString("", ";", "").replace('/', '\\'), "%", "%")

  /** Performs the tool creation of a tool with for a given os
   * @param file
   * @param mainClas
   * @param properties
   * @param javaFlags
   * @param toolFlags
   * @param classPath
   * @param template
   * @param classpathFormater
   */
  private def tool(template: Path, classpathFormater: List[String] => String, file: Path, mainClass: String,
                   properties: String, javaFlags: String, toolFlags: String, classPath: List[Path], makeExecutable: Boolean): Option[String] = {
    val patches = Map (
      ("class", mainClass),
      ("properties", properties),
      ("javaflags", javaFlags),
      ("toolflags", toolFlags),
      ("classpath", classpathFormater(classPath.map(_.absolutePath)))
      )

    val result = readAndPatchResource(template, patches)
    writeFile(file.asFile, result, makeExecutable)

  }
  private def generateTool(config: ToolConfiguration): Option[String] =
    generateTool(config.toolName, config.destination, config.mainClass, config.properties, config.javaFlags, config.toolFlags, config.classPath)

  private def generateTool(toolName: String, destination: Path, mainClass: String, properties: String, javaFlags: String, toolFlags: String, classPath: List[Path]): Option[String] ={
    val unixFile = destination / toolName
    val winFile = destination /(toolName + ".bat")
    tool(unixTemplate, getUnixclasspath, unixFile, mainClass, properties, javaFlags, toolFlags, classPath, true) orElse
            tool(winTemplate, getWinclasspath, winFile, mainClass, properties, javaFlags, toolFlags, classPath, false)
  }


  /*============================================================================*\
  **                    Definition of the different tools                       **
  \*============================================================================*/
  private val defaultJavaFlags = "-Xmx256M -Xms32M"

  /**
   * A class that holds the different parameters of a tool
   */
  class ToolConfiguration(val toolName: String, val destination: Path, val mainClass: String, val properties: String, val javaFlags: String, val toolFlags: String, val classPath: List[Path])

  /**
   * Generate all tools
   * @param destination Root folder where all the binaries will be written
   * @param classpath Should be specified when you want to use a specific classpath, could be Nil if you want
   * to make the bin use what is in the lib folder of the distribution.
   */
  def tools(destination: Path, classpath: List[Path]) = task {
    val scala = new ToolConfiguration("scala", destination, "scala.tools.nsc.MainGenericRunner", "",defaultJavaFlags, "", classpath)
    val scalac = new ToolConfiguration("scalac", destination, "scala.tools.nsc.Main", "",defaultJavaFlags, "", classpath)
    val scaladoc = new ToolConfiguration("scaladoc",destination,"scala.tools.nsc.ScalaDoc", "",defaultJavaFlags,"", classpath)
    val fsc = new ToolConfiguration("fsc", destination,"scala.tools.nsc.CompileClient", "",defaultJavaFlags, "", classpath)
    val scalap = new ToolConfiguration("scalap",destination, "scala.tools.scalap.Main", "",defaultJavaFlags, "", classpath)

    
    val toolList = scala :: scalac :: scaladoc :: fsc :: scalap :: Nil

    def process(list: List[ToolConfiguration]): Option[String] = list match {
      case x :: xs => {
        log.debug("Generating "+x.toolName+" bin")
        generateTool(x) orElse process(xs)
      }
      case Nil => None

    }
    FileUtilities.createDirectory(destination, log)
    process(toolList)

  }
}


