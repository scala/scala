import sbt._
import org.apache.commons.lang3.SystemUtils
import org.apache.commons.lang3.StringUtils.replaceEach

/**
 * A class that generates a shell or batch script to execute a Scala program.
 *
 * This is a simplified copy of Ant task (see scala.tools.ant.ScalaTool).
 */
case class ScalaTool(mainClass: String,
                     classpath: List[String],
                     properties: Map[String, String],
                     javaOpts: String,
                     toolFlags: String) {
  //  For classpath, the platform specific
  //  demarcation of any script variables (e.g. `${SCALA_HOME}` or
  //  `%SCALA_HOME%`) can be specified in a platform independent way (e.g.
  //  `@SCALA_HOME@`) and automatically translated for you.
  def patchedToolScript(template: String, forWindows: Boolean) = {
    val varRegex = """@(\w+)@""" // the group should be able to capture each of the keys of the map below
    val platformClasspath =
      if(forWindows) classpath.mkString(";").replace('/', '\\').replaceAll(varRegex, "%$1%")
      else if(SystemUtils.IS_OS_WINDOWS) {
        // When building on Windows, use a Windows classpath in the shell script (for MSYS/Cygwin).
        // This is only used for "quick", which uses absolute paths, so it is not portable anyway.
        classpath.mkString(";").replace("\\", "\\\\").replaceAll(varRegex, """\${$1}""")
      } else classpath.mkString(":").replace('\\', '/').replaceAll(varRegex, """\${$1}""")

    val variables = Map(
      ("@@"           -> "@"), // for backwards compatibility
      ("@class@"      -> mainClass),
      ("@properties@" -> (properties map { case (k, v) => s"""-D$k="$v""""} mkString " ")),
      ("@javaflags@"  -> javaOpts),
      ("@toolflags@"  -> toolFlags),
      ("@classpath@"  -> platformClasspath)
    )

    val (from, to) = variables.unzip
    replaceEach(template, from.toArray, to.toArray)
  }

  def writeScript(file: String, platform: String, rootDir: File, outDir: File): File = {
    val forWindows   = platform match { case "windows" => true case _ => false }
    val templatePath = s"scala/tools/ant/templates/tool-$platform.tmpl"
    val suffix       = if(forWindows) ".bat" else ""
    val scriptFile   = outDir / s"$file$suffix"
    val patched      = patchedToolScript(IO.read(rootDir / templatePath).replace("\r", ""), forWindows)
    IO.write(scriptFile, if(forWindows) patched.replace("\n", "\r\n") else patched)
    scriptFile
  }
}
