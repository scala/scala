package scala.tools.nsc
package interactive
package tests

import java.io.File.pathSeparatorChar
import java.io.File.separatorChar
import scala.tools.nsc.interactive.tests.core.PresentationCompilerInstance
import scala.tools.nsc.io.{File,Path}
import core.Reporter
import core.TestSettings

trait InteractiveTestSettings extends TestSettings with PresentationCompilerInstance {
  /** Character delimiter for comments in .opts file */
  private final val CommentStartDelimiter = "#"

  private final val TestOptionsFileExtension = "flags"

  /** Prepare the settings object. Load the .opts file and adjust all paths from the
   *  Unix-like syntax to the platform specific syntax. This is necessary so that a
   *  single .opts file can be used on all platforms.
   *
   *  @note Bootclasspath is treated specially. If there is a -bootclasspath option in
   *        the file, the 'usejavacp' setting is set to false. This ensures that the
   *        bootclasspath takes precedence over the scala-library used to run the current
   *        test.
   */
  override protected def prepareSettings(settings: Settings) {
    def adjustPaths(paths: settings.PathSetting*) {
      for (p <- paths if argsString.contains(p.name)) p.value = p.value.map {
        case '/' => separatorChar
        case ':' => pathSeparatorChar
        case c   => c
      }
    }

    // need this so that the classpath comes from what partest
    // instead of scala.home
    settings.usejavacp.value = !argsString.contains("-bootclasspath")

    // pass any options coming from outside
    settings.processArgumentString(argsString) match {
      case (false, rest) =>
        println("error processing arguments (unprocessed: %s)".format(rest))
      case _ => ()
    }

    // Make the --sourcepath path provided in the .flags file (if any) relative to the test's base directory
    if(settings.sourcepath.isSetByUser)
      settings.sourcepath.value = (baseDir / Path(settings.sourcepath.value)).path

    adjustPaths(settings.bootclasspath, settings.classpath, settings.javabootclasspath, settings.sourcepath)
  }

  /** If there's a file ending in .opts, read it and parse it for cmd line arguments. */
  protected val argsString = {
    val optsFile = outDir / "%s.%s".format(System.getProperty("partest.testname"), TestOptionsFileExtension)
    val str = try File(optsFile).slurp() catch {
      case e: java.io.IOException => ""
    }
    str.lines.filter(!_.startsWith(CommentStartDelimiter)).mkString(" ")
  }

  override protected def printClassPath(implicit reporter: Reporter) {
    reporter.println("\toutDir: %s".format(outDir.path))
    reporter.println("\tbaseDir: %s".format(baseDir.path))
    reporter.println("\targsString: %s".format(argsString))
    super.printClassPath(reporter)
  }
}
