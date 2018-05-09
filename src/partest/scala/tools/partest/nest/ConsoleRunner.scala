/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

class ConsoleRunner(val config: RunnerSpec.Config) extends AbstractRunner {
  val suiteRunner = new SuiteRunner (
    testSourcePath = config.optSourcePath getOrElse PartestDefaults.sourcePath,
    fileManager = new FileManager(ClassPath split PathResolver.Environment.javaUserClassPath map (Path(_))), // the script sets up our classpath for us via ant
    updateCheck = config.optUpdateCheck,
    failed = config.optFailed,
    nestUI = nestUI)
}

object ConsoleRunner {
  def main(args: Array[String]): Unit = {
    val r = new ConsoleRunner(RunnerSpec.forArgs(args))
    // So we can ctrl-C a test run and still hear all
    // the buffered failure info.
    scala.sys addShutdownHook r.issueSummaryReport()
    System.exit( if (r.run) 0 else 1 )
  }
}
