/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools
package partest
package nest

class ConsoleRunner(argstr: String) extends AbstractRunner(argstr) {

  override val suiteRunner = new SuiteRunner (
    testSourcePath = optSourcePath getOrElse PartestDefaults.sourcePath,
    fileManager = new FileManager(ClassPath split PathResolver.Environment.javaUserClassPath map (Path(_))), // the script sets up our classpath for us via ant
    updateCheck = optUpdateCheck,
    failed = optFailed)

  // So we can ctrl-C a test run and still hear all
  // the buffered failure info.
  scala.sys addShutdownHook issueSummaryReport()

  override def run(): Unit = {
    super.run()
    System exit ( if (isSuccess) 0 else 1 )
  }

  run()
}

object ConsoleRunner {
  def main(args: Array[String]): Unit = {
    new ConsoleRunner(args mkString " ")
  }
}

