/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools
package partest

import nsc.io._

trait Entities {
  self: Universe =>

  abstract class TestEntity extends AbsTestEntity
                               with TestContribution
                               with TestHousekeeping
                               with EntityLogging
                               with CompilableTest
                               with ScriptableTest
                               with DiffableTest {
    def location: Path
    def category: TestCategory

    lazy val label          = location.stripExtension
    lazy val testClasspath  = createClasspathString()

    /** Was this test successful? Calling this for the first time forces
     *  lazy val "process" which actually runs the test.
     */
    def isSuccess = process

    /** Some standard files, which may or may not be present.
     */
    def scalaOptsFile = withExtension("flags").toFile     // opts to scalac
    def javaOptsFile  = withExtension("javaopts").toFile  // opts to java (but not javac)
    def commandFile   = withExtension("cmds").toFile      // sequence of commands to execute
    def logFile       = withExtension("log").toFile       // collected output

    /** Some standard directories.
     */
    def outDir        = withExtension("obj").toDirectory  // output dir, e.g. files/pos/t14.obj
    def categoryDir   = location.parent.normalize         // category dir, e.g. files/pos/
    def sourcesDir    = location ifDirectory (_.normalize) getOrElse categoryDir

    /** Standard arguments for run, exec, diff.
     */
    def argumentsToRun  = List("Test", "jvm")
    def argumentsToExec = List(location.path)
    def argumentsToDiff = ((checkFile, logFile))

    /** Using a .cmds file for a custom test sequence.
     */
    def commandList   = safeLines(commandFile)
    def testSequence  =
      if (commandFile.isFile && commandList.nonEmpty) commandList map customTestStep
      else category.testSequence

    def run()   = runScala(argumentsToRun)
    def exec()  = runExec(argumentsToExec)
    def diff()  = runDiff(argumentsToDiff._1, argumentsToDiff._2)

    /** The memoized result of the test run.
     */
    private lazy val process = {
      def preCheck  = precondition || returning(false)(_ => trace("precondition failed"))
      def allSteps  = testSequence.actions forall (f => f(this))
      val outcome   = runWrappers(preCheck && allSteps)

      // if outcome is empty, the JVM is trying to shut down, so we clam up
      // to avoid echoing lots of spurious failure messages.
      if (outcome.isEmpty && !isShuttingDown) setShuttingDown()
      else outcome getOrElse false
    }
  }

  case class TestDirectory(category: TestCategory, location: Directory) extends TestEntity { }
  case class TestFile(category: TestCategory, location: File) extends TestEntity { }
}
