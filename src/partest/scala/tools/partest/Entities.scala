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
                               with TestAlarms
                               with EntityLogging
                               with CompilableTest
                               with ScriptableTest
                               with DiffableTest {
    def location: Path
    def category: TestCategory

    lazy val label          = location.stripExtension
    lazy val testClasspath  = returning(createClasspathString())(vtrace)

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
      val outcome   = runWrappers(testSequence.actions forall (f => f(this)))

      // an empty outcome means we've been interrupted and are shutting down.
      outcome getOrElse false
    }
  }

  case class TestDirectory(category: TestCategory, location: Directory) extends TestEntity { }
  case class TestFile(category: TestCategory, location: File) extends TestEntity { }
}
