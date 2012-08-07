/* NEST (New Scala Test)
 * Copyright 2007-2012 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.File
import scala.util.Properties.setProp
import scala.tools.nsc.util.ScalaClassLoader
import scala.tools.nsc.io.Path
import scala.collection.{ mutable, immutable }
import java.util.concurrent._
import scala.collection.convert.decorateAll._

case class TestRunParams(val scalaCheckParentClassLoader: ScalaClassLoader)

trait DirectRunner {
  def fileManager: FileManager

  import PartestDefaults.numThreads

  def denotesTestFile(arg: String) = Path(arg).hasExtension("scala", "res", "xml")
  def denotesTestDir(arg: String)  = Path(arg).ifDirectory(_.files.nonEmpty) exists (x => x)
  def denotesTestPath(arg: String) = denotesTestDir(arg) || denotesTestFile(arg)

  /** No duplicate, no empty directories, don't mess with this unless
   *  you like partest hangs.
   */
  def onlyValidTestPaths[T](args: List[T]): List[T] = {
    args.distinct filter (arg => denotesTestPath("" + arg) || {
      NestUI.warning("Discarding invalid test path '%s'\n" format arg)
      false
    })
  }
  def runTestsForFiles(_kindFiles: List[File], kind: String): immutable.Map[String, TestState] = {
    // @partest maintainer: we cannot create a fresh file manager here
    // since the FM must respect --buildpath and --classpath from the command line
    // for example, see how it's done in ReflectiveRunner
    //val consFM = new ConsoleFileManager
    //import consFM.{ latestCompFile, latestLibFile, latestPartestFile }
    val latestCompFile    = new File(fileManager.LATEST_COMP)
    val latestReflectFile = new File(fileManager.LATEST_REFLECT)
    val latestLibFile     = new File(fileManager.LATEST_LIB)
    val latestPartestFile = new File(fileManager.LATEST_PARTEST)
    val latestActorsFile  = new File(fileManager.LATEST_ACTORS)
    val latestActMigFile  = new File(fileManager.LATEST_ACTORS_MIGRATION)
    val scalacheckURL     = PathSettings.scalaCheck.toURL
    val scalaCheckParentClassLoader = ScalaClassLoader.fromURLs(
      scalacheckURL :: (List(latestCompFile, latestReflectFile, latestLibFile, latestActorsFile, latestActMigFile, latestPartestFile).map(_.toURI.toURL))
    )

    val kindFiles = onlyValidTestPaths(_kindFiles)
    val pool      = Executors.newFixedThreadPool(numThreads)
    val manager   = new RunnerManager(kind, fileManager, TestRunParams(scalaCheckParentClassLoader))
    val futures   = kindFiles map (f => (f, pool submit callable(manager runTest f))) toMap

    pool.shutdown()
    try if (!pool.awaitTermination(4, TimeUnit.HOURS))
      NestUI.warning("Thread pool timeout elapsed before all tests were complete!")
    catch { case t: InterruptedException =>
      NestUI.warning("Thread pool was interrupted")
      t.printStackTrace()
    }

    for ((file, future) <- futures) yield {
      val state = if (future.isCancelled) TestState.Timeout else future.get
      (file.getAbsolutePath, state)
    }
  }
}
