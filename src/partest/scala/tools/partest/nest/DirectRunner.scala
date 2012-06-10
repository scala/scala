/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
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

  import PartestDefaults.numActors

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

  private def timeoutResult = {
    NestUI.verbose("worker timed out; adding failed test")
    Map("worker timed out; adding failed test" -> TestState.Timeout)
  }
  def runTestsForFiles(_kindFiles: List[File], kind: String): immutable.Map[String, TestState] = {
    val kindFiles = onlyValidTestPaths(_kindFiles)
    val groupSize = (kindFiles.length / numActors) + 1

    // @partest maintainer: we cannot create a fresh file manager here
    // since the FM must respect --buildpath and --classpath from the command line
    // for example, see how it's done in ReflectiveRunner
    //val consFM = new ConsoleFileManager
    //import consFM.{ latestCompFile, latestLibFile, latestPartestFile }
    val latestCompFile = new File(fileManager.LATEST_COMP)
    val latestReflectFile = new File(fileManager.LATEST_REFLECT)
    val latestLibFile = new File(fileManager.LATEST_LIB)
    val latestPartestFile = new File(fileManager.LATEST_PARTEST)
    val latestActorsFile = new File(fileManager.LATEST_ACTORS)
    val latestActMigFile = new File(fileManager.LATEST_ACTORS_MIGRATION)
    val scalacheckURL = PathSettings.scalaCheck.toURL
    val scalaCheckParentClassLoader = ScalaClassLoader.fromURLs(
      scalacheckURL :: (List(latestCompFile, latestReflectFile, latestLibFile, latestActorsFile, latestActMigFile, latestPartestFile).map(_.toURI.toURL))
    )
    Output.init()

    val pool      = Executors.newCachedThreadPool()
    val fileSets  = kindFiles grouped groupSize toList;
    val workers   = fileSets map (_ => new Worker(kind, fileManager, TestRunParams(scalaCheckParentClassLoader)))
    val callables = (fileSets, workers).zipped map ((xs, w) => callable(w actDirect xs))
    val results   = pool.invokeAll(callables.asJava, 1, TimeUnit.HOURS).asScala

    results flatMap { r =>
      if (r.isCancelled) timeoutResult
      else for ((file, status) <- r.get) yield (file.getAbsolutePath, status)
    } toMap
  }
}
