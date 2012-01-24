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
import scala.actors.Actor._
import scala.actors.TIMEOUT

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

  def setProperties() {
    if (isPartestDebug)
      scala.actors.Debug.level = 3

    if (PartestDefaults.poolSize.isEmpty) {
      scala.actors.Debug.info("actors.corePoolSize not defined")
      setProp("actors.corePoolSize", "12")
    }
  }

  def runTestsForFiles(_kindFiles: List[File], kind: String): immutable.Map[String, Int] = {
    val kindFiles = onlyValidTestPaths(_kindFiles)
    val groupSize = (kindFiles.length / numActors) + 1

    // @partest maintainer: we cannot create a fresh file manager here
    // since the FM must respect --buildpath and --classpath from the command line
    // for example, see how it's done in ReflectiveRunner
    //val consFM = new ConsoleFileManager
    //import consFM.{ latestCompFile, latestLibFile, latestPartestFile }
    val latestCompFile = new File(fileManager.LATEST_COMP);
    val latestLibFile = new File(fileManager.LATEST_LIB);
    val latestPartestFile = new File(fileManager.LATEST_PARTEST);

    val scalacheckURL = PathSettings.scalaCheck.toURL
    val scalaCheckParentClassLoader = ScalaClassLoader.fromURLs(
      List(scalacheckURL, latestCompFile.toURI.toURL, latestLibFile.toURI.toURL, latestPartestFile.toURI.toURL)
    )
    Output.init()

    val workers = kindFiles.grouped(groupSize).toList map { toTest =>
      val worker = new Worker(fileManager, TestRunParams(scalaCheckParentClassLoader))
      worker.start()
      worker ! RunTests(kind, toTest)
      worker
    }

    workers map { w =>
      receiveWithin(3600 * 1000) {
        case Results(testResults) => testResults
        case TIMEOUT =>
          // add at least one failure
          NestUI.verbose("worker timed out; adding failed test")
          Map("worker timed out; adding failed test" -> 2)
      }
    } reduceLeft (_ ++ _)
  }
}
