/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{ File }
import java.util.StringTokenizer
import scala.util.Properties.{ setProp }
import scala.tools.util.Signallable
import scala.tools.nsc.util.ScalaClassLoader
import scala.tools.nsc.io.Directory
import scala.collection.{ mutable, immutable }
import scala.actors.Actor._
import scala.actors.TIMEOUT

case class TestRunParams(val scalaCheckParentClassLoader: ScalaClassLoader)

trait DirectRunner {

  def fileManager: FileManager

  import PartestDefaults.numActors

  def setProperties() {
    if (isPartestDebug)
      scala.actors.Debug.level = 3

    if (PartestDefaults.poolSize.isEmpty) {
      scala.actors.Debug.info("actors.corePoolSize not defined")
      setProp("actors.corePoolSize", "16")
    }
  }

  /** These things, formerly inside runTestsForFiles, have been promoted
   *  into private fields so I can inspect them via signal when partest shows
   *  signs of dementia.
   */
  private var workers: List[Worker]       = Nil
  private var logsToDelete: List[LogFile] = Nil
  private var outdirsToDelete: List[File] = Nil
  private val results                     = new mutable.HashMap[String, Int]()
  private def addResults(kvs: Traversable[(String, Int)]) = synchronized { results ++= kvs }
  private val signallable = Signallable("HUP", "Make partest dump its state.")(dumpState())

  def dumpState() {
    println("Dumping partest internals.")
    println("results.size = " + results.size + ", " + workers.size + " workers.")
    workers foreach println
    workers filter (_.currentFileElapsed > 60) foreach { w =>
      val elapsed = w.currentFileElapsed
      println("A worker requires euthanasia! At least so it seems, since I received")
      println("a signal and this one has been in la-la land for " + elapsed + " seconds.")
      println("Attempting to force test timeout.")
      w.forceTimeout()
    }
  }

  def runTestsForFiles(_kindFiles: List[File], kind: String): immutable.Map[String, Int] = {
    /** NO DUPLICATES, or partest will blow the count and hang forever. */
    val kindFiles = _kindFiles.distinct
    val groupSize = (kindFiles.length / numActors) + 1

    val consFM = new ConsoleFileManager
    import consFM.{ latestCompFile, latestLibFile, latestPartestFile }
    val scalacheckURL = PathSettings.scalaCheck.toURL
    val scalaCheckParentClassLoader = ScalaClassLoader.fromURLs(
      List(scalacheckURL, latestCompFile.toURI.toURL, latestLibFile.toURI.toURL, latestPartestFile.toURI.toURL)
    )
    Output.init

    this.workers = kindFiles.grouped(groupSize).toList map { toTest =>
      val worker = new Worker(fileManager, TestRunParams(scalaCheckParentClassLoader))
      worker.start()
      worker ! RunTests(kind, toTest)
      worker
    }

    workers foreach { w =>
      receiveWithin(3600 * 1000) {
        case Results(res, logs, outdirs) =>
          logsToDelete ++= (logs filter (_.toDelete))
          outdirsToDelete ++= outdirs
          addResults(res)
        case TIMEOUT =>
          // add at least one failure
          NestUI.verbose("worker timed out; adding failed test")
          addResults(Seq(("worker timed out; adding failed test" -> 2)))
      }
    }

    if (isPartestDebug)
      fileManager.showTestTimings()
    else {
      for (x <- logsToDelete ++ outdirsToDelete) {
        NestUI.verbose("deleting "+x)
        Directory(x).deleteRecursively()
      }
    }

    results.toMap
  }

}
