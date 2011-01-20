/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
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
import scala.tools.nsc.io.Path
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
