/* NEST (New Scala Test)
 * Copyright 2007-2009 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{File, PrintStream, FileOutputStream, BufferedReader,
                InputStreamReader, StringWriter, PrintWriter}
import java.util.StringTokenizer

import scala.actors.Actor._
import scala.actors.TIMEOUT

trait DirectRunner {

  def fileManager: FileManager

  private val numActors = Integer.parseInt(System.getProperty("scalatest.actors", "8"))

  if ((System.getProperty("partest.debug", "false") equals "true") ||
      (System.getProperty("scalatest.debug", "false") equals "true"))
    scala.actors.Debug.level = 3

  private val coreProp = try {
    System.getProperty("actors.corePoolSize")
  } catch {
    case ace: java.security.AccessControlException =>
      null
  }
  if (coreProp == null) {
    scala.actors.Debug.info("actors.corePoolSize not defined")
    System.setProperty("actors.corePoolSize", "16")
  }

  def runTestsForFiles(kindFiles: List[File], kind: String): (Int, Int) = {
    val len = kindFiles.length
    val (testsEach, lastFrag) = (len/numActors, len%numActors)
    val last = numActors-1
    val workers = for (i <- List.range(0, numActors)) yield {
      val toTest = kindFiles.slice(i*testsEach, (i+1)*testsEach)
      val worker = new Worker(fileManager)
      worker.start()
      if (i == last)
        worker ! RunTests(kind, (kindFiles splitAt (last*testsEach))._2)
      else
        worker ! RunTests(kind, toTest)
      worker
    }
    var succs = 0; var fails = 0
    var logsToDelete: List[File] = List()
    var outdirsToDelete: List[File] = List()
    workers foreach { w =>
      receiveWithin(600 * 1000) {
        case Results(s, f, logs, outdirs) =>
          logsToDelete = logsToDelete ::: logs.filter(_.toDelete)
          outdirsToDelete = outdirsToDelete ::: outdirs
          succs += s
          fails += f
        case TIMEOUT =>
          // add at least one failure
          fails += 1
      }
    }
    logsToDelete.foreach { log =>
      NestUI.verbose("deleting "+log)
      fileManager.deleteRecursive(log)
    }
    outdirsToDelete.foreach { outdir =>
      NestUI.verbose("deleting "+outdir)
      fileManager.deleteRecursive(outdir)
    }

    (succs, fails)
  }

}
