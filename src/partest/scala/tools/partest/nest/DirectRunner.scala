/* NEST (New Scala Test)
 * Copyright 2007-2008 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id: $

package scala.tools.partest.nest

import java.io.{File, PrintStream, FileOutputStream, BufferedReader,
                InputStreamReader, StringWriter, PrintWriter}
import java.util.StringTokenizer

import scala.actors.Actor._

trait DirectRunner {

  def fileManager: FileManager

  protected val numActors = Integer.parseInt(System.getProperty("scalatest.actors", "8"))

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
      receive {
        case Results(s, f, logs, outdirs) =>
          logsToDelete = logsToDelete ::: logs.filter(_.toDelete)
          outdirsToDelete = outdirsToDelete ::: outdirs
          succs += s
          fails += f
      }
    }
    logsToDelete.foreach { log =>
      NestUI.verbose("deleting "+log+"\n")
      fileManager.deleteRecursive(log)
    }
    outdirsToDelete.foreach { outdir =>
      NestUI.verbose("deleting "+outdir+"\n")
      fileManager.deleteRecursive(outdir)
    }

    (succs, fails)
  }

}
