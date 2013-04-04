/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import java.io.File
import scala.util.Properties.setProp
import scala.tools.nsc.util.{ ScalaClassLoader, Exceptional }
import scala.tools.nsc.io.Path
import scala.collection.{ mutable, immutable }
import java.util.concurrent._

case class TestRunParams(val scalaCheckParentClassLoader: ScalaClassLoader)

trait DirectRunner {
  def fileManager: FileManager

  import PartestDefaults.numThreads

  Thread.setDefaultUncaughtExceptionHandler(
    new Thread.UncaughtExceptionHandler {
      def uncaughtException(thread: Thread, t: Throwable) {
        val t1 = Exceptional unwrap t
        System.err.println(s"Uncaught exception on thread $thread: $t1")
        t1.printStackTrace()
      }
    }
  )
  def runTestsForFiles(kindFiles: List[File], kind: String): List[TestState] = {

    NestUI.resetTestNumber()

    val allUrls           = PathSettings.scalaCheck.toURL :: fileManager.latestUrls
    val parentClassLoader = ScalaClassLoader fromURLs allUrls
    val pool              = Executors newFixedThreadPool numThreads
    val manager           = new RunnerManager(kind, fileManager, TestRunParams(parentClassLoader))
    val futures           = kindFiles map (f => pool submit callable(manager runTest f))

    pool.shutdown()
    try if (!pool.awaitTermination(4, TimeUnit.HOURS))
      NestUI warning "Thread pool timeout elapsed before all tests were complete!"
    catch { case t: InterruptedException =>
      NestUI warning "Thread pool was interrupted"
      t.printStackTrace()
    }

    futures map (_.get)
  }
}
