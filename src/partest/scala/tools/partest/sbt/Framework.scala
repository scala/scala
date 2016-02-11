package scala.tools.partest.sbt

import scala.language.reflectiveCalls

import _root_.sbt.testing._
import java.net.URLClassLoader
import java.io.File

object Framework {
  // as partest is not driven by test classes discovered by sbt, need to add this marker fingerprint to definedTests
  val fingerprint = new AnnotatedFingerprint { def isModule = true; def annotationName = "partest" }

  // TODO how can we export `fingerprint` so that a user can just add this to their build.sbt
  // definedTests in Test += new sbt.TestDefinition("partest", fingerprint, true, Array())
}
class Framework extends sbt.testing.Framework {
  def fingerprints: Array[Fingerprint] = Array(Framework.fingerprint)
  def name: String = "partest"

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): sbt.testing.Runner =
    new Runner(args, remoteArgs, testClassLoader)
}

/** Represents one run of a suite of tests.
  */
case class Runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader) extends sbt.testing.Runner {

  def tasks(taskDefs: Array[TaskDef]): Array[sbt.testing.Task] = taskDefs map (SbtPartestTask(_, args): sbt.testing.Task)

  /** Indicates the client is done with this <code>Runner</code> instance.
    *
    *  @return a possibly multi-line summary string, or the empty string if no summary is provided -- TODO
    */
  def done(): String = ""
}

/** Run partest in this VM. Assumes we're running in a forked VM!
  */
case class SbtPartestTask(taskDef: TaskDef, args: Array[String]) extends Task {
  /** Executes this task, possibly returning to the client new tasks to execute. */
  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val forkedCp    = scala.util.Properties.javaClassPath
    val classLoader = new URLClassLoader(forkedCp.split(java.io.File.pathSeparator).map(new File(_).toURI.toURL))
    val runner      = SBTRunner(Framework.fingerprint, eventHandler, loggers, "files", classLoader, null, null, Array.empty[String], args)

    if (Runtime.getRuntime().maxMemory() / (1024*1024) < 800)
      loggers foreach (_.warn(s"""Low heap size detected (~ ${Runtime.getRuntime().maxMemory() / (1024*1024)}M). Please add the following to your build.sbt: javaOptions in Test += "-Xmx1G""""))

    try runner.run
    catch {
      case ex: ClassNotFoundException =>
        loggers foreach { l => l.error("Please make sure partest is running in a forked VM by including the following line in build.sbt:\nfork in Test := true") }
        throw ex
    }

    Array()
  }

  type SBTRunner = { def run(): Unit }

  // use reflection to instantiate scala.tools.partest.nest.SBTRunner,
  // casting to the structural type SBTRunner above so that method calls on the result will be invoked reflectively as well
  private def SBTRunner(partestFingerprint: Fingerprint, eventHandler: EventHandler, loggers: Array[Logger], srcDir: String, testClassLoader: URLClassLoader, javaCmd: File, javacCmd: File, scalacArgs: Array[String], args: Array[String]): SBTRunner = {
    val runnerClass = Class.forName("scala.tools.partest.nest.SBTRunner")
    runnerClass.getConstructors()(0).newInstance(partestFingerprint, eventHandler, loggers, srcDir, testClassLoader, javaCmd, javacCmd, scalacArgs, args).asInstanceOf[SBTRunner]
  }

  /** A possibly zero-length array of string tags associated with this task. */
  def tags: Array[String] = Array()
}
