package scala.tools
package partest

import scala.tools.nsc._
import org.scalacheck._
import org.scalacheck.Test._
import scala.collection.{ mutable, immutable }
import scala.tools.nsc.io.Directory

object RunScalacheck {
  class Reporter(val verbosity: Int) extends TestCallback {
    import Pretty._
    private val prettyPrms = Params(verbosity)

    override def onTestResult(name: String, res: Test.Result): Unit = {
      if (res.passed && verbosity == 0) ()
      else failureReason(currentTest) = pretty(res, prettyPrms)
    }
  }

  val failureReason      = mutable.Map[String, String]() withDefaultValue ""
  val uncaughtExceptions = mutable.Map[String, (Throwable, Int)]()
  val pwd                = scala.util.Properties.userDir + java.io.File.separator
  val outdir             = Directory.makeTemp("partest")
  val isDebug            = sys.props contains "partest.debug"
  val verbosity          = if (isDebug) 1 else 0
  val reporter           = new Reporter(verbosity)
  val timings            = mutable.HashMap[String, Long]()
  val settings           = new Settings(msg => uncaughtExceptions(msg) = ((new Throwable, 0)))

  settings.outdir.value = outdir.path
  settings.classpath.value = outdir.path
  settings.nowarn.value = true

  var currentGlobal: Global = _
  var currentTest: String = _
  var testsCompletedWithCurrentGlobal: Int = 0

  resetGlobal()

  def newLoader() = {
    new java.net.URLClassLoader(Array(outdir.toURI.toURL), getClass.getClassLoader)
  }

  def newGlobal() = {
    val global = new Global(settings)
    new global.Run
    global
  }
  def resetGlobal() {
    currentGlobal = newGlobal()
    testsCompletedWithCurrentGlobal = 0
  }

  def reportExceptions() {
    if (uncaughtExceptions.isEmpty) return

    log("There were %d uncaught exceptions during the test run.".format(uncaughtExceptions.size))
    // uncaughtExceptions foreach { case (file, (ex, num)) =>
    //   log("*** Caught during test %s (global had passed %d tests)".format(file, num))
    //   ex.printStackTrace()
    //   log("\n")
    // }
  }

  def debuglog(msg: => String) = {
    if (isDebug)
      Console.err.println(msg)
  }
  def log(msg: String) = {
    Console.err.println(msg)
  }

  def singletonInstance(cl: ClassLoader, className: String): AnyRef = {
    val name = if (className endsWith "$") className else className + "$"
    val clazz = java.lang.Class.forName(name, true, cl)
    val singleton = clazz getField "MODULE$" get null
    singleton
  }

  def withFreshOutdir[T](body: => T): T = {
    if (outdir.exists)
      outdir.deleteRecursively()
    outdir.createDirectory()
    body
  }

  private def handler(file: String, isCompiler: Boolean): PartialFunction[Throwable, Boolean] = {
    case t: Throwable =>
      uncaughtExceptions(file) = ((t, testsCompletedWithCurrentGlobal))
      isCompiler && (testsCompletedWithCurrentGlobal > 0) && {
        resetGlobal()
        runOne(file)
      }
  }

  def runOne(file: String): Boolean = {
    val g = currentGlobal

    def compileTest() = {
      g.reporter.reset
      g invalidateClassPathEntries outdir.path
      val f  = new java.io.File(file)
      val fs = if (f.isDirectory) f.listFiles.toList.map(_.getAbsoluteFile) else List(f)
      new g.Run compile fs.map(_.getPath)
      !g.reporter.hasErrors
    }
    def runTest() = {
      val loader = newLoader()
      val prop   = singletonInstance(loader, "Test")
      val params = Params(testCallback = reporter)
      debuglog("Running Test for " + file + " with (loader, prop, params) = " + ((loader, prop, params)))

      prop match {
        case x: org.scalacheck.Prop => check(params, x).passed
        case x                      =>
          debuglog("Warning: %s is not a Prop".format(file))
          x.asInstanceOf[{ def main(args: Array[String]) }].main(Array[String]())
          true
      }
    }

    def compile = try compileTest() catch handler(file, isCompiler = true)
    def execute = try runTest() catch handler(file, isCompiler = false)
    def finish  = { testsCompletedWithCurrentGlobal += 1 ; true }

    val start = System.nanoTime
    try withFreshOutdir(compile && execute && finish)
    finally timings(file) = System.nanoTime - start
  }

  def process(args: Array[String]): Boolean = {
    val results = scala.util.Random.shuffle(args.toList) map { f =>
      currentTest = f
      debuglog("Testing %s..." format f)
      val ok = try runOne(f) catch handler(f, isCompiler = true)
      val note = (
        if (ok) "PASSED  (%.2f s)".format(timings(f) / 1e9)
        else "FAILED  (%s)".format(failureReason(f))
      )
      println("%60s:  %s".format(f stripPrefix pwd, note))
      (f, ok)
    }

    val (passed, failed) = results partition (_._2)

    if (failed.isEmpty)
      println("All %d tests passed.".format(results.size))
    else {
      val fs = failed map (_._1 split java.io.File.separator last)
      println("%d/%d tests failed: %s".format(failed.size, results.size, fs.mkString(", ")))
      reportExceptions()
    }

    failed.isEmpty
  }

  def main(args: Array[String]): Unit = {
    if (process(args)) sys.exit(0)
    else sys.exit(1)
  }
}
