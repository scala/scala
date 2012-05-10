import sbt._

import Build._
import Keys._
import Project.Initialize
import complete._
import scala.collection.{ mutable, immutable }

/** This object */
object partest {

  /** The key for the run-partest task that exists in Scala's test suite. */
  lazy val runPartest       = TaskKey[Unit]("run-partest", "Runs the partest test suite against the quick.")
  lazy val runPartestSingle = InputKey[Unit]("run-partest-single", "Runs a single partest test against quick.")
  lazy val runPartestFailed = TaskKey[Unit]("run-partest-failed", "Runs failed partest tests.")
  lazy val runPartestGrep   = InputKey[Unit]("run-partest-grep", "Runs a single partest test against quick.")
  lazy val partestRunner    = TaskKey[PartestRunner]("partest-runner", "Creates a runner that can run partest suites")
  lazy val partestTests     = TaskKey[Map[String, Seq[File]]]("partest-tests", "Creates a map of test-type to a sequence of the test files/directoryies to test.")
  lazy val partestDirs      = SettingKey[Map[String,File]]("partest-dirs", "The map of partest test type to directory associated with that test type")

  lazy val partestTaskSettings: Seq[Setting[_]] = Seq(
    javaOptions in partestRunner := Seq("-Xmx512M -Xms256M"),
    partestDirs <<= baseDirectory apply { bd =>
      partestTestTypes map (kind => kind -> (bd / "test" / "files" / kind)) toMap
    },
    partestRunner <<= partestRunnerTask(fullClasspath in Runtime, javaOptions in partestRunner),
    partestTests <<= partestTestsTask(partestDirs),
    runPartest <<= runPartestTask(partestRunner, partestTests, scalacOptions in Test),
    runPartestSingle <<= runSingleTestTask(partestRunner, partestDirs, scalacOptions in Test),
    runPartestFailed <<= runPartestTask(partestRunner, partestTests, scalacOptions in Test, Seq("--failed"))
  )

  // What's fun here is that we want "*.scala" files *and* directories in the base directory...
  def partestResources(base: File, testType: String): PathFinder = testType match {
    case "res"          => base ** "*.res"
    case "buildmanager" => base * "*"
    // TODO - Only allow directories that have "*.scala" children...
    case _              => base * "*" filter { f => !f.getName.endsWith(".obj") && (f.isDirectory || f.getName.endsWith(".scala")) }
  }
  lazy val partestTestTypes = Seq("run", "jvm", "pos", "neg", "buildmanager", "res", "shootout", "scalap", "specialized", "presentation", "scalacheck")

  // TODO - Figure out how to specify only a subset of resources...
  def partestTestsTask(testDirs: ScopedSetting[Map[String,File]]): Project.Initialize[Task[Map[String, Seq[File]]]] =
    testDirs map (m => m map { case (kind, dir) => kind -> partestResources(dir, kind).get })

  // TODO - Split partest task into Configurations and build a Task for each Configuration.
  // *then* mix all of them together for run-testsuite or something clever like this.
  def runPartestTask(runner: ScopedTask[PartestRunner], testRuns: ScopedTask[Map[String,Seq[File]]], scalacOptions: ScopedTask[Seq[String]], extraArgs: Seq[String] = Seq()): Initialize[Task[Unit]] = {
    (runner, testRuns, scalacOptions, streams) map {
      (runner, runs, scalaOpts, s) => runPartestImpl(runner, runs, scalaOpts, s, extraArgs)
    }
  }
  private def runPartestImpl(runner: PartestRunner, runs: Map[String, Seq[File]], scalacOptions: Seq[String], s: TaskStreams, extras: Seq[String] = Seq()): Unit = {
    val testArgs  = runs.toSeq collect { case (kind, files) if files.nonEmpty => Seq("-" + kind, files mkString ",") } flatten
    val extraArgs = scalacOptions flatMap (opt => Seq("-scalacoption", opt))

    import collection.JavaConverters._
    val results = runner run Array(testArgs ++ extraArgs ++ extras: _*) asScala
    // TODO - save results
    val failures = results collect {
      case (path, 1) => path + " [FAILED]"
      case (path, 2) => path + " [TIMEOUT]"
    }

    if (failures.isEmpty)
      s.log.info(""+results.size+" tests passed.")
    else {
      failures foreach (s.log error _)
      error("Test Failures! ("+failures.size+" of "+results.size+")")
    }
  }

  def convertTestsForAutoComplete(tests: Map[String, Seq[File]]): (Set[String], Set[String]) =
    (tests.keys.toSet, tests.values flatMap (_ map cleanFileName) toSet)

  /** Takes a test file, as sent ot Partest, and cleans it up for auto-complete */
  def cleanFileName(file: File): String = {
    // TODO - Something intelligent here
    val TestPattern = ".*/test/(.*)".r
    file.getCanonicalPath match {
      case TestPattern(n) => n
      case _ => file.getName
    }
  }

  // TODO - Allow a filter for the second part of this...
  def runSingleTestParser(testDirs: Map[String, File]): State => Parser[(String, String)] = {
    import DefaultParsers._
    state => {
      Space ~> token(NotSpace examples testDirs.keys.toSet) flatMap { kind =>
        val files: Set[String] = testDirs get kind match {
          case Some(dir) =>
            partestResources(dir, kind).get flatMap (_ relativeTo dir) map (_ getName) toSet
          case _ =>
            Set()
        }
        Space ~> token(NotSpace examples files) map (kind -> _)
      }
    }
  }

  def runSingleTestTask(runner: ScopedTask[PartestRunner], testDirs: ScopedSetting[Map[String, File]], scalacOptions: ScopedTask[Seq[String]]) : Initialize[InputTask[Unit]] = {
    import sbinary.DefaultProtocol._

    InputTask(testDirs apply runSingleTestParser) { result =>
      (runner, result, testDirs, scalacOptions, streams) map {
        case (r, (kind, filter), dirs, o, s) =>
        // TODO - Use partest resources somehow to filter the filter correctly....
        val files: Seq[File] =
          if (filter == "*") partestResources(dirs(kind), kind).get
          else (dirs(kind) * filter).get

        runPartestImpl(r, Map(kind -> files), o, s)
      }
    }
  }

  def partestRunnerTask(classpath: ScopedTask[Classpath], javacOptions: SettingKey[Seq[String]]): Project.Initialize[Task[PartestRunner]] =
   (classpath, javacOptions) map ((cp, opts) => new PartestRunner(Build.data(cp), opts mkString " "))
}

class PartestRunner(classpath: Seq[File], javaOpts: String) {
  // Classloader that does *not* have this as parent, for differing Scala version.
  lazy val classLoader = new java.net.URLClassLoader(classpath.map(_.toURI.toURL).toArray, null)
  lazy val (mainClass, mainMethod) = try {
    val c = classLoader.loadClass("scala.tools.partest.nest.SBTRunner")
    val m = c.getMethod("mainReflect", classOf[Array[String]])
    (c,m)
  }
  lazy val classPathArgs = Seq("-cp", classpath.map(_.getAbsoluteFile).mkString(java.io.File.pathSeparator))
  def run(args: Array[String]): java.util.Map[String,Int] = try {
    // TODO - undo this settings after running.  Also globals are bad.
    System.setProperty("partest.java_opts", javaOpts)
    val allArgs = (classPathArgs ++ args).toArray
    mainMethod.invoke(null, allArgs).asInstanceOf[java.util.Map[String,Int]]
  } catch {
    case e =>
    //error("Could not run Partest: " + e)
    throw e
  }
}
