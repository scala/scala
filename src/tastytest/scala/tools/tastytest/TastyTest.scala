package scala.tools.tastytest

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.tools.nsc
import scala.util.{ Try, Success, Failure }

import dotty.tools.dotc
import dotc.reporting.{ Reporter => DottyReporter }

import java.nio.file.{ Files => JFiles, Paths => JPaths, Path => JPath }
import java.io.ByteArrayOutputStream
import java.{ lang => jl, util => ju }
import jl.reflect.Modifier

import CommandLineParsers._
import SourceKind._
import Files._

object TastyTest {

  def tastytest(dottyLibrary: String, srcRoot: String, pkgName: String, run: Boolean, neg: Boolean, outDir: Option[String]): Try[Unit] = {
    val results = Map(
      "run" -> Tests.suite("run", run)(runSuite(dottyLibrary, srcRoot, pkgName, outDir)),
      "neg" -> Tests.suite("neg", neg)(negSuite(dottyLibrary, srcRoot, pkgName, outDir))
    )
    if (results.values.forall(_.isEmpty)) {
      printwarnln("No suites to run.")
    }
    successWhen(results.values.forall(_.getOrElse(true)))({
      val failures = results.filter(_._2.exists(!_))
      val str = if (failures.size == 1) "suite" else "suites"
      s"${failures.size} $str failed: ${failures.map(_._1).mkString(", ")}."
    })
  }

  def runSuite(dottyLibrary: String, srcRoot: String, pkgName: String, outDir: Option[String]): Try[Unit] = for {
    (pre, src2, src3) <- getSources(srcRoot/"run")
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- scalacPos(out, dottyLibrary, pre:_*)
    _                 <- dotcPos(out, dottyLibrary, src3:_*)
    _                 <- scalacPos(out, dottyLibrary, src2:_*)
    testNames         <- visibleClasses(out, pkgName, src2:_*)
    _                 <- runMainOn(out, dottyLibrary, testNames:_*)
  } yield ()

  def negSuite(dottyLibrary: String, srcRoot: String, pkgName: String, outDir: Option[String]): Try[Unit] = for {
    (pre, src2, src3) <- getSources(srcRoot/"neg", src2Filters = Set(Scala, Check))
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- scalacPos(out, dottyLibrary, pre:_*)
    _                 <- dotcPos(out, dottyLibrary, src3:_*)
    _                 <- scalacNeg(out, dottyLibrary, src2:_*)
  } yield ()

  private def scalacPos(out: String, dottyLibrary: String, sources: String*): Try[Unit] =
    successWhen(scalac(out, dottyLibrary, sources:_*))("scalac failed to compile sources.")

  private def scalacNeg(out: String, dottyLibrary: String, files: String*): Try[Unit] = {
    val errors = mutable.ArrayBuffer.empty[String]
    val unexpectedFail = mutable.ArrayBuffer.empty[String]
    val failMap = {
      val (sources, rest) = files.partition(ScalaFail.filter)
      sources.map({ s =>
        val check = s.stripSuffix(ScalaFail.name) + ".check"
        s -> rest.find(_ == check)
      }).toMap
    }
    if (failMap.isEmpty) {
      printwarnln(s"Warning: there are no source files marked as fail tests. (**/*${ScalaFail.name})")
    }
    for (source <- files.filter(_.endsWith(".scala"))) {
      val buf = new StringBuilder(50)
      val compiled = {
        val byteArrayStream = new ByteArrayOutputStream(50)
        try {
          val compiled = Console.withErr(byteArrayStream) {
            Console.withOut(byteArrayStream) {
              scalac(out, dottyLibrary, source)
            }
          }
          byteArrayStream.flush()
          buf.append(byteArrayStream.toString)
          compiled
        }
        finally {
          byteArrayStream.close()
        }
      }
      if (compiled) {
        if (failMap.contains(source)) {
          errors += source
          printerrln(s"ERROR: $source successfully compiled.")
        }
      }
      else {
        val output = buf.toString
        failMap.get(source) match {
          case None =>
            unexpectedFail += source
            System.err.println(output)
            printerrln(s"ERROR: $source did not compile when expected to. Perhaps it should match (**/*${ScalaFail.name})")
          case Some(checkFileOpt) =>
            checkFileOpt match {
              case Some(checkFile) =>
                processLines(checkFile) { stream =>
                  val checkLines  = stream.iterator().asScala.toSeq
                  val outputLines = Diff.splitIntoLines(output)
                  val diff        = Diff.compareContents(checkLines, outputLines)
                  if (diff.nonEmpty) {
                    errors += source
                    printerrln(s"ERROR: $source failed, unexpected output.\n$diff")
                  }
                }
              case None =>
                if (output.nonEmpty) {
                  errors += source
                  val diff = Diff.compareContents("", output)
                  printerrln(s"ERROR: $source failed, no check file found for unexpected output.\n$diff")
                }
            }
        }
      }
    }
    successWhen(errors.isEmpty && unexpectedFail.isEmpty) {
      if (unexpectedFail.nonEmpty) {
        val str = if (unexpectedFail.size == 1) "file" else "files"
        s"${unexpectedFail.length} $str did not compile when expected to: ${unexpectedFail.mkString(", ")}."
      }
      else {
        val str = if (errors.size == 1) "error" else "errors"
        s"${errors.length} $str. These sources either compiled or had an incorrect or missing check file: ${errors.mkString(", ")}."
      }
    }
  }

  private def scalac(out: String, dottyLibrary: String, sources: String*): Boolean = {
    sources.isEmpty || {
      val args = Array(
        "-d", out,
        "-classpath", classpaths(out, dottyLibrary),
        "-deprecation",
        "-Xfatal-warnings"
      ) ++ sources
      Try(nsc.Main.process(args)).getOrElse(false)
    }
  }

  // TODO call it directly when we are bootstrapped
  private[this] lazy val dotcProcess: Array[String] => Boolean = {
    val mainClass = Class.forName("dotty.tools.dotc.Main")
    val reporterClass = Class.forName("dotty.tools.dotc.reporting.Reporter")
    val Main_process = mainClass.getMethod("process", classOf[Array[String]])
    assert(Modifier.isStatic(Main_process.getModifiers), "dotty.tools.dotc.Main.process is not static!")
    val Reporter_hasErrors = reporterClass.getMethod("hasErrors")
    args => Try {
      val reporter  = Main_process.invoke(null, args)
      val hasErrors = Reporter_hasErrors.invoke(reporter).asInstanceOf[Boolean]
      !hasErrors
    }.getOrElse(false)
  }

  private def dotcPos(out: String, dottyLibrary: String, sources: String*): Try[Unit] = {
    val result = sources.isEmpty || {
      val args = Array(
        "-d", out,
        "-classpath", classpaths(out, dottyLibrary),
        "-deprecation",
        "-Xfatal-warnings"
      ) ++ sources
      dotcProcess(args)
    }
    successWhen(result)("dotc failed to compile sources.")
  }

  private def classpaths(paths: String*): String = paths.mkString(":")

  private def getSourceAsName(path: String): String =
    path.split(pathSep).last.stripSuffix(".scala")

  private def getSources(root: String, preFilters: Set[SourceKind] = Set(Scala),
    src2Filters: Set[SourceKind] = Set(Scala), src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String], Seq[String])] = {
    for {
      pre  <- getFiles(root/"pre")
      src2 <- getFiles(root/"src-2")
      src3 <- getFiles(root/"src-3")
    } yield (whitelist(preFilters, pre:_*), whitelist(src2Filters, src2:_*), whitelist(src3Filters, src3:_*))
  }

  private def visibleClasses(classpath: String, pkgName: String, src2: String*): Try[Seq[String]] = Try {
    val classes = {
      val matcher = globMatcher(
        s"$classpath/${if (pkgName.isEmpty) "" else pkgName.*->/}Test*.class"
      )
      val visibleTests = src2.map(getSourceAsName)
      val addPkg: String => String = if (pkgName.isEmpty) identity else pkgName + "." + _
      val prefix = if (pkgName.isEmpty) "" else pkgName.*->/
      val cp = JPaths.get(classpath).normalize
      def nameFromClass(path: JPath) = {
        path.subpath(cp.getNameCount, path.getNameCount)
            .normalize
            .toString
            .stripPrefix(prefix)
            .stripSuffix(".class")
      }
      var stream: ju.stream.Stream[JPath] = null
      try {
        stream = JFiles.walk(cp)
        stream.filter(p => !JFiles.isDirectory(p) && matcher.matches(p))
              .map(_.normalize)
              .iterator
              .asScala
              .drop(1) // drop the classpath itself
              .map(nameFromClass)
              .filter(visibleTests.contains)
              .map(addPkg)
              .toSeq
      }
      finally if (stream != null) {
        stream.close()
      }
    }
    if (classes.isEmpty) printwarnln("Warning: found no test classes.")
    classes
  }

  private def successWhen(cond: Boolean)(ifFalse: => String): Try[Unit] =
    Option.when(cond)(()).failOnEmpty(new TestFailure(ifFalse))

  private def runMainOn(out: String, dottyLibrary: String, tests: String*): Try[Unit] = {
    def runTests(errors: mutable.ArrayBuffer[String], runner: Runner): Try[Unit] = Try {
      for (test <- tests) {
        runner.run(test) match {
          case Success(output) =>
            val diff = Diff.compareContents("Suite passed!", output)
            if (diff.nonEmpty) {
              errors += test
              printerrln(s"ERROR: $test failed, unexpected output.\n$diff")
            }
          case Failure(err) =>
            errors += test
            printerrln(s"ERROR: $test failed: ${err.getClass.getSimpleName} ${err.getMessage}")
        }
      }
    }
    for {
      runner <- Runner.classloadFrom(classpaths(out, dottyLibrary))
      errors =  mutable.ArrayBuffer.empty[String]
      _      <- runTests(errors, runner)
      _      <- successWhen(errors.isEmpty)({
                  val str = if (errors.size == 1) "error" else "errors"
                  s"${errors.length} $str. Fix ${errors.mkString(", ")}."
                })
    } yield ()
  }

  private val helpText: String = """|# TASTy Test Help
  |
  |This runner can be used to test compilation and runtime behaviour of Scala 2 sources that depend on sources compiled with Scala 3.
  |
  |The following arguments are available to TASTy Test:
  |
  |  -help                            Display this help.
  |  -run                             Perform the run test.
  |  -neg                             Perform the neg test.
  |  --dotty-library  <paths>         Paths separated by `:`, the classpath for the dotty library.
  |  --src            <path=.>        The path that contains all compilation sources across test kinds.
  |  --out            <path=.>        output for classpaths, optional.
  |  --package        <pkg=tastytest> The package containing run tests.
  |
  |* This runner should be invoked with the `scala-compiler` module on the classpath, easily acheived by using the `scala` shell command.
  |* During compilation of test sources, and during run test execution, `--dotty-library` is on the classpath.
  |* TASTy Test currently supports run and neg tests.
  |* run tests execute as follows:
  |  1. Compile sources in `$src$/run/pre/**` with the Scala 2 compiler, to be shared accross both compilers.
  |  2. Compile sources in `$src$/run/src-3/**` with the Dotty compiler in a separate process, using `--dotty-compiler` as the JVM classpath.
  |     - Classes compiled in (1) are now on the classpath.
  |  3. Compile sources in `$src$/run/src-2/**` with the Scala 2 compiler.
  |     - Classes compiled in (1) and (2) are now on the classpath.
  |  4. Classes with name `$package$Test*` are assumed to be test cases and their main methods are executed sequentially.
  |     - A successful test should print the single line `Suite passed!` and not have any runtime exceptions.
  |     - The class will not be executed if there is no source file in `$src$/run/src-2/**` that matches the simple name of the class.
  |* neg tests execute as follows:
  |  1. Compile sources in `$src$/neg/pre/**` with the Scala 2 compiler, to be shared accross both compilers.
  |  2. Compile sources in `$src$/neg/src-3/**` with the Dotty compiler in a separate process, using `--dotty-compiler` as the JVM classpath.
  |     - Classes compiled in (1) are now on the classpath.
  |  3. Compile sources in `$src$/neg/src-2/**` with the Scala 2 compiler.
  |     - Classes compiled in (1) and (2) are now on the classpath.
  |     - A source file matching `<path>/<name>_fail.scala` succeeds a test if it fails compilation and all compiler output matches a checkfile of name `<path>/<name>.check`
  |
  |Note: Failing tests without a fix should be put in a sibling directory, such as `suspended`, to document that they are incompatible at present.""".stripMargin

  def run(args: Seq[String]): Boolean = process(args).fold(
    err => {
      printerrln(s"ERROR: ${err.getClass.getName}: ${err.getMessage}")
      true
    },
    _ => false
  )

  def process(implicit args: Seq[String]): Try[Unit] = {
    if (booleanArg("-help")) {
      Success(println(helpText))
    }
    else for {
      dottyLibrary  <- requiredArg("--dotty-library")
      srcRoot       =  optionalArg("--src", currentDir)
      pkgName       =  optionalArg("--package", "tastytest")
      run           =  booleanArg("-run")
      neg           =  booleanArg("-neg")
      out           =  findArg("--out")
      _             <- tastytest(dottyLibrary, srcRoot, pkgName, run, neg, out)
    } yield ()
  }

  def main(args: Array[String]): Unit = sys.exit(if (run(args.toList)) 1 else 0)
}
