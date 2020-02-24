package scala.tools.tastytest

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.tools.nsc
import scala.util.{ Try, Success, Failure }
import scala.util.chaining._

import dotty.tools.dotc
import dotc.reporting.{ Reporter => DottyReporter }

import java.nio.file.{ Files => JFiles, Paths => JPaths, Path => JPath }
import java.io.ByteArrayOutputStream
import java.{ lang => jl, util => ju }
import jl.reflect.Modifier

import SourceKind._
import Files._
import scala.tools.nsc.{Global, Settings, reporters}, reporters.ConsoleReporter

object TastyTest {

  def runSuite(src: String, dottyLibrary: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String]): Try[Unit] = for {
    (pre, src2, src3) <- getRunSources(srcRoot/src)
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- scalacPos(out, dottyLibrary, sourceRoot=srcRoot/src/"pre", additionalSettings, pre:_*)
    _                 <- dotcPos(out, dottyLibrary, sourceRoot=srcRoot/src/"src-3", src3:_*)
    _                 <- scalacPos(out, dottyLibrary, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2:_*)
    testNames         <- visibleClasses(out, pkgName, src2:_*)
    _                 <- runMainOn(out, dottyLibrary, testNames:_*)
  } yield ()

  def posSuite(src: String, dottyLibrary: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String]): Try[Unit] = for {
    (pre, src2, src3) <- getRunSources(srcRoot/src)
    _                 =  println(s"Sources to compile under test: ${src2.map(cyan).mkString(", ")}")
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- scalacPos(out, dottyLibrary, sourceRoot=srcRoot/src/"pre", additionalSettings, pre:_*)
    _                 <- dotcPos(out, dottyLibrary, sourceRoot=srcRoot/src/"src-3", src3:_*)
    _                 <- scalacPos(out, dottyLibrary, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2:_*)
  } yield ()

  def negSuite(src: String, dottyLibrary: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String]): Try[Unit] = for {
    (src2, src3)      <- getNegSources(srcRoot/src, src2Filters = Set(Scala, Check, SkipCheck))
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- dotcPos(out, dottyLibrary, sourceRoot=srcRoot/src/"src-3", src3:_*)
    _                 <- scalacNeg(out, dottyLibrary, additionalSettings, src2:_*)
  } yield ()

  private def scalacPos(out: String, dottyLibrary: String, sourceRoot: String, additionalSettings: Seq[String], sources: String*): Try[Unit] = {
    println(s"compiling sources in ${yellow(sourceRoot)} with scalac.")
    successWhen(scalac(out, dottyLibrary, additionalSettings, sources:_*))("scalac failed to compile sources.")
  }

  private def scalacNeg(out: String, dottyLibrary: String, additionalSettings: Seq[String], files: String*): Try[Unit] = {
    val errors = mutable.ArrayBuffer.empty[String]
    val unexpectedFail = mutable.ArrayBuffer.empty[String]
    val failMap = {
      val (sources, rest) = files.partition(ScalaFail.filter)
      sources.map({ s =>
        val name  = s.stripSuffix(ScalaFail.name)
        val check = Check.fileOf(name)
        val skip  = SkipCheck.fileOf(name)
        val found = rest.find(n => n == check || n == skip)
        s -> found
      }).toMap
    }
    if (failMap.isEmpty) {
      printwarnln(s"Warning: there are no source files marked as fail tests. (**/*${ScalaFail.name})")
    }
    for (source <- files.filter(Scala.filter)) {
      val buf = new StringBuilder(50)
      val compiled = {
        val byteArrayStream = new ByteArrayOutputStream(50)
        try {
          if (ScalaFail.filter(source)) {
            println(s"neg test ${cyan(source.stripSuffix(ScalaFail.name))} started")
          }
          val compiled = Console.withErr(byteArrayStream) {
            Console.withOut(byteArrayStream) {
              scalac(out, dottyLibrary, additionalSettings, source)
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
              case Some(checkFile) if Check.filter(checkFile) =>
                processLines(checkFile) { stream =>
                  val checkLines  = stream.iterator().asScala.toSeq
                  val outputLines = Diff.splitIntoLines(output)
                  val diff        = Diff.compareContents(outputLines, checkLines)
                  if (diff.nonEmpty) {
                    errors += source
                    printerrln(s"ERROR: $source failed, unexpected output.\n$diff")
                  }
                }
              case Some(skipCheckFile) if SkipCheck.filter(skipCheckFile) =>
                printwarnln(s"warning: skipping check on ${skipCheckFile.stripSuffix(SkipCheck.name)}")
              case None =>
                if (output.nonEmpty) {
                  errors += source
                  val diff = Diff.compareContents(output, "")
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

  private def scalac(out: String, dottyLibrary: String, additionalSettings: Seq[String], sources: String*): Boolean = {

    def runCompile(global: Global): Boolean = {
      global.reporter.reset()
      new global.Run() compile sources.toList
      val result = !global.reporter.hasErrors
      global.reporter.finish()
      result
    }

    def newCompiler(args: String*): Global =
      fromSettings(new Settings().tap(_ processArguments(args.toList, true)))

    def fromSettings(settings: Settings): Global =
      Global(settings, new ConsoleReporter(settings).tap(_.shortname = true))

    def compile(args: String*) =
      Try(runCompile(newCompiler(args: _*))).getOrElse(false)

    sources.isEmpty || {
      val settings = Array(
        "-d", out,
        "-classpath", classpaths(out, dottyLibrary),
        "-deprecation",
        "-Xfatal-warnings",
        "-usejavacp"
      ) ++ additionalSettings
      compile(settings:_*)
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

  private def dotcPos(out: String, dottyLibrary: String, sourceRoot: String, sources: String*): Try[Unit] = {
    println(s"compiling sources in ${yellow(sourceRoot)} with dotc.")
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

  private def getRunSources(root: String, preFilters: Set[SourceKind] = Set(Scala),
    src2Filters: Set[SourceKind] = Set(Scala), src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String], Seq[String])] = {
    for {
      (src2, src3) <- getNegSources(root, src2Filters, src3Filters)
      pre          <- getFiles(root/"pre")
    } yield (whitelist(preFilters, pre:_*), src2, src3)
  }

  private def getNegSources(root: String, src2Filters: Set[SourceKind] = Set(Scala),
    src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String])] = {
    for {
      src2 <- getFiles(root/"src-2")
      src3 <- getFiles(root/"src-3")
    } yield (whitelist(src2Filters, src2:_*), whitelist(src3Filters, src3:_*))
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
        val (pkgs, name) = {
          val names = test.split('.')
          names.init.mkString(".") -> names.last
        }
        println(s"run suite ${if (pkgs.nonEmpty) pkgs + '.' else ""}${cyan(name)} started")
        runner.run(test) match {
          case Success(output) =>
            val diff = Diff.compareContents(output, "Suite passed!")
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

}
