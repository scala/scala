package scala.tools.tastytest

import scala.collection.mutable
import scala.jdk.CollectionConverters._

import scala.util.{ Try, Success, Failure }

import java.nio.file.{ Files => JFiles, Paths => JPaths, Path => JPath }
import java.io.ByteArrayOutputStream
import java.{ util => ju }

import SourceKind._
import Files._
import scala.util.Properties

object TastyTest {

  def runSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String]): Try[Unit] = for {
    (pre, src2, src3) <- getRunSources(srcRoot/src)
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- scalacPos(out, sourceRoot=srcRoot/src/"pre", additionalSettings, pre:_*)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", src3:_*)
    _                 <- scalacPos(out, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2:_*)
    testNames         <- visibleClasses(out, pkgName, src2:_*)
    _                 <- runMainOn(out, testNames:_*)
  } yield ()

  def posSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String]): Try[Unit] = for {
    (pre, src2, src3) <- getRunSources(srcRoot/src)
    _                 =  println(s"Sources to compile under test: ${src2.map(cyan).mkString(", ")}")
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- scalacPos(out, sourceRoot=srcRoot/src/"pre", additionalSettings, pre:_*)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", src3:_*)
    _                 <- scalacPos(out, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2:_*)
  } yield ()

  def negSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String]): Try[Unit] = for {
    (src2, src3)      <- getNegSources(srcRoot/src, src2Filters = Set(Scala, Check, SkipCheck))
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", src3:_*)
    _                 <- scalacNeg(out, additionalSettings, src2:_*)
  } yield ()

  private def scalacPos(out: String, sourceRoot: String, additionalSettings: Seq[String], sources: String*): Try[Unit] = {
    println(s"compiling sources in ${yellow(sourceRoot)} with scalac.")
    successWhen(Scalac.scalac(out, additionalSettings, sources:_*))("scalac failed to compile sources.")
  }

  private def scalacNeg(out: String, additionalSettings: Seq[String], files: String*): Try[Unit] = {
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
              Scalac.scalac(out, additionalSettings, source)
            }
          }
          byteArrayStream.flush()
          buf.append(byteArrayStream.toString)
          compiled
        }
        finally byteArrayStream.close()
      }
      if (compiled.getOrElse(false)) {
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

  def dotcPos(out: String, sourceRoot: String, sources: String*): Try[Unit] = {
    println(s"compiling sources in ${yellow(sourceRoot)} with dotc.")
    successWhen(Dotc.dotc(out, sources:_*))("dotc failed to compile sources.")
  }

  private def getSourceAsName(path: String): String =
    path.substring(path.lastIndexOf(pathSep) + pathSep.length).stripSuffix(".scala")

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

  private def successWhen(cond: Try[Boolean])(ifFalse: => String): Try[Unit] =
    cond.flatMap(success => if (success) Success(()) else Failure(new TestFailure(ifFalse)))

  private def runMainOn(out: String, tests: String*): Try[Unit] = {
    def runTests(errors: mutable.ArrayBuffer[String], runner: Runner): Try[Unit] = Try {
      for (test <- tests) {
        val (pkgs, name) = {
          val names = test.split('.')
          names.init.mkString(".") -> names.last
        }
        println(s"run suite ${if (pkgs.nonEmpty) pkgs + '.' else ""}${cyan(name)} started")
        runner.runCaptured(test) match {
          case Success(output) =>
            val diff = Diff.compareContents(output, "")
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
      cldr   <- Runner.classloadFrom(out)
      runner <- Runner.capturingRunner(cldr)
      errors =  mutable.ArrayBuffer.empty[String]
      _      <- runTests(errors, runner)
      _      <- successWhen(errors.isEmpty)({
                  val str = if (errors.size == 1) "error" else "errors"
                  s"${errors.length} $str. Fix ${errors.mkString(", ")}."
                })
    } yield ()
  }

}
