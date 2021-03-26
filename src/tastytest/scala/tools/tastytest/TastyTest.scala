package scala.tools.tastytest

import scala.collection.mutable
import scala.jdk.CollectionConverters._

import scala.util.{ Try, Success, Failure }

import java.nio.file.{ Files => JFiles, Paths => JPaths, Path => JPath }
import java.io.ByteArrayOutputStream
import java.{ util => ju }

import SourceKind._
import Files._

object TastyTest {

  private[tastytest] val verbose = false

  private def log(s: => String): Unit =
    if (verbose) println(s)

  def runSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (pre, src2, src3) <- getRunSources(srcRoot/src)
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- scalacPos(out, sourceRoot=srcRoot/src/"pre", additionalSettings, pre:_*)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", additionalDottySettings, src3:_*)
    _                 <- scalacPos(out, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2:_*)
    testNames         <- visibleClasses(out, pkgName, src2:_*)
    _                 <- runMainOn(out, testNames:_*)
  } yield ()

  def posSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (pre, src2, src3) <- getRunSources(srcRoot/src, preFilters = Set(Scala, Java))
    _                 =  log(s"Sources to compile under test: ${src2.map(cyan).mkString(", ")}")
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- javacPos(out, sourceRoot=srcRoot/src/"pre", filterByKind(Set(Java), pre:_*):_*)
    _                 <- scalacPos(out, sourceRoot=srcRoot/src/"pre", additionalSettings, filterByKind(Set(Scala), pre:_*):_*)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", additionalDottySettings, src3:_*)
    _                 <- scalacPos(out, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2:_*)
  } yield ()

  def negSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (src2, src3)      <- get2And3Sources(srcRoot/src, src2Filters = Set(Scala, Check, SkipCheck))
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", additionalDottySettings, src3:_*)
    _                 <- scalacNeg(out, additionalSettings, src2:_*)
  } yield ()

  def negChangePreSuite(src: String, srcRoot: String, pkgName: String, outDirs: Option[(String, String)], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (preA, preB, src2, src3) <- getMovePreChangeSources(srcRoot/src, src2Filters = Set(Scala, Check, SkipCheck))
    (out1, out2)             <- outDirs.fold(tempDir(pkgName) *> tempDir(pkgName))(p => dir(p._1) *> dir(p._2))
    _                        <- scalacPos(out1, sourceRoot=srcRoot/src/"pre-A", additionalSettings, preA:_*)
    _                        <- dotcPos(out2, out1, sourceRoot=srcRoot/src/"src-3", additionalDottySettings, src3:_*)
    _                        <- scalacPos(out2, sourceRoot=srcRoot/src/"pre-B", additionalSettings, preB:_*)
    _                        <- scalacNeg(out2, additionalSettings, src2:_*)
  } yield ()

  def negSuiteIsolated(src: String, srcRoot: String, pkgName: String, outDirs: Option[(String, String)], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (src2, src3A, src3B) <- getNegIsolatedSources(srcRoot/src, src2Filters = Set(Scala, Check, SkipCheck))
    (out1, out2)         <- outDirs.fold(tempDir(pkgName) *> tempDir(pkgName))(p => dir(p._1) *> dir(p._2))
    _                    <- dotcPos(out1, sourceRoot=srcRoot/src/"src-3-A", additionalDottySettings, src3A:_*)
    _                    <- dotcPos(out2, classpath(out1, out2), sourceRoot=srcRoot/src/"src-3-B", additionalDottySettings, src3B:_*)
    _                    <- scalacNeg(out2, additionalSettings, src2:_*)
  } yield ()

  private def javacPos(out: String, sourceRoot: String, sources: String*): Try[Unit] = {
    log(s"compiling sources in ${yellow(sourceRoot)} with javac.")
    successWhen(Javac.javac(out, sources:_*))("javac failed to compile sources.")
  }

  private def scalacPos(out: String, sourceRoot: String, additionalSettings: Seq[String], sources: String*): Try[Unit] = {
    log(s"compiling sources in ${yellow(sourceRoot)} with scalac.")
    successWhen(Scalac.scalac(out, "-Ytasty-reader" +: additionalSettings, sources:_*))("scalac failed to compile sources.")
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
            log(s"neg test ${cyan(source.stripSuffix(ScalaFail.name))} started")
          }
          val compiled = Console.withErr(byteArrayStream) {
            Console.withOut(byteArrayStream) {
              Scalac.scalac(out, "-Ytasty-reader" +: additionalSettings, source)
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
          case Some(Some(checkFile)) if Check.filter(checkFile) =>
            processLines(checkFile) { stream =>
              val checkLines  = stream.iterator().asScala.toSeq
              val outputLines = Diff.splitIntoLines(output)
              val diff        = Diff.compareContents(outputLines, checkLines)
              if (diff.nonEmpty) {
                errors += source
                printerrln(s"ERROR: $source failed, unexpected output.\n$diff")
              }
            }
          case Some(Some(skipCheckFile)) =>
            printwarnln(s"warning: skipping check on ${skipCheckFile.stripSuffix(SkipCheck.name)}")
          case Some(None) =>
            if (output.nonEmpty) {
              errors += source
              val diff = Diff.compareContents(output, "")
              printerrln(s"ERROR: $source failed, no check file found for unexpected output.\n$diff")
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

  def dotcPos(out: String, sourceRoot: String, additionalSettings: Seq[String], sources: String*)(implicit cl: Dotc.ClassLoader): Try[Unit] = dotcPos(out, out, sourceRoot, additionalSettings, sources:_*)

  def dotcPos(out: String, classpath: String, sourceRoot: String, additionalSettings: Seq[String], sources: String*)(implicit cl: Dotc.ClassLoader): Try[Unit] = {
    log(s"compiling sources in ${yellow(sourceRoot)} with dotc.")
    val process = Dotc.dotc(out, classpath, additionalSettings, sources:_*)
    successWhen(process)("dotc failed to compile sources.")
  }

  private def getSourceAsName(path: String): String =
    path.substring(path.lastIndexOf(pathSep) + pathSep.length).stripSuffix(".scala")

  private def getRunSources(root: String, preFilters: Set[SourceKind] = Set(Scala),
    src2Filters: Set[SourceKind] = Set(Scala), src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String], Seq[String])] = {
    for {
      (src2, src3) <- get2And3Sources(root, src2Filters, src3Filters)
      pre          <- getFiles(root/"pre")
    } yield (filterByKind(preFilters, pre:_*), src2, src3)
  }

  private def getMovePreChangeSources(root: String,
    preAFilters: Set[SourceKind] = Set(Scala),
    preBFilters: Set[SourceKind] = Set(Scala),
    src2Filters: Set[SourceKind] /*= Set(Scala)*/,
    src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String], Seq[String], Seq[String])] = {
    for {
      (src2, src3) <- get2And3Sources(root, src2Filters, src3Filters)
      (preA, preB) <- getPreChangeSources(root, preAFilters, preBFilters)
    } yield (filterByKind(preAFilters, preA:_*), filterByKind(preBFilters, preB:_*), src2, src3)
  }

  private def get2And3Sources(root: String, src2Filters: Set[SourceKind] /*= Set(Scala)*/,
    src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String])] = {
    for {
      src2 <- getFiles(root/"src-2")
      src3 <- getFiles(root/"src-3")
    } yield (filterByKind(src2Filters, src2:_*), filterByKind(src3Filters, src3:_*))
  }

  private def getPreChangeSources(root: String, preAFilters: Set[SourceKind] /*= Set(Scala)*/,
    preBFilters: Set[SourceKind] /*= Set(Scala)*/
  ): Try[(Seq[String], Seq[String])] = {
    for {
      preA <- getFiles(root/"pre-a")
      preB <- getFiles(root/"pre-b")
    } yield (filterByKind(preAFilters, preA:_*), filterByKind(preBFilters, preB:_*))
  }

  private def getNegIsolatedSources(root: String, src2Filters: Set[SourceKind] /*= Set(Scala)*/,
    src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String], Seq[String])] = {
    for {
      src2  <- getFiles(root/"src-2")
      src3A <- getFiles(root/"src-3-A")
      src3B <- getFiles(root/"src-3-B")
    } yield (filterByKind(src2Filters, src2:_*), filterByKind(src3Filters, src3A:_*), filterByKind(src3Filters, src3B:_*))
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
        log(s"run suite ${if (pkgs.nonEmpty) pkgs + '.' else ""}${cyan(name)} started")
        runner.runCaptured(test) match {
          case Success(output) =>
            val diff = Diff.compareContents(output, "")
            if (diff.nonEmpty) {
              errors += test
              printerrln(s"ERROR: $test failed, unexpected output.\n$diff")
            }
          case Failure(err) =>
            errors += test
            printerrln(s"ERROR: $test failed: ${err.getClass.getSimpleName} ${err.getMessage} in ${err.getStackTrace().mkString("\n  ", "\n  ", "")}")
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
