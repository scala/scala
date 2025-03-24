/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.tastytest

import scala.collection.mutable
import scala.jdk.CollectionConverters._

import scala.util.{ Try, Success, Failure }

import java.nio.file.{ Files => JFiles, Paths => JPaths, Path => JPath }
import java.io.ByteArrayOutputStream
import java.{ util => ju }

import SourceKind._
import Files._
import java.io.OutputStream

object TastyTest {

  private[tastytest] val verbose = false
  private[tastytest] val debug = false

  private def log(s: => String): Unit =
    if (verbose) println(s)

  /**Simulates a Scala 2 application that depends on a Scala 3 library, where both may depend on a common prelude
   * compiled by Scala 2.
   *
   * Steps:
   *  1) compile all Scala files in `pre` with scala 2 to `out`
   *  2) compile all Scala files in `src-3` with scala 3 to `out`, with `out` as the classpath
   *  3) compile all Scala files in `src-2` with scala 2 to `out`, with `out` as the classpath
   *  4) run the main method of all classes in `out/pkgName` that match a file in `src-2`.
   *     e.g. `out/tastytest/TestFoo.class` should be compiled from a corresponding file
   *          `src-2/tastytest/TestFoo.scala`.
   */
  def runSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (pre, src2, src3) <- getRunSources(srcRoot/src)
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- scalacPos(out, individualCapable=false, sourceRoot=srcRoot/src/"pre", additionalSettings, pre:_*)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", additionalDottySettings, src3:_*)
    _                 <- scalacPos(out, individualCapable=true, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2:_*)
    testNames         <- visibleClasses(out, pkgName, src2:_*)
    _                 <- runMainOn(out, testNames:_*)
  } yield ()

  /**Simulates a Scala 2 application that depends on a Scala 3 library,
   * where pipeline-compatible compilation is tested by restricting to a TASTy-only classpath.
   *
   * Steps:
   *  1) compile all Scala/Java files in `src-3` with scala 3 to `out-classes`, send (Java TASTy to `java-tasty.jar`).
   *  2) copy TASTy files from `out-classes` to `out`. (ensuring Scala 2 will not see class files)
   *  3) compile all Scala files in `src-2` with scala 2 to `out`, with `out:java-tasty.jar` as the classpath.
   *  4) compile Java files in `src-3` to `out-classes` with Javac.
   *  5) run the main method of all classes in `out/pkgName` that match a file in `src-2`.
   *     e.g. `out/tastytest/TestFoo.class` should be compiled from a corresponding file
   *          `src-2/tastytest/TestFoo.scala`. Use `out:out-classes` as the classpath.
   */
  def runPipelinedSuite(src: String, srcRoot: String, pkgName: String, outDirs: Option[(String, String, String)], additionalSettings: Seq[String], additionalDottySettings: Seq[String], testFilter: Option[SourceKind] = None)(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (src2, src3)               <- get2And3Sources(srcRoot/src, src2Filters = Set(Scala), src3Filters = Set(Scala, Java))
    case ((out, outJ), outCls) <- outDirs.fold(tempDir(pkgName) <*> tempDir(pkgName) <*> tempDir(pkgName))(p => dir(p._1) <*> dir(p._2) <*> dir(p._3))
    tastyJar                    = outJ/"java-tasty.jar"
    _                          <- dotcPos(outCls, sourceRoot=srcRoot/src/"src-3", pipelineDottyOpts(tastyJar) ++: additionalDottySettings, src3:_*)
    allOuts                    <- getFiles(outCls)
    relTastys                  <- relativize(outCls, allowByKind(Set(TastyFile), allOuts:_*):_*)
    _                          <- copyAll(relTastys, outCls, out)
    src2Filtered                = testFilter.fold(src2)(kind => allowByKind(Set(kind), src2:_*))
    _                          <- scalacPos(out, tastyJar, individualCapable=true, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2Filtered:_*)
    _                          <- javacPos(outCls, sourceRoot=srcRoot/src/"src-3", allowByKind(Set(Java), src3:_*):_*)
    testNames                  <- visibleClasses(out, pkgName, src2Filtered:_*)
    _                          <- runMainOn(classpath(out, outCls), testNames:_*)
  } yield ()

  /**Simulates a Scala 2 application that depends on a Scala 3 library, where both may depend on a common prelude
   * compiled by Scala 2 and Java. In this case the applications are not executed.
   * Steps:
   *  1) compile all Java files in `pre` with Java to `out`
   *  2) compile all Scala files in `pre` with Scala 2 to `out`, with `out` as the classpath
   *  3) compile all Scala files in `src-3` with scala 3 to `out`, with `out` as the classpath
   *  4) compile all Scala files in `src-2` with scala 2 to `out`, with `out` as the classpath
   */
  def posSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (pre, src2, src3) <- getRunSources(srcRoot/src, preFilters = Set(Scala, Java))
    _                 =  log(s"Sources to compile under test: ${src2.map(cyan).mkString(", ")}")
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- javacPos(out, sourceRoot=srcRoot/src/"pre", allowByKind(Set(Java), pre:_*):_*)
    _                 <- scalacPos(out, individualCapable=false, sourceRoot=srcRoot/src/"pre", additionalSettings, allowByKind(Set(Scala), pre:_*):_*)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", additionalDottySettings, src3:_*)
    _                 <- scalacPos(out, individualCapable=true, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2:_*)
  } yield ()


  /**Simulates running scaladoc on a Scala 2 library that depends on a Scala 3 library.
   * Steps:
   *  1) compile all Scala files in `src-3` with scala 3 to `out`, with `out` as the classpath
   *  2) compile all Scala files in `src-2` with scaladoc (scala 2) to `out`, with `out` as the classpath
   */
  def posDocSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (src2, src3)      <- get2And3Sources(srcRoot/src, src2Filters = Set(Scala), src3Filters = Set(Scala))
    _                 =  log(s"Sources to compile under test: ${src2.map(cyan).mkString(", ")}")
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", additionalDottySettings, src3:_*)
    _                 <- scaladoc(out, sourceRoot=srcRoot/src/"src-2", additionalSettings, src2:_*)
  } yield ()

    /**Simulates a Scala 2 application that depends on a Scala 3 library, and is expected to fail compilation.
   * Steps:
   *  1) compile all Scala files in `src-3` with scala 3 to `out`
   *  2) attempt to compile all Scala files in `src-2` with scala 2 to `out`, with `out` as the classpath.
   *     - If a file matches `FOO_fail.scala`, then it is expected to fail compilation.
   *     - For each `FOO_fail.scala`, if the file fails compilation, there is expected to be a corresponding `FOO.check` file, containing
   *       the captured errors, or else a `FOO.skipcheck` file indicating to skip comparing errors.
   *     - If `FOO_fail.scala` has a corresponding `FOO_pre.scala` file, then that is compiled first to `out`,
   *       so that `FOO_fail.scala` may depend on its compilation results.
   */
  def negSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (src2, src3)      <- get2And3Sources(srcRoot/src, src2Filters = Set(Scala, Check, SkipCheck))
    out               <- outDir.fold(tempDir(pkgName))(dir)
    _                 <- dotcPos(out, sourceRoot=srcRoot/src/"src-3", additionalDottySettings, src3:_*)
    _                 <- scalacNeg(out, additionalSettings, src2:_*)
  } yield ()

  /**Simulates a Scala 2 application that depends on a Scala 3 library, and is expected to fail compilation,
   * where pipeline-compatible compilation is tested by restricting to a TASTy-only classpath.
   *
   * Steps:
   *  1) compile all Scala/Java files in `src-3` with scala 3 to `out-classes`, send (Java TASTy to `java-tasty.jar`).
   *  2) copy TASTy files from `out-classes` to `out`. (ensuring Scala 2 will not see class files)
   *  3) attempt to compile all Scala files in `src-2` with scala 2 to `out`, with `out:java-tasty.jar` as the classpath.
   *     - If a file matches `FOO_fail.scala`, then it is expected to fail compilation.
   *     - For each `FOO_fail.scala`, if the file fails compilation, there is expected to be a corresponding `FOO.check` file, containing
   *       the captured errors, or else a `FOO.skipcheck` file indicating to skip comparing errors.
   *     - If `FOO_fail.scala` has a corresponding `FOO_pre.scala` file, then that is compiled first to `out`,
   *       so that `FOO_fail.scala` may depend on its compilation results.
   */
  def negPipelinedSuite(src: String, srcRoot: String, pkgName: String, outDirs: Option[(String, String, String)], additionalSettings: Seq[String], additionalDottySettings: Seq[String], testFilter: Option[SourceKind] = None)(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (src2, src3)               <- get2And3Sources(srcRoot/src, src2Filters = Set(Scala, Check, SkipCheck), src3Filters = Set(Scala, Java))
    case ((out, outJ), outCls) <- outDirs.fold(tempDir(pkgName) <*> tempDir(pkgName) <*> tempDir(pkgName))(p => dir(p._1) <*> dir(p._2) <*> dir(p._3))
    tastyJar                    = outJ/"java-tasty.jar"
    _                          <- dotcPos(outCls, sourceRoot=srcRoot/src/"src-3", pipelineDottyOpts(tastyJar) ++: additionalDottySettings, src3:_*)
    allOuts                    <- getFiles(outCls)
    relTastys                  <- relativize(outCls, allowByKind(Set(TastyFile), allOuts:_*):_*)
    _                          <- copyAll(relTastys, outCls, out)
    src2Filtered                = testFilter.fold(src2)(kind => allowByKind(Set(kind, Check, SkipCheck), src2:_*))
    _                          <- scalacNeg(out, tastyJar, additionalSettings, src2Filtered:_*)
  } yield ()

  /**Simulates a Scala 3 application that depends on a Scala 2 library, where the Scala 2
   * library directly depends on an upstream Scala 3 library. The Scala 3 application is expected to fail compilation.
   * Steps:
   *  1) compile all Scala files in `src-3-upstream` with scala 3 to `out`
   *  2) compile all Scala files in `src-2-downstream` with scala 2 to `out`, with `out` as the classpath.
   *  3) attempt to compile all Scala files in `src-3-app` with scala 3 to `out`, with `out` as the classpath,
   *     following the same steps as `negSuite` to check for errors in compilation.
   */
  def negFullCircleSuite(src: String, srcRoot: String, pkgName: String, outDir: Option[String], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (src3u, src2d, src3a) <- getFullCircleSources(srcRoot/src, src3appFilters = Set(Scala, Check, SkipCheck))
    out                   <- outDir.fold(tempDir(pkgName))(dir)
    _                     <- dotcPos(out, sourceRoot=srcRoot/src/"src-3-upstream", additionalDottySettings, src3u:_*)
    _                     <- scalacPos(out, individualCapable=false, sourceRoot=srcRoot/src/"src-2-downstream", additionalSettings, src2d:_*)
    _                     <- dotcNeg(out, additionalDottySettings, src3a:_*)
  } yield ()

  /**Same as `negSuite`, but introduces a dependency on a prelude by both the Scala 3 and Scala 2 libraries. In
   * this case, they depend on binary incompatible versions of the same prelude (e.g. some definitions have moved
   * between versions). Steps:
   *  1) compile all Scala files in `pre-A` with scala 2 to `out1`.
   *  2) compile all Scala files in `pre-B` with scala 2 to `out2`.
   *  3) compile all Scala files in `src-3` with scala 3 to `out2`, with `out1` as the classpath.
   *  4) attempt to compile all Scala files in `src-2` with scala 2 to `out2`, with `out2` as the classpath,
   *     following the same steps as `negSuite` to check for errors in compilation.
   */
  def negChangePreSuite(src: String, srcRoot: String, pkgName: String, outDirs: Option[(String, String)], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (preA, preB, src2, src3) <- getMovePreChangeSources(srcRoot/src, src2Filters = Set(Scala, Check, SkipCheck))
    (out1, out2)             <- outDirs.fold(tempDir(pkgName) <*> tempDir(pkgName))(p => dir(p._1) <*> dir(p._2))
    _                        <- scalacPos(out1, individualCapable=false, sourceRoot=srcRoot/src/"pre-A", additionalSettings, preA:_*)
    _                        <- scalacPos(out2, individualCapable=false, sourceRoot=srcRoot/src/"pre-B", additionalSettings, preB:_*)
    _                        <- dotcPos(out2, out1, sourceRoot=srcRoot/src/"src-3", additionalDottySettings, src3:_*)
    _                        <- scalacNeg(out2,additionalSettings, src2:_*)
  } yield ()

  /**Same as `negSuite`, but in addition, the Scala 3 library depends on another upstream Scala 3 library,
   * which is missing from the classpath when compiling the Scala 2 library. Steps:
   *  1) compile all Scala files in `src-3-A` with scala 3 to `out1`.
   *  3) compile all Scala files in `src-3-B` with scala 3 to `out2`, with `out1:out2` as the classpath.
   *  3) attempt to compile all Scala files in `src-2` with scala 2 to `out2`, with `out2` as the classpath,
   *     following the same steps as `negSuite` to check for errors in compilation.
   */
  def negSuiteIsolated(src: String, srcRoot: String, pkgName: String, outDirs: Option[(String, String)], additionalSettings: Seq[String], additionalDottySettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = for {
    (src2, src3A, src3B) <- getNegIsolatedSources(srcRoot/src, src2Filters = Set(Scala, Check, SkipCheck))
    (out1, out2)         <- outDirs.fold(tempDir(pkgName) <*> tempDir(pkgName))(p => dir(p._1) <*> dir(p._2))
    _                    <- dotcPos(out1, sourceRoot=srcRoot/src/"src-3-A", additionalDottySettings, src3A:_*)
    _                    <- dotcPos(out2, classpath(out1, out2), sourceRoot=srcRoot/src/"src-3-B", additionalDottySettings, src3B:_*)
    _                    <- scalacNeg(out2, additionalSettings, src2:_*)
  } yield ()

  private def javacPos(out: String, sourceRoot: String, sources: String*): Try[Unit] = {
    log(s"compiling sources in ${yellow(sourceRoot)} with javac.")
    successWhen(Javac.javac(out, sources:_*))("javac failed to compile sources.")
  }

  private def scalacPos(out: String, extraCp: String, individualCapable: Boolean, sourceRoot: String, additionalSettings: Seq[String], sources: String*): Try[Unit] =
    scalacPos(out, extraCp = Some(extraCp), individualCapable, sourceRoot, additionalSettings, sources:_*)

  private def scalacPos(out: String, individualCapable: Boolean, sourceRoot: String, additionalSettings: Seq[String], sources: String*): Try[Unit] =
    scalacPos(out, extraCp = None, individualCapable, sourceRoot, additionalSettings, sources:_*)

  private def scalacPos(out: String, extraCp: Option[String], individualCapable: Boolean, sourceRoot: String, additionalSettings: Seq[String], sources: String*): Try[Unit] = {
    log(s"compiling sources in ${yellow(sourceRoot)} with scalac.")
    val res = {
      if (debug && individualCapable) {
        def compileIndividual(srcs: List[String]): Try[Boolean] = {
          srcs match {
            case Nil => Success(true)
            case src :: rest =>
              log(s"compiling source ${yellow(src)} with scalac.")
              Scalac.scalac(out, extraCp, "-Ytasty-reader" +: additionalSettings, src) match {
                case Success(true) => compileIndividual(rest)
                case err => err
              }
          }
        }
        compileIndividual(sources.toList)
      }
      else {
        Scalac.scalac(out, extraCp, "-Ytasty-reader" +: additionalSettings, sources:_*)
      }
    }
    successWhen(res)("scalac failed to compile sources.")
  }

  private def scaladoc(out: String, sourceRoot: String, additionalSettings: Seq[String], sources: String*): Try[Unit] = {
    log(s"compiling sources in ${yellow(sourceRoot)} with scalac.")
    val res = Scaladoc.scaladoc(out, "-Ytasty-reader" +: additionalSettings, sources:_*)
    successWhen(res)("scaladoc failed to compile resources")
  }

  private def scalacNeg(out: String, additionalSettings: Seq[String], files: String*): Try[Unit] =
    scalacNeg(out, extraCp = None, additionalSettings, files:_*)

  private def scalacNeg(out: String, extraCp: String, additionalSettings: Seq[String], files: String*): Try[Unit] =
    scalacNeg(out, extraCp = Some(extraCp), additionalSettings, files:_*)

  private def scalacNeg(out: String, extraCp: Option[String], additionalSettings: Seq[String], files: String*): Try[Unit] = {
    def compile(source: String, writer: OutputStream) =
      Scalac.scalac(writer, out, extraCp, "-Ytasty-reader" +: additionalSettings, source)
    negTestImpl(withCapture(_, compile, identity))(files:_*)
  }

  private def withCapture(source: String, compile: (String, OutputStream) => Try[Boolean], post: String => String): (String, Try[Boolean]) = {
    val byteArrayStream = new ByteArrayOutputStream(50)
    try {
      val compiled = compile(source, byteArrayStream)
      (post(byteArrayStream.toString), compiled)
    } finally byteArrayStream.close()
  }

  private def negTestImpl(compile: String => (String, Try[Boolean]))(files: String*): Try[Unit] = {
    val errors = mutable.ArrayBuffer.empty[String]
    val unexpectedFail = mutable.ArrayBuffer.empty[String]
    val crashes = mutable.ArrayBuffer.empty[String]
    val failMap: Map[String, (Option[String], Option[String])] = {
      val (sources, rest) = files.partition(ScalaFail.permits)
      sources.map({ s =>
        val name = s.stripSuffix(ScalaFail.name)
        val check = Check.fileOf(name)
        val skip = SkipCheck.fileOf(name)
        val pre = ScalaPre.fileOf(name)
        val foundCheck = rest.find(n => n == check || n == skip)
        val foundPre   = rest.find(_ == pre)
        s -> (foundCheck, foundPre)
      }).toMap
    }
    if (failMap.isEmpty) {
      printwarnln(s"Warning: there are no source files marked as fail tests. (**/*${ScalaFail.name})")
    }
    def negCompile(source: String): Unit = {
      val (output, compiled) = {
        if (ScalaFail.permits(source)) {
          val testName = source.stripSuffix(ScalaFail.name)
          log(s"neg test ${cyan(testName)} started")
          failMap(source) match {
            case (_, Some(pre)) =>
              log(s"  - compiling pre file...")
              negCompile(pre)
            case _ =>
          }
        }
        compile(source)
      }
      compiled match {
        case Failure(exception) =>
          crashes += source
          printerrln(s"ERROR: fatal error running compiler for $source: $exception")
        case Success(true) =>
          if (failMap.contains(source)) {
            errors += source
            printerrln(s"ERROR: $source successfully compiled when expected to fail.")
          }
        case Success(false) =>
          failMap.get(source) match {
            case None =>
              unexpectedFail += source
              System.err.println(output)
              printerrln(s"ERROR: $source did not compile when expected to. Perhaps it should match (**/*${ScalaFail.name})")
            case Some((Some(checkFile), _)) if Check.permits(checkFile) =>
              processLines(checkFile) { stream =>
                val checkLines  = Diff.splitIntoLines(stream)
                val outputLines = Diff.splitIntoLines(output)
                assert(outputLines.filterNot(_.isEmpty()).nonEmpty, s"outputLines should not be empty: $outputLines")
                val diff        = Diff.compareContents(outputLines, checkLines)
                if (diff.nonEmpty) {
                  errors += source
                  printerrln(s"ERROR: $source failed, unexpected output.\n$diff")
                }
              }
            case Some((Some(skipCheckFile), _)) =>
              printwarnln(s"warning: skipping check on ${skipCheckFile.stripSuffix(SkipCheck.name)}")
            case Some((None, _)) =>
              if (output.nonEmpty) {
                errors += source
                val diff = Diff.compareContents(output, "")
                printerrln(s"ERROR: $source failed, no check file found for unexpected output.\n$diff")
              }
          }
      }
    }

    val sources = files.filter(Scala.permits).filterNot(ScalaPre.permits)
    sources.foreach(negCompile)
    successWhen(errors.isEmpty && unexpectedFail.isEmpty && crashes.isEmpty) {
      var msgs = List.empty[String]
      if (crashes.nonEmpty) {
        val str = if (crashes.size == 1) "file" else "files"
        msgs ::= s"${crashes.length} $str fatally crashed the compiler: ${crashes.mkString(", ")}."
      }
      else if (unexpectedFail.nonEmpty) {
        val str = if (unexpectedFail.size == 1) "file" else "files"
        msgs ::= s"${unexpectedFail.length} $str did not compile when expected to: ${unexpectedFail.mkString(", ")}."
      }
      else if (errors.nonEmpty) {
        val str = if (errors.size == 1) "error" else "errors"
        msgs ::= s"Found ${errors.length} $str. These sources either compiled or had an incorrect or missing check file: ${errors.mkString(", ")}."
      }
      msgs.mkString(System.lineSeparator())
    }
  }

  def dotcPos(out: String, sourceRoot: String, additionalSettings: Seq[String], sources: String*)(implicit cl: Dotc.ClassLoader): Try[Unit] = dotcPos(out, out, sourceRoot, additionalSettings, sources:_*)

  def dotcPos(out: String, classpath: String, sourceRoot: String, additionalSettings: Seq[String], sources: String*)(implicit cl: Dotc.ClassLoader): Try[Unit] = {
    log(s"compiling sources in ${yellow(sourceRoot)} with dotc.")
    val process = Dotc.dotc(out, classpath, additionalSettings, sources:_*)
    successWhen(process)("dotc failed to compile sources.")
  }

  private def pipelineDottyOpts(tastyJar: String): Seq[String] =
    Seq("-Xjava-tasty", "-Xearly-tasty-output", tastyJar)

  private def dotcNeg(out: String, additionalSettings: Seq[String], files: String*)(implicit cl: Dotc.ClassLoader): Try[Unit] = {
    def compile(source: String, writer: OutputStream) = {
      Dotc.dotc(writer, out, out, additionalSettings, source)
    }
    def scrub(source: String, output: String): String = {
      output.linesIterator.collect {
        case header if header.contains(source) =>
          val filePart = source.split(java.util.regex.Pattern.quote(Files.pathSep)).last
          header.trim.replace(source, filePart)
        case ok => ok
      }.mkString(System.lineSeparator())
    }
    negTestImpl(src => withCapture(src, compile, scrub(src, _)))(files:_*)
  }

  private def getSourceAsName(path: String): String =
    path.substring(path.lastIndexOf(pathSep) + pathSep.length).stripSuffix(".scala")

  private def getRunSources(root: String, preFilters: Set[SourceKind] = Set(Scala),
    src2Filters: Set[SourceKind] = Set(Scala), src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String], Seq[String])] = {
    for {
      (src2, src3) <- get2And3Sources(root, src2Filters, src3Filters)
      pre          <- getFiles(root/"pre")
    } yield (allowByKind(preFilters, pre:_*), src2, src3)
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
    } yield (allowByKind(preAFilters, preA:_*), allowByKind(preBFilters, preB:_*), src2, src3)
  }

  private def get2And3Sources(root: String, src2Filters: Set[SourceKind] /*= Set(Scala)*/,
    src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String])] = {
    for {
      src2 <- getFiles(root/"src-2")
      src3 <- getFiles(root/"src-3")
    } yield (allowByKind(src2Filters, src2:_*), allowByKind(src3Filters, src3:_*))
  }

  private def getFullCircleSources(root: String, src3upFilters: Set[SourceKind] = Set(Scala),
    src2downFilters: Set[SourceKind] = Set(Scala),
    src3appFilters: Set[SourceKind]
  ): Try[(Seq[String], Seq[String], Seq[String])] = {
    for {
      src3up <- getFiles(root/"src-3-upstream")
      src2down <- getFiles(root/"src-2-downstream")
      src3app <- getFiles(root/"src-3-app")
    } yield (
      allowByKind(src3upFilters, src3up:_*),
      allowByKind(src2downFilters, src2down:_*),
      allowByKind(src3appFilters, src3app:_*)
    )
  }

  private def getPreChangeSources(root: String, preAFilters: Set[SourceKind] /*= Set(Scala)*/,
    preBFilters: Set[SourceKind] /*= Set(Scala)*/
  ): Try[(Seq[String], Seq[String])] = {
    for {
      preA <- getFiles(root/"pre-a")
      preB <- getFiles(root/"pre-b")
    } yield (allowByKind(preAFilters, preA:_*), allowByKind(preBFilters, preB:_*))
  }

  private def getNegIsolatedSources(root: String, src2Filters: Set[SourceKind] /*= Set(Scala)*/,
    src3Filters: Set[SourceKind] = Set(Scala)
  ): Try[(Seq[String], Seq[String], Seq[String])] = {
    for {
      src2  <- getFiles(root/"src-2")
      src3A <- getFiles(root/"src-3-A")
      src3B <- getFiles(root/"src-3-B")
    } yield (allowByKind(src2Filters, src2:_*), allowByKind(src3Filters, src3A:_*), allowByKind(src3Filters, src3B:_*))
  }

  private def visibleClasses(classpath: String, pkgName: String, src2: String*): Try[Seq[String]] = Try {
    val classes = {
      val matcher = globMatcher(
        s"$classpath/${if (pkgName.isEmpty) "" else pkgName.toBinaryName}Test*.class"
      )
      val visibleTests = src2.map(getSourceAsName)
      val addPkg: String => String = if (pkgName.isEmpty) identity else pkgName + "." + _
      val prefix = if (pkgName.isEmpty) "" else pkgName.toBinaryName
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
