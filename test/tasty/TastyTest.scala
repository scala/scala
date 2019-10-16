import scala.sys.process._
import java.nio.file.{Files, Paths, Path, DirectoryStream, FileSystems}
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import scala.util.Properties

import scala.tools.nsc

object TastyTest {

  def tastytest(dottyLibrary: String, dottyCompiler: String, src2dir: String, src3dir: String, pkgName: String): Try[Unit] = {
    val pkgPath = pkgName.split(raw"\.").toIndexedSeq
    for {
      src2      <- getFiles(path(src2dir, pkgPath:_*))
      src3      <- getFiles(path(src3dir, pkgPath:_*))
      out       <- tempDir(pkgName)
      _         <- dotc(out, dottyLibrary, dottyCompiler, src3:_*)
      _         <- scalac(out, dottyLibrary, src2:_*)
      testNames =  src2.map(getSourceAsName).filter(_.startsWith("Test")).map(pkgName + "." + _)
      _         <- runTests(out, dottyLibrary, testNames:_*)
    } yield println("All passed!")
  }

  private def scalac(out: String, dottyLibrary: String, sources: String*): Try[Unit] = {
    val args = Array(
      "-d", out,
      "-classpath", classpaths(out, dottyLibrary)
    ) ++ sources
    successWhen(nsc.Main.process(args))("scalac failed to compile sources.")
  }

  private def dotc(out: String, dottyLibrary: String, dottyCompiler: String, sources: String*): Try[Unit] = {
    val dotc = (
         "java"
      +: "-classpath" +: dottyCompiler
      +: "dotty.tools.dotc.Main"
      +: "-d" +: out
      +: "-classpath" +: classpaths(out, dottyLibrary)
      +: sources
    )
    successWhen(dotc.! == 0)("dotc failed to compile sources.")
  }

  private def classpaths(paths: String*): String = paths.mkString(":")
  private def path(part: String, parts: String*): String = (part +: parts).mkString(pathSep)

  private def optionalArg(arg: String, default: => String)(implicit args: Seq[String]): String =
    findArg(arg).getOrElse(default)

  private def requiredArg(arg: String)(implicit args: Seq[String]): Try[String] =
    failOnEmpty(findArg(arg))(s"please provide argument: $arg")

  private def booleanArg(arg: String)(implicit args: Seq[String]): Boolean =
    args.contains(arg)

  private def findArg(arg: String)(implicit args: Seq[String]): Option[String] =
    args.sliding(2).filter(_.length == 2).find(_.head == arg).map(_.last)

  private def getSourceAsName(path: String): String =
    path.split(pathSep).last.stripSuffix(".scala")

  private def getFiles(dir: String): Try[Seq[String]] = Try {
    var stream: DirectoryStream[Path] = null
    try {
      stream = Files.newDirectoryStream(Paths.get(dir))
      stream.iterator.asScala.map(_.toString).toSeq
    } finally {
      if (stream != null) {
        stream.close()
      }
    }
  }

  private def tempDir(dir: String): Try[String] = Try(Files.createTempDirectory(dir)).map(_.toString)

  private def successWhen(cond: Boolean)(ifFalse: => String): Try[Unit] =
    failOnEmpty(Option.when(cond)(()))(ifFalse)

  private def failOnEmpty[A](opt: Option[A])(ifEmpty: => String): Try[A] =
    opt.toRight(new IllegalStateException(ifEmpty)).toTry

  private def runTests(out: String, dottyLibrary: String, tests: String*): Try[Unit] = {
    val errors = mutable.ArrayBuffer.empty[String]
    for (test <- tests) {
      val buf = new StringBuilder(50)
      val success = {
        val byteArrayStream = new java.io.ByteArrayOutputStream(50)
        try {
          val success = Console.withOut(byteArrayStream) {
            nsc.MainGenericRunner.process(Array("-classpath", classpaths(out, dottyLibrary), test))
          }
          byteArrayStream.flush()
          buf.append(byteArrayStream.toString)
          success
        }
        finally {
          byteArrayStream.close()
        }
      }
      if (!success) {
        errors += test
        System.err.println(s"ERROR: $test failed.")
      }
      else {
        val output = buf.toString
        if ("Suite passed!" != output.trim) {
          errors += test
          System.err.println(s"ERROR: $test failed, unexpected output <<<;OUTPUT;\n${output}\n;OUTPUT;")
        }
      }
    }
    successWhen(errors.isEmpty) {
      val str = if (errors.size == 1) "error" else "errors"
      s"${errors.length} $str. Fix ${errors.mkString(", ")}."
    }
  }

  private val pathSep: String = FileSystems.getDefault.getSeparator

  private val helpText: String = """|# TASTy Test Help
  |
  |This runner can be used to test compilation and runtime behaviour of Scala 2 sources that depend on sources compiled with Scala 3.
  |
  |The following arguments are available to TASTy Test:
  |
  |  -help                      *optional*          Display this help.
  |  --dotty-library   <paths>  *required*          Paths separated by `:`, the classpath for the dotty library.
  |  --dotty-compiler  <paths>  *required*          Paths separated by `:`, the classpath for the dotty compiler.
  |  --src-2           <path>   *default=src-2*     The path to scala 2 sources that depend on dotty compiled code.
  |  --src-3           <path>   *default=src-3*     The path to scala 3 sources that will be compiled by dotty.
  |  --package         <pkg>    *default=tastytest* The package for all classes, must match source directory structure.
  |
  |* This runner should be invoked with the `scala-compiler` module on the classpath, easily acheived by using the `scala` shell command.
  |* During compilation of both source directories, and during test execution, `--dotty-library` is on the classpath.
  |* TASTy Test operates in this order:
  |  1. Compile sources in `$src-3$/$package$` with the Dotty compiler in a separate process, using `--dotty-compiler` as the JVM classpath.
  |  2. Compile sources in `$src-2$/$package$` with the Scala 2 compiler, classes compiled in (1) are now on the classpath.
  |  3. Classes with name `/Test.*/` are assumed to be test cases and their main methods are executed sequentially.
  |     - A successful test should print the single line `Suite passed!` and not have any runtime exceptions.
  |
  |Note: Failing tests without a fix should be put in a sibling directory, such as `suspended`, to document that they are incompatible at present.""".stripMargin

  def run(implicit args: Seq[String]): Boolean = process.fold(
    err => {
      val prefix = err match {
        case _: IllegalStateException => ""
        case _                        => s" ${err.getClass.getSimpleName}:"
      }
      System.err.println(s"ERROR:$prefix ${err.getMessage}")
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
      dottyCompiler <- requiredArg("--dotty-compiler")
      src2dir       =  optionalArg("--src-2", "src-2")
      src3dir       =  optionalArg("--src-3", "src-3")
      pkgName       =  optionalArg("--package", "tastytest")
      _             <- tastytest(dottyLibrary, dottyCompiler, src2dir, src3dir, pkgName)
    } yield ()
  }

  def main(args: Array[String]): Unit = sys.exit(if (run(args.toList)) 1 else 0)
}
