import scala.sys.process._
import java.nio.file.{Files, Paths, Path, DirectoryStream}
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import scala.util.Properties

object TastyTest {

  def run(args: Seq[String]) = for {
    dcp       <- findArg(args, "--dotty-library")
    dccp      <- findArg(args, "--dotty-compiler")
    out       <- tempDir("tastytest")
    src2      <- getFiles("src-2/tastytest")
    src3      <- getFiles("src-3/tastytest")
    _         <- Dotc(out, dcp, dccp, src3:_*)
    _         <- Scalac(out, dcp, src2:_*)
    testFiles <- getFiles("src-2/tastytest")
    testNames =  testFiles.map(_.stripPrefix("src-2/tastytest/").stripSuffix(".scala")).filter(_.startsWith("Test"))
    _         <- runTests(out, dcp, testNames:_*)
  } yield ()

  val scalac = "../../build/quick/bin/scalac"
  val scala  = "../../build/quick/bin/scala"

  def Java(javaArgs: Seq[String], compilerArgs: Seq[String]) = exec("java", (javaArgs ++ compilerArgs):_*)
  def Scalac(out: String, dcp: String, sources: String*) = exec(scalac, (sharedOpts(out, dcp) ++ sources):_*)
  def Dotc(out: String, dcp: String, dccp: String, sources: String*) =
    Java(
      javaArgs     = Seq("-classpath", dccp, "dotty.tools.dotc.Main"),
      compilerArgs = sharedOpts(out, dcp) ++ sources
    )

  def paths(paths: String*) = paths.mkString(":")

  def findArg(args: Seq[String], arg: String) =
    args.sliding(2)
        .filter(_.length == 2)
        .find(_.head == arg)
        .map(_.last)
        .fold[Try[String]](
          Failure(new IllegalArgumentException(s"please provide argument: $arg")))(
          Success(_))

  def sharedOpts(out: String, dcp: String) = Seq("-d", out, "-classpath", paths(out, dcp))

  def getFiles(dir: String) = Try {
    var stream: DirectoryStream[Path] = null
    try {
      stream = Files.newDirectoryStream(Paths.get(dir))
      stream.iterator().asScala.map(_.toString).toSeq
    } finally {
      if (stream != null) {
        stream.close()
      }
    }
  }

  def tempDir(dir: String) = Try(Files.createTempDirectory(dir)).map(_.toString)

  def exec(command: String, args: String*): Try[String] = Try((command +: args).!!)

  def runTests(out: String, dcp: String, names: String*) = {
    val errors = mutable.ArrayBuffer.empty[String]
    for (test <- names) {
      exec(scala, "-classpath", paths(out, dcp), s"tastytest.$test").fold(
        err => {
          errors += test
          val msg = {
            if (err.getMessage.contains("Nonzero exit value"))
              s"ERROR: $test failed."
            else
              s"ERROR: $test failed, error: `${err.getMessage()}`"
          }
          System.err.println(msg)
        },
        content => {
          if (content.trim != "Suite passed!") {
            errors += test
            System.err.println(s"ERROR: $test failed, unexpected output: `$content`")
          }
        }
      )
    }
    if (errors.size > 0) {
      val str = if (errors.size == 1) "error" else "errors"
      Failure(new IllegalStateException(s"${errors.length} $str. Fix ${errors.mkString(",")}."))
    }
    else {
      Success(())
    }
  }

  def main(args: Array[String]): Unit = run(args.toList).fold(
    err => {
      System.err.println(s"ERROR: ${err.getMessage}")
      sys.exit(1)
    },
    _ => {
      println("All passed!")
    }
  )
}
