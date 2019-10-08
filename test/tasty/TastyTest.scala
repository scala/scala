import scala.sys.process._
import java.nio.file.{Files, Paths, Path, DirectoryStream}
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.util.Try

object TastyTest extends App {

  def Which(command: String)    = exec("which", command).filter(!_.contains(" not found"))
  def Coursier(library: String) = exec("echo", "coursier", "fetch", "-p", library).map(_.lazyLines.last.trim)
  def Scalac(dcp: String, sources: String*)  = exec("../../build/quick/bin/scalac" +: sharedOpts(dcp), sources:_*)
  def Dotc(dcp: String, sources: String*)    = exec("dotc" +: sharedOpts(dcp), sources:_*)

  def sharedOpts(dcp: String) = Seq("-d", "out", "-classpath", s"out:$dcp")

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

  def exec(command: String, args: String*): Try[String]      = exec(Seq(command), args:_*)
  def exec(command: Seq[String], args: String*): Try[String] = Try((command :++ args).!!)

  def runTests(dcp: String, names: String*): mutable.ArrayBuffer[String] = {
    val errors = mutable.ArrayBuffer.empty[String]
    for (test <- names) {
      exec("../../build/quick/bin/scala", "-classpath", s"out:$dcp", s"tastytest.$test").fold(
        err => {
          errors += test
          val msg = {
            if (err.getMessage.contains("Nonzero exit value"))
              s"$test failed."
            else
              s"$test failed, error: `${err.getMessage()}`"
          }
          System.err.println(msg)
        },
        content => {
          if (content.trim != "Suite passed!") {
            errors += test
            System.err.println(s"$test failed, unexpected output: `$content`")
          }
        }
      )
    }
    errors
  }

  val program = for {
    _         <- Which("coursier")
    _         <- Which("dotc")
    dcp       <- Coursier("ch.epfl.lamp:dotty-library_0.19:0.19.0-RC1")
    src2      <- getFiles("src-2/tastytest")
    src3      <- getFiles("src-3/tastytest")
    _         <- Scalac(dcp, "src-2/tastytest/package.scala")
    _         <- Dotc(dcp, src3:_*)
    _         <- Scalac(dcp, src2:_*)
    testFiles <- getFiles("src-2/tastytest")
    testNames = testFiles.map(_.stripPrefix("src-2/tastytest/").stripSuffix(".scala")).filter(_.startsWith("Test"))
  } yield runTests(dcp, testNames:_*)

  program.fold(
    err => {
      System.err.println(err.getMessage)
      sys.exit(1)
    },
    failed => {
      if (failed.size > 0) {
        val str = if (failed.size == 1) "error" else "errors"
        println(s"${failed.length} $str. Fix ${failed.mkString(",")}.")
      }
      else {
        println("All passed!")
      }
    }
  )
}
