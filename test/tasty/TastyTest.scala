import scala.sys.process._
import java.nio.file.{Files, Paths, Path, DirectoryStream}
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.util.Try

object TastyTest extends App {

  val sharedOpts = Seq("-d", "out", "-classpath", "out")
  val Scalac = "../../build/quick/bin/scalac" +: sharedOpts
  val Dotc   = "dotc" +: sharedOpts

  def getFiles(dir: String): Seq[String] = {
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

  try {
    val coursier = "which coursier".!!
    if (coursier.contains(" not found")) {
      System.err.println(coursier)
      sys.exit(1)
    }
    val dotc = "which dotc".!!
    if (dotc.contains(" not found")) {
      System.err.println(dotc)
      sys.exit(1)
    }
    val dcp = "echo coursier fetch -p ch.epfl.lamp:dotty-library_0.19:0.19.0-RC1".!!.lazyLines.last.trim
    val _1 = (Scalac :+ "src-2/tastytest/package.scala").!
    if (_1 == 0) {
      val _2 = (Dotc :++ getFiles("src-3/tastytest")).!
      if (_2 == 0) {
        val _3 = (Scalac :++ getFiles("src-2/tastytest")).!
        if (_3 == 0) {
          val testNames = {
            getFiles("src-2/tastytest")
              .map(_.stripPrefix("src-2/tastytest/").stripSuffix(".scala"))
              .filter(_.startsWith("Test"))
          }
          val errors = mutable.ArrayBuffer.empty[String]
          println("compiled all")
          for (test <- testNames) {
            val command = Seq("../../build/quick/bin/scala", "-classpath", s"out:$dcp", s"tastytest.$test")
            val output = Try(command.!!)
            output.fold(
              err => {
                errors += test
                System.err.println(s"$test failed, error: `${err.getMessage()}`")
              },
              content => {
                if (content.trim != "Suite passed!") {
                  errors += test
                  System.err.println(s"$test failed, unexpected output: `$content`")
                }
              }
            )
          }
          if (errors.size > 0) {
            val str = if (errors.size == 1) "error" else "errors"
            println(s"${errors.length} $str. Fix ${errors.mkString(",")}.")
            sys.exit(1)
          }
          else {
            println("All passed!")
          }
        }
        else {
          sys.exit(_3)
        }
      }
      else {
        sys.exit(_2)
      }
    }
    else {
      sys.exit(_1)
    }
  } catch {
    case e: java.io.IOException =>
      System.err.println(e.getMessage)
      sys.exit(1)
  }
}
