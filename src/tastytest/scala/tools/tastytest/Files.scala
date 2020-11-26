package scala.tools.tastytest

import scala.io.{ Source, BufferedSource }
import scala.jdk.CollectionConverters._
import scala.util.Try

import java.{ lang => jl, util => ju }
import java.nio.file.{ Files => JFiles, Paths => JPaths, Path => JPath, PathMatcher, FileSystems }
import java.io.FileNotFoundException

object Files {

  def globMatcher(str: String): PathMatcher = FileSystems.getDefault.getPathMatcher(s"glob:$str")

  def tempDir(dir: String): Try[String] = Try(JFiles.createTempDirectory(dir)).map(_.toString)

  def currentDir: String = FileSystems.getDefault.getPath(".").toString

  def dir(dir: String): Try[String] = Try {
    val path = JPaths.get(dir)
    if (JFiles.isDirectory(path)) {
      path.normalize.toString
    }
    else {
      throw new FileNotFoundException(s"$path is not a directory.")
    }
  }

  def getFiles(dir: String): Try[Seq[String]] = Try {
    var stream: java.util.stream.Stream[JPath] = null
    try {
      stream = JFiles.walk(JPaths.get(dir))
      val files = {
        stream.filter(!JFiles.isDirectory(_))
              .map(_.normalize.toString)
              .iterator
              .asScala
              .toSeq
      }
      if (files.isEmpty) printwarnln(s"Warning: $dir is empty.")
      files
    } finally {
      if (stream != null) {
        stream.close()
      }
    }
  }

  def processLines[A](file: String)(op: ju.stream.Stream[String] => A): A = {
    var stream: java.util.stream.Stream[String] = null
    try {
      stream = JFiles.lines(JPaths.get(file))
      op(stream)
    }
    finally if (stream != null) {
      stream.close()
    }
  }

  def use[T](resource: String)(op: jl.Iterable[String] => Try[T]): Try[T] = Try {
    var source: BufferedSource = null
    try {
      source = Source.fromResource(resource)
      op(() => source.getLines().asJava)
    }
    finally if (source != null) {
      source.close()
    }
  }.flatten

  val pathSep: String = FileSystems.getDefault.getSeparator

  val classpathSep: String = java.io.File.pathSeparator

}
