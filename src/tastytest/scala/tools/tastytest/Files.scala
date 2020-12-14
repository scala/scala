package scala.tools.tastytest

import scala.io.{ Source, BufferedSource }
import scala.jdk.CollectionConverters._
import scala.util.Try

import java.{ lang => jl, util => ju }
import java.nio.file.{ Files => JFiles, Paths => JPaths, Path => JPath, PathMatcher, FileSystems }
import java.io.{FileNotFoundException, File => JFile}

object Files {

  def globMatcher(str: String): PathMatcher = FileSystems.getDefault.getPathMatcher(s"glob:$str")

  def tempDir(category: String): Try[(String, String => Unit)] =
    Try(JFiles.createTempDirectory(category)).map(f => f.toString -> deleteRecursively(category))

  private def consumeAny[T](t: T): Unit = ()

  def currentDir: String = FileSystems.getDefault.getPath(".").toString

  def dir(dir: String): Try[(String, String => Unit)] = Try {
    val path = JPaths.get(dir)
    if (JFiles.isDirectory(path)) {
      path.normalize.toString -> consumeAny
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

  def deleteRecursively(category: String)(dir: String) = {
    if (scala.reflect.io.Directory(new JFile(dir)).deleteRecursively()) {
      // ok to silence
      log(green(s"deleted recursively dir in category $category: $dir"))
    }
    else {
      // don't silence as temp files could be growing unbounded.
      println(yellow(s"warn: could not delete files recursively in dir in category $category: $dir"))
    }
  }

  val pathSep: String = FileSystems.getDefault.getSeparator

  val classpathSep: String = java.io.File.pathSeparator

}
