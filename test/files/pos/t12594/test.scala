// scalac: -Werror -Wunused

import java.nio.file.Path
import java.nio.file.FileSystem
import java.net.URI
import java.nio.file.LinkOption
import java.nio.file.{WatchKey, WatchService}
import java.nio.file.WatchEvent.{Kind, Modifier}

class FooPath extends Path {

  override def getFileSystem(): FileSystem = default[FileSystem]

  override def isAbsolute(): Boolean = maybe

  override def getRoot(): Path = somePath

  override def getFileName(): Path = somePath

  override def getParent(): Path = somePath

  override def getNameCount(): Int = number

  override def getName(x$1: Int): Path = somePath

  override def subpath(x$1: Int, x$2: Int): Path = somePath

  override def startsWith(x$1: Path): Boolean = maybe

  override def endsWith(x$1: Path): Boolean = maybe

  override def normalize(): Path = somePath

  override def resolve(x$1: Path): Path = somePath

  override def relativize(x$1: Path): Path = somePath

  override def toUri(): URI = default[URI]

  override def toAbsolutePath(): Path = somePath

  override def toRealPath(x$1: LinkOption*): Path = somePath

  override def register(x$1: WatchService, x$2: Array[Kind[_ <: Object]], x$3: Modifier*): WatchKey = someKey

  override def compareTo(x$1: Path): Int = number

  def someKey: WatchKey = default[WatchKey]
  def somePath: Path = default[Path]
  def maybe: Boolean = false
  def number: Int = 42
  def default[A]: A = null.asInstanceOf[A]

  // default methods
  override def endsWith(x$1: String): Boolean = ??? // String does not match java.nio.file.Path in `override def endsWith(x$1: java.nio.file.Path): Boolean`
  override def iterator(): java.util.Iterator[java.nio.file.Path] = ???
  override def register(x$1: java.nio.file.WatchService, x$2: java.nio.file.WatchEvent.Kind[_]*): java.nio.file.WatchKey = ???
  override def resolve(x$1: String): java.nio.file.Path = ??? // String does not match java.nio.file.Path in `override def resolve(x$1: java.nio.file.Path): java.nio.file.Path`
  override def resolveSibling(x$1: String): java.nio.file.Path = ???
  override def resolveSibling(x$1: java.nio.file.Path): java.nio.file.Path = ???
  override def startsWith(x$1: String): Boolean = ??? // String does not match java.nio.file.Path in `override def startsWith(x$1: java.nio.file.Path): Boolean`
  override def toFile(): java.io.File = ???
}
