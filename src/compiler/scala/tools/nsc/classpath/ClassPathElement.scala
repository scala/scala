package scala.tools.nsc.classpath

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, LinkOption, Path, Paths}

import scala.reflect.io.{ManifestResources, VirtualDirectory}
import scala.tools.nsc.classpath.FileUtils.{endsClass, endsJImage, endsJarOrZip, mayBeValidPackage}
import scala.tools.nsc.util.ClassPath

sealed trait ClassPathElement {

}
object ClassPathElement {

  def fromPathOption(path: Path): Option[PathBasedClassPathElement] = {
    val base = new BasicPathElement(path)
    if (base.isJarOrZip) Some(new ZipJarClassPathElement(base))
    else if (base.isDirectory) Some(new DirectoryClassPathElement(base))
    else if (base.isImage) ??? // Some(new ImagePathElement(base))
    else None
  }

  def fromPathOption(path: String): Option[PathBasedClassPathElement] = fromPathOption(Paths.get(path))

  sealed trait PathBasedClassPathElement extends ClassPathElement {
    def isDirectory = false
    def isJarOrZip = false
    def isImage = false

    private[ClassPathElement] val underlying: BasicPathElement

    def file = path.toFile
    def path = underlying.path
  }
  object JrtClassPathElement$ extends ClassPathElement
  case class VirtualDirectoryClassPathElement(dir: VirtualDirectory) extends ClassPathElement
  case class ManifestClassPathElement(manifest: ManifestResources) extends ClassPathElement


  class ZipJarClassPathElement(private[ClassPathElement] val underlying: BasicPathElement) extends PathBasedClassPathElement {
    override def isJarOrZip: Boolean = true
    assert(underlying.isJarOrZip)

  }

  class DirectoryClassPathElement(private[ClassPathElement] val underlying: BasicPathElement) extends PathBasedClassPathElement {

    override def isDirectory: Boolean = true

    def contents = {
      import scala.collection.JavaConverters._
      Files.list(path).iterator.asScala
    }

    assert(underlying.isDirectory)
  }

  private[ClassPathElement] final class BasicPathElement(val path: Path) {
    import BasicPathElement._

    val attributes = Files.readAttributes(path, classOf[BasicFileAttributes], noLinkOptions: _*)

    def isDirectory: Boolean = attributes.isDirectory
    def isRegularFile: Boolean = attributes.isRegularFile

    lazy val filename = path.getFileName.toString
    lazy val absoluePath = path.toAbsolutePath
    lazy val uri = path.toUri
    lazy val url = uri.toURL

    def isClass: Boolean = isRegularFile && endsClass(filename)
    def isJarOrZip: Boolean = isRegularFile && endsJarOrZip(filename)
    def isImage: Boolean = isRegularFile && endsJImage(filename)

    def isPackage: Boolean = isDirectory && mayBeValidPackage(filename)
  }
  private object BasicPathElement {
    private val noLinkOptions = new Array[LinkOption](0)
  }
}
