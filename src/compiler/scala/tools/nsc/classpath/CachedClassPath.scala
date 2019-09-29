package scala.tools.nsc.classpath

import java.net.URL
import java.util.concurrent.ConcurrentHashMap

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{ClassPath, ClassRepresentation, EfficientClassPath}

class CachedClassPath(underlying: ClassPath, val name:String) extends EfficientClassPath {
  override private[nsc] def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit = {
    val entries = forPackage(inPackage)
    entries.packages foreach onPackageEntry
    entries.classesAndSources foreach onClassesAndSources
  }

  override private[nsc] def hasPackage(inPackage: PackageName) = !forPackage(inPackage).isEmpty
  override private[nsc] def packages(inPackage: PackageName) = forPackage(inPackage).packages

  override private[nsc] def classes(inPackage: PackageName) = forPackage(inPackage).classes
  override private[nsc] def clazz(inPackage: PackageName, className: String) =
    forPackage(inPackage).classesAndSourcesByName.get(className).collect{case cls:ClassFileEntry => cls}

  override private[nsc] def sources(inPackage: PackageName) = forPackage(inPackage).sources
  override private[nsc] def source(inPackage: PackageName, className: String) =
    forPackage(inPackage).classesAndSourcesByName.get(className).collect{case cls:SourceFileEntry => cls}

  override def findClassFile(className: String): Option[AbstractFile] = {
    val (inPackage, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    forPackage(PackageName(inPackage)).classesAndSourcesByName.get(simpleClassName) flatMap (_.binary)
  }

  override lazy val asURLs: Seq[URL] = underlying.asURLs
  override lazy val asClassPathStrings: Seq[String] = underlying.asClassPathStrings
  override lazy val asSourcePathString: String = underlying.asSourcePathString

  override private[nsc] def list(inPackage: PackageName) = forPackage(inPackage)

  override def findClass(className: String): Option[ClassRepresentation] = {
    val (inPackage, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    forPackage(PackageName(inPackage)).classesAndSourcesByName.get(simpleClassName)
  }

  override def asClassPathString: String = super.asClassPathString

  def forPackage(inPackage: PackageName): ClassPathEntries = packageCache.computeIfAbsent(inPackage, p => underlying.list(p))


  private val packageCache = new ConcurrentHashMap[PackageName, ClassPathEntries]()

}
