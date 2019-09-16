package scala.tools.nsc.classpath

import java.net.URL
import java.util.concurrent.ConcurrentHashMap

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{ClassPath, ClassRepresentation, EfficientClassPath}

class CachedClassPath(underlying: ClassPath) extends EfficientClassPath {
  override private[nsc] def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit = {
    val info = forPackage(inPackage)
    info.entries.packages foreach onPackageEntry
    info.entries.classesAndSources foreach onClassesAndSources
  }

  override private[nsc] def hasPackage(inPackage: PackageName) = forPackage(inPackage).isEmpty
  override private[nsc] def packages(inPackage: PackageName) = forPackage(inPackage).entries.packages

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

  override private[nsc] def list(inPackage: PackageName) = forPackage(inPackage).entries

  override def findClass(className: String): Option[ClassRepresentation] = {
    val (inPackage, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    forPackage(PackageName(inPackage)).classesAndSourcesByName.get(simpleClassName)
  }

  override def asClassPathString: String = super.asClassPathString

  def forPackage(inPackage: PackageName): PackageInfo = ???

  case class PackageInfo(inPackage: String, entries: ClassPathEntries) {
    def isEmpty = entries.packages.isEmpty && entries.classesAndSources.isEmpty

    lazy val classesAndSourcesByName: Map[String, ClassRepresentation] = entries.classesAndSources.map(e => e.name -> e)(collection.breakOut)
    lazy val sources = entries.classesAndSources.collect { case s: SourceFileEntry => s }
    lazy val classes = entries.classesAndSources.collect { case c: ClassFileEntry => c }

  }

  private val packageCache = new ConcurrentHashMap[String, PackageInfo]()

}
