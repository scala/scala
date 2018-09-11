package scala.tools.nsc.util

import java.net.URL
import java.nio.file._
import java.util

import javax.tools.JavaFileManager.Location
import javax.tools.JavaFileObject.Kind
import javax.tools._

import scala.collection.JavaConverters.{iterableAsScalaIterableConverter, _}
import scala.collection.{immutable, mutable}
import scala.reflect.internal.jpms.ClassOutput.{OutputPathClassOutput, SupplierClassOutput}
import scala.reflect.internal.jpms.FileManagerJava9Api._
import scala.reflect.internal.jpms.{JpmsClasspathSupport, JpmsModuleDescriptor, ResolvedModuleGraph}
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.Settings
import scala.tools.nsc.classpath.PackageNameUtils.separatePkgAndClassNames
import scala.tools.nsc.classpath._
import scala.tools.nsc.io.AbstractFile

final class JpmsClassPath(patches: Map[String, List[String]], private val impl: JpmsClasspathSupport) extends ClassPath {
  private var _sourceModule: JpmsModuleDescriptor = null
  private var _sourceModuleSeen = false // for debugging (see NOTE on moduleGraph below)
  def sourceModule: JpmsModuleDescriptor = {
    _sourceModuleSeen = true
    _sourceModule
  }
  def sourceModule_= (moduleDef: JpmsModuleDescriptor): Unit = {
    assert(!_sourceModuleSeen)
    _sourceModule = moduleDef
  }

  // NOTE: do not touch until `sourceModule` has been set from the parser
  private lazy val _moduleGraph: ResolvedModuleGraph = impl.resolveModuleGraph(sourceModule)

  override private[nsc] def jpmsModuleGraph: Option[ResolvedModuleGraph] = Some(_moduleGraph)

  override def asURLs: Seq[URL] = Nil // TODO
  override def asClassPathStrings: Seq[String] = Nil // TODO
  override def asSourcePathString: String = ""

  private val fileManager = impl.getFileManager
  private def locationAsPaths(location: StandardLocation): IndexedSeq[Path] = getLocationAsPaths(fileManager, location).asScala.toVector
  private val packageIndex = mutable.AnyRefMap[String, PackageEntryImpl]()
  private val moduleLocations = Array(StandardLocation.MODULE_SOURCE_PATH, StandardLocation.UPGRADE_MODULE_PATH, StandardLocation.SYSTEM_MODULES, StandardLocation.MODULE_PATH, StandardLocation.PATCH_MODULE_PATH)
  private val paths = mutable.ArrayBuffer[(Path, StandardLocation, Location)]()
  indexLocations()

  // TODO JPMS It's a bit annoying that we have to start interrogating the classpath before we've resolve the module
  //      graph: we have to assume that all modules are potentially on the module path. It seems pretty hard to
  //      change our initialization order though: `new Run() -> Definitions.init -> PackageLoader.doComplete -> ClassPath.list`
  //      happens before we even have a list of source files, which might include a module-info.java file, which is
  //      relevant to the module graph construction.
  private def indexLocations() {
    for {
      moduleLocation <- moduleLocations
      location1 <- listLocationsForModules(fileManager, moduleLocation).asScala
      location <- location1.asScala
      path <- getLocationAsPaths(fileManager, location).asScala
    } {
      paths.addOne((path, moduleLocation, location))
    }
    val cpLocation = StandardLocation.CLASS_PATH
    for (path <- getLocationAsPaths(fileManager, cpLocation).asScala)
      paths.addOne((path, cpLocation, cpLocation))
  }

  private[nsc] def hasPackage(pkg: String): Boolean = {
    throw new UnsupportedOperationException()
  }

  private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    val seen = mutable.Set[String]()
    val result = immutable.ArraySeq.untagged.newBuilder[PackageEntry]
    for {
      (path, standardLocation, location) <- paths
    } {
      // We use NIO path API to list classpath/modulepath elements, rather than FileManager.list which
      // only returns files, not sub-directories. The only way to find sub-directories with FileManager.list
      // is to set `recurse = true`, which needlessly creates JavaFileObject and other metadata for the everything
      // in the classpath.
      val directory = path.resolve(inPackage.replaceAllLiterally(".", path.getFileSystem.getSeparator))
      if (Files.isDirectory(directory)) {
        val listing = Files.list(directory).iterator().asScala
        for (file <- listing) {
          if (Files.isDirectory(file)) {
            val fileName = file.getFileName.toString
            if (!seen(fileName) && !fileName.contains("-")) {
              seen += fileName
              val name = if (inPackage == "") fileName else inPackage + "." + fileName
              result += PackageEntryImpl(name)
            }
          }
        }
      }
    }
    result.result()
  }

  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = {
    val result = immutable.ArraySeq.untagged.newBuilder[ClassFileEntry]
    for {
      (path, standardLocation, location) <- paths
    } {
      val moduleName = if (isModuleOrientedLocation(standardLocation))
        inferModuleName(fileManager, location)
      else ""
      for (jfo <- fileManager.list(location, inPackage, util.EnumSet.of(JavaFileObject.Kind.CLASS), false).asScala) {
        val binaryName = fileManager.inferBinaryName(location, jfo)
        val path = asPath(fileManager, jfo)
        result += JpmsClassFileEntryImpl(new PlainNioFile(path), moduleName, standardLocation)
      }
    }
    result.result()
  }

  override private[nsc] def list(inPackage: String) = ClassPathEntries(packages(inPackage), classes(inPackage))

  private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = Nil

  override def findClassFile(className: String): Option[AbstractFile] = {
    val (inPackage, classSimpleName) = separatePkgAndClassNames(className)
    val pathsIterator = paths.iterator
    while (pathsIterator.hasNext) {
      val (path, standardLocation, location) = pathsIterator.next()
      val it = fileManager.list(location, inPackage, util.EnumSet.of(JavaFileObject.Kind.CLASS), false).iterator()
      while (it.hasNext) {
        val jfo = it.next()
        if (jfo.isNameCompatible(classSimpleName, Kind.CLASS))
          return Some(new PlainNioFile(asPath(fileManager, jfo)))
      }
    }
    None
  }

}

object JpmsClassPath {
  def apply(s: Settings): ClassPath = {
    val javaOptions = new java.util.ArrayList[java.util.List[String]]()

    def add(optName: String, args: String*): Unit = {
      javaOptions.add((optName +: args).asJava)
    }

    if (s.modulePath.isSetByUser) {
      add("--module-path", s.modulePath.value)
    }
    var allPatches: Map[String, List[String]] = Map.empty
    if (s.patchModule.isSetByUser) {
      def parse(s: String) = s match {
        case EqualsPattern(module, patch) => (module, patch)
        case _ => throw new IllegalArgumentException(s"Unable to parse argument $s")
      }

      allPatches = s.patchModule.value.map(parse).groupBy(_._1).mapValues(_.map(_._2)).to(Map)
      for ((module, patches) <- allPatches) {
        add("--patch-module", s"$module=${patches.mkString(",")}")
      }
    }
    add("--class-path", s.classpath.value)
    val releaseOptional = java.util.Optional.ofNullable(s.release.value).filter(!_.isEmpty)
    val singleOutput = s.outputDirs.getSingleOutput.get
    val classOutput = if (singleOutput.file != null)
      new OutputPathClassOutput(singleOutput.file.toPath)
    else {
      val supplier = () => {
        val file = singleOutput.lookupName("module-info.class", false)
        if (file == null) null
        else file.toByteArray
      }
      new SupplierClassOutput(() => supplier())
    }
    val impl = new JpmsClasspathSupport(releaseOptional, classOutput, javaOptions, s.addModules.value.asJava, s.addExports.value.asJava, s.addReads.value.asJava)
    // TODO JPMS refactor this classpath so that settings are re-read on subsequent runs. Maybe currentClasspath should just be recreated on each new Run?
    new JpmsClassPath(allPatches, impl)
  }

  private val EqualsPattern = """(.*)=(.*)""".r
}

private[nsc] case class JpmsClassFileEntryImpl(file: AbstractFile, override val jpmsModuleName: String, location: StandardLocation) extends ClassFileEntry {
  override val name = FileUtils.stripClassExtension(file.name) // class name

  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}
