package scala.tools.nsc
package io

import java.io.{ InputStream, OutputStream, IOException, FileNotFoundException, FileInputStream }
import java.util.jar._
import collection.JavaConverters._
import Attributes.Name

class Jar(file: File) extends Iterable[JarEntry] {
  def this(path: String) = this(File(path))
  protected def errorFn(msg: String): Unit = Console println msg

  lazy val jarFile  = new JarFile(file.jfile)
  lazy val manifest = withJarInput(s => Option(s.getManifest))
  def mainClass     = manifest map (f => f(Name.MAIN_CLASS))

  def withJarInput[T](f: JarInputStream => T): T = {
    val in = new JarInputStream(file.inputStream())
    try f(in)
    finally in.close()
  }
  def jarWriter() = new JarWriter(file)

  override def foreach[U](f: JarEntry => U): Unit = withJarInput { in =>
    Iterator continually in.getNextJarEntry() takeWhile (_ != null) foreach f
  }
  override def iterator: Iterator[JarEntry] = this.toList.iterator
  def fileishIterator: Iterator[Fileish] = jarFile.entries.asScala map (x => Fileish(x, () => getEntryStream(x)))

  private def getEntryStream(entry: JarEntry) = jarFile getInputStream entry match {
    case null   => errorFn("No such entry: " + entry) ; null
    case x      => x
  }
  override def toString = "" + file
}

class JarWriter(file: File, val manifest: Manifest = new Manifest()) {
  private lazy val out = new JarOutputStream(file.outputStream(), manifest)
  def writeAllFrom(dir: Directory) = {
    try dir.list foreach (x => addEntry(x, ""))
    finally out.close()

    file
  }
  private def addFile(entry: File, prefix: String) {
    out putNextEntry new JarEntry(prefix + entry.name)
    try transfer(entry.inputStream(), out)
    finally out.closeEntry()
  }
  private def addEntry(entry: Path, prefix: String) {
    if (entry.isFile) addFile(entry.toFile, prefix)
    else addDirectory(entry.toDirectory, prefix + entry.name + "/")
  }
  private def addDirectory(entry: Directory, prefix: String) {
    entry.list foreach (p => addEntry(p, prefix))
  }
  private def transfer(in: InputStream, out: OutputStream) = {
    val buf = new Array[Byte](10240)
    def loop: Unit = in.read(buf, 0, buf.length) match {
      case -1 => in.close()
      case n  => out.write(buf, 0, n) ; loop
    }
    loop
  }
}

object Jar {
  // See http://download.java.net/jdk7/docs/api/java/nio/file/Path.html
  // for some ideas.
  private val ZipMagicNumber = List[Byte](80, 75, 3, 4)
  private def magicNumberIsZip(f: Path) = f.isFile && (f.toFile.bytes().take(4).toList == ZipMagicNumber)

  def isJarOrZip(f: Path): Boolean = isJarOrZip(f, true)
  def isJarOrZip(f: Path, examineFile: Boolean): Boolean =
    f.hasExtension("zip", "jar") || (examineFile && magicNumberIsZip(f))

  def locateByClass(clazz: Class[_]): Option[File] = {
    try Some(File(clazz.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()))
    catch { case _: Exception => None }
  }
  /** Walks upward from wherever the scala library jar is searching for
   *  the given jar name.  This approach finds the scala library jar in the
   *  release layout and in trunk builds going up from pack.
   */
  def locateByName(name: String): Option[File] = {
    def toSrc(d: Directory) = d.dirs.toList map (_ / name)
    def walk(d: Directory)  = d.parents flatMap toSrc find (_.isFile) map (_.toFile)

    locateByClass(classOf[ScalaObject]) flatMap (x => walk(x.parent))
  }

  def create(file: File, sourceDir: Directory, mainClass: String): File = {
    val writer = new Jar(file).jarWriter()
    writer.manifest(Name.MANIFEST_VERSION) = "1.0"
    writer.manifest(Name.MAIN_CLASS) = mainClass
    writer writeAllFrom sourceDir
  }
}
