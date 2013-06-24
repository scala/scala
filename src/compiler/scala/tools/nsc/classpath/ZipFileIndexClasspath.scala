package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.ZipFileIndex.Entry
import java.io.File
import java.io.ByteArrayInputStream
import scala.tools.nsc.classpath.RelativePath.RelativeDirectory
import scala.Array.canBuildFrom
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.asScalaSetConverter
import scala.tools.nsc.util.ClassPath
import javax.tools.JavaFileObject
import scala.tools.nsc.classpath.RelativePath.RelativeFile

class ZipFileIndexClasspath(index: ZipFileIndex, 
      dir: RelativeDirectory, val context: ClassPath.ClassPathContext[AbstractFile]) extends ClassPath[AbstractFile] {
    import scala.collection.JavaConverters._ 
    def asClasspathString: String = index.getZipFile().getAbsolutePath() 
    def asURLs: List[java.net.URL] = ???
    def name: String = dir.basename()
    def classes: IndexedSeq[ClassPath[AbstractFile]#ClassRep] = {
      val files = index.getFiles(dir).asScala.toArray
      val classFiles = files.filter(validClassFile)
      val classReps = classFiles map { classFile =>
        val relativeFile = new RelativePath.RelativeFile(dir, classFile)
        val entry = index.getZipIndexEntry(relativeFile)
        val abstractFile = new ZipFileIndexEntryAbstractFile(index, entry)
        ClassRep(Some(abstractFile), None)
      }
      classReps
    }
    def packages: IndexedSeq[ClassPath[AbstractFile]] = {
      val dirs = if (dir.getPath() == "") {
        val allDirs = index.getAllDirectories().asScala.toArray
        def isTopLevelDir(dir: RelativeDirectory): Boolean = dir.getPath().indexOf('/') == (dir.getPath().length()-1)
        allDirs.filter(isTopLevelDir).map(_.getPath())
      } else index.getDirectories(dir).asScala.toArray
      val packageDirs = dirs.filter(validPackage)
      val packageClassPaths = packageDirs map { packageDir =>
        val nestedDir = new RelativePath.RelativeDirectory(dir, packageDir) 
        new ZipFileIndexClasspath(index, nestedDir, context)
      }
      packageClassPaths
    }
    def sourcepaths: IndexedSeq[scala.tools.nsc.io.AbstractFile] = IndexedSeq.empty
    override def findClass(name: String): Option[ClassRep] = {
      val classFile = new RelativeFile(dir, name.replace('.', '/') + ".class")
      val entry = index.getZipIndexEntry(classFile)
      if (entry == null)
        None
      else {
        val file = new ZipFileIndexEntryAbstractFile(index, entry) 
       Some(ClassRep(Some(file), None))
      }
    }
    
    override def toString(): String = {
      val zipFileName = index.getZipFile().toString()
      zipFileName + ": " + dir
    }
  }

object ZipFileIndexClasspath {
  def create(zipFile: File, context: ClassPath.ClassPathContext[AbstractFile]): ZipFileIndexClasspath = {
    val index = ZipFileIndex.getZipFileIndex(zipFile, null, false, null, false)
    new ZipFileIndexClasspath(index, new RelativePath.RelativeDirectory(""), context)
  }
}

class ZipFileIndexEntryAbstractFile(index: ZipFileIndex, entry: Entry) extends AbstractFile {
  assert(entry != null)
  def absolute: scala.reflect.io.AbstractFile = this
  def container: scala.reflect.io.AbstractFile = ???
  def create(): Unit = throw new UnsupportedOperationException 
  def delete(): Unit = throw new UnsupportedOperationException 
  def file: java.io.File = null 
  lazy val input: java.io.InputStream = {
    new ByteArrayInputStream(index.read(entry))
  } 
  def isDirectory: Boolean = false 
  def iterator: Iterator[scala.reflect.io.AbstractFile] = Iterator.empty 
  def lastModified: Long = entry.getLastModified()
  def lookupName(name: String,directory: Boolean): scala.reflect.io.AbstractFile = null 
  def lookupNameUnchecked(name: String,directory: Boolean): scala.reflect.io.AbstractFile = null 
  def name: String = entry.getFileName()
  def output: java.io.OutputStream = throw new UnsupportedOperationException
  def path: String = entry.getName()
}
