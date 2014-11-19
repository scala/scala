package scala.tools.nsc.classpath

import scala.tools.nsc.util.ClassPath
import scala.reflect.io.AbstractFile

class FlatClassPathFactory extends ClassPathFactory[FlatClassPath] {
  def expandPath(path: String, expandStar: Boolean = true): List[String] = ClassPath.expandPath(path, expandStar)
  def expandDir(extdir: String): List[String] = ClassPath.expandDir(extdir)

  def createClassPath(file: AbstractFile): FlatClassPath = {
    if (file.hasExtension("jar") || file.hasExtension("zip")) {
      ZipArchiveFlatClassPath.create(file.file)
    } else if (file.isDirectory) {
      new DirectoryFlatClassPath(file.file)
    } else {
      sys.error(s"Unsupported classpath element: $file")
    }
  }


  def sourcesInPath(path: String): List[FlatClassPath] = {
    // TODO: implement properly
    Nil
  }
}
