package scala.tools.nsc

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.reflect.io.PlainNioFile

object FileUtils {
  def assertDirectorySame(dir1: Path, dir2: Path, dir2Label: String): Unit = {
    assert(FileUtils.diff(dir1, dir2), s"Difference detected between recompiling $dir2Label Run:\njardiff -r $dir1 $dir2\n")
  }
  def diff(dir1: Path, dir2: Path): Boolean = {
    def allFiles(dir: Path) = Files.walk(dir).iterator().asScala.map(x => (dir.relativize(x), x)).toList.filter(_._2.getFileName.toString.endsWith(".class")).sortBy(_._1.toString)

    val dir1Files = allFiles(dir1)
    val dir2Files = allFiles(dir2)
    val identical = dir1Files.corresponds(dir2Files) {
      case ((rel1, file1), (rel2, file2)) =>
        rel1 == rel2 && java.util.Arrays.equals(Files.readAllBytes(file1), Files.readAllBytes(file2))
    }
    identical
  }

  def deleteRecursive(f: Path) = new PlainNioFile(f).delete()
  def copyRecursive(src: Path, dest: Path): Unit = {
    class CopyVisitor(src: Path, dest: Path) extends SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.createDirectories(dest.resolve(src.relativize(dir)))
        super.preVisitDirectory(dir, attrs)
      }
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.copy(file, dest.resolve(src.relativize(file)))
        super.visitFile(file, attrs)
      }
    }
    Files.walkFileTree(src, new CopyVisitor(src, dest))
  }
}
