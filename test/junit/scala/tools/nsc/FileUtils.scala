package scala.tools.nsc

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import difflib.DiffUtils

import scala.collection.JavaConverters.{asScalaBufferConverter, asScalaIteratorConverter}
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.backend.jvm.AsmUtils

object FileUtils {
  def createDir(dir: Path, s: String): Path = {
    val subDir = dir.resolve(s)
    Files.createDirectories(subDir)
  }

  def assertDirectorySame(dir1: Path, dir2: Path, dir2Label: String): Unit = {
    val diffs = FileUtils.diff(dir1, dir2)
    def diffText = {
      val builder = new java.lang.StringBuilder
      var showDetail = 1 // limit printing of diff to first class
      diffs.foreach { diff =>
        val showDiff = {
          try showDetail > 0
          finally showDetail -= 1
        }
        diff.diffString(builder, showDiff)
      }
      builder.toString
    }
    assert(diffs.isEmpty, s"Difference detected between recompiling $dir2Label Run:\njardiff -r $dir1 $dir2\n$diffText")
  }
  sealed abstract class Diff(path: Path) {
    def diffString(builder: java.lang.StringBuilder, showDiff: Boolean): Unit = builder.append(toString)
  }
  final case class ContentsDiffer(relativePath: Path, path1: Path, path2: Path, left: Array[Byte], right: Array[Byte]) extends Diff(relativePath) {
    override def toString: String = {
      s"ContentsDiffer($relativePath)"
    }
    override def diffString(builder: java.lang.StringBuilder, showDiff: Boolean): Unit = {
      builder.append(productPrefix).append("(").append(relativePath).append(")")
      if (relativePath.getFileName.toString.endsWith(".class")) {
        if (showDiff) {
          val class1 = AsmUtils.readClass(path1.toFile.getAbsolutePath)
          val class2 = AsmUtils.readClass(path2.toFile.getAbsolutePath)
          val text1 = AsmUtils.textify(class1)
          val text2 = AsmUtils.textify(class2)
          builder.append(unifiedDiff(path1, path2, text1, text2))
        } else {
          builder.append("[diff suppressed for brevity]")
        }
      }
    }
  }

  final case class Missing(relativePath: Path, foundPath: Path) extends Diff(relativePath)

  def diff(dir1: Path, dir2: Path): List[Diff] = {
    val diffs = collection.mutable.ListBuffer[Diff]()
    def allFiles(dir: Path): Map[Path, Map[String, Path]] = {
      val classFiles: List[(Path, Path)] = Files.walk(dir).iterator().asScala.map(x => (dir.relativize(x), x)).toList.filter(_._2.getFileName.toString.endsWith(".class")).toList
      classFiles.groupBy(_._1).mapValues(ps => ps.map { case (_, p) => (p.getFileName.toString, p)}.toMap).toMap
    }
    val dir1Files = allFiles(dir1)
    val dir2Files = allFiles(dir2)
    val allSubDirs = dir1Files.keySet ++ dir2Files.keySet
    for (subDir <- allSubDirs.toList.sortBy(_.iterator().asScala.map(_.toString).toIterable)) {
      val files1 = dir1Files.getOrElse(subDir, Map.empty)
      val files2 = dir2Files.getOrElse(subDir, Map.empty)
      val allFileNames = files1.keySet ++ files2.keySet
      for (name <- allFileNames.toList.sorted) {
        (files1.get(name), files2.get(name)) match {
          case (Some(file1), Some(file2)) =>
            val bytes1 = Files.readAllBytes(file1)
            val bytes2 = Files.readAllBytes(file2)
            if (!java.util.Arrays.equals(bytes1, bytes2)) {
              diffs += ContentsDiffer(dir1.relativize(file1), file1, file2, bytes1, bytes2)
            }
          case (Some(file1), None) =>
            val relativePath = file1.relativize(dir1)
            diffs += Missing(relativePath, file1)
          case (None, Some(file2)) =>
            val relativePath = file2.relativize(dir2)
            diffs += Missing(relativePath, file2)
          case (None, None) =>
            throw new IllegalStateException()
        }
      }
    }
    diffs.toList
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

  private def unifiedDiff(path1: Path, path2: Path, text1: String, text2: String) = {
    def lines(s: String) = {
      val result = new java.util.ArrayList[String]()
      s.linesIterator.foreach(result.add)
      result
    }

    val lines1 = lines(text1)
    val lines2 = lines(text2)
    val patch = DiffUtils.diff(lines1, lines2)
    val value = DiffUtils.generateUnifiedDiff(path1.toString, path2.toString, lines1, patch, 10)
    val diffToString = value.asScala.mkString("\n")
    diffToString
  }
}
