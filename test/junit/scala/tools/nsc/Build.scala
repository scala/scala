package scala.tools.nsc

import java.io.File
import java.nio.charset.Charset
import java.nio.file.{Files, Path}

import scala.collection.JavaConverters._
import scala.collection.mutable
import FileUtils.createDir

final class Build(base: Path, name: String) {

  val buildBase = createDir(base, name)
  val scalacOptions = mutable.ListBuffer[String]()
  final class Project(val name: String) {
    def fullName: String = Build.this.name + "." + name
    val base = createDir(buildBase, name)
    val out = createDir(base, "target")
    val src = createDir(base, "src")
    val scalacOptions = mutable.ListBuffer[String]()
    scalacOptions += "-usejavacp"
    val sources = mutable.ListBuffer[Path]()
    object classpath {
      val value = mutable.ListBuffer[String]()
      def +=(path: Path): classpath.type = {
        value += path.toString
        this
      }
      def +=(path: String): classpath.type = {
        value += path
        this
      }
    }
    def withSource(relativePath: String)(code: String): this.type = {
      val srcFile = src.resolve(relativePath)
      Files.createDirectories(srcFile.getParent)
      Files.write(srcFile, code.getBytes(Charset.defaultCharset()))
      sources += srcFile
      this
    }
    def argsFile(extraOpts: List[String] = Nil, printArgs: Boolean = false): Path = {
      val cp = List("-cp", if (classpath.value.isEmpty) "__DUMMY__" else classpath.value.mkString(File.pathSeparator)) // Dummy to avoid default classpath of "."
      val printArgsOpt = if (printArgs) List("-Xprint-args", "-") else Nil
      val entries = List(
        Build.this.scalacOptions.toList,
        scalacOptions.toList,
        extraOpts,
        printArgsOpt,
        List("-d", out.toString) ::: cp ::: sources.toList.map(_.toString)
      ).flatten
      Files.write(out.resolve(fullName + ".args"), entries.asJava)
    }
  }
  private val projectsMap = mutable.LinkedHashMap[String, Project]()
  def projects: List[Project] = projectsMap.valuesIterator.toList
  def project(name: String): Project = {
    projectsMap.getOrElseUpdate(name, new Project(name))
  }
}
