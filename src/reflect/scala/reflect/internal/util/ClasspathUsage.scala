package scala.reflect.internal.util

import java.io.{File, FileWriter}

import scala.collection.immutable
import scala.collection.mutable
import scala.reflect.io.AbstractFile

/**
  * Records usage of jars during the compilation.
  *
  * Stores used jars in `deps-scope.txt` file, full classpath in `classpath-scope.txt` and diff in `diff-scope.txt`,
  * all placed in output dir.
  */
class ClasspathUsage() {

  private val usedJars = mutable.Set.empty[String]

  private def isJarPath(path: String) = path.endsWith(".jar")

  def register(classfile: AbstractFile): Unit = {
    classfile.underlyingSource match {
      case Some(file) if isJarPath(file.path) => usedJars += file.path
      case _ =>
    }
  }

  def diff(classpath: String): ClasspathUsage.Jars = {
    val declaredJars = classpath.split(File.pathSeparator).toSet.filter(isJarPath)
    ClasspathUsage.Jars(usedJars.toSet, declaredJars.toSet)
  }

  def store(parent: File, scope: String, jars: ClasspathUsage.Jars): Unit = {
    def store(fileName: String, body: Iterable[String]) = {
      val file = new File(parent, fileName + "-" + scope + ".txt")
      val fw = new FileWriter(file)
      try fw.append(body.toList.sorted.mkString("\n")) finally fw.close()
    }

    store("jars-used", jars.used)
    store("jars-path", jars.declared)
    store("jars-diff", jars.diff)
  }

}

object ClasspathUsage {
  case class Jars(used: immutable.Set[String], declared: immutable.Set[String]) {
    def diff: immutable.Set[String] = declared -- used
  }
}