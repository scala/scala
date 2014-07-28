package scala.tools.nsc

import scala.reflect.io.AbstractFile
import scala.tools.nsc.util._
//import scala.tools.nsc.classpath.ZipFileIndexClasspath
//import scala.tools.nsc.classpath.ZipFileIndex
//import scala.tools.nsc.classpath.RelativePath

object ZipArchiveBenchmark { // TODO fix or remove this file - it didn't compile because part of classes doesn't exist

  val scalaLibraryJarPath = "/Users/grek/scala/scala-master/build/pack/lib/scala-library.jar"

  def countClasses(classpath: ClassPath[AbstractFile]): Int = {
    classpath.classes.size + classpath.packages.map(countClasses).sum
  }

  def createClasspath: ClassPath[AbstractFile] = {
    val file = new java.io.File(scalaLibraryJarPath)
    val abstractFile = AbstractFile.getDirectory(file)
    val classpath = new util.DirectoryClassPath(abstractFile, util.ClassPath.DefaultJavaContext)
    classpath
  }

//  def createClasspath2: ClassPath[AbstractFile] = {
//    val file = new java.io.File(scalaLibraryJarPath)
//    ZipFileIndex.clearCache()
//    val index = ZipFileIndex.getZipFileIndex(file, null, false, null, false)
//    new ZipFileIndexClasspath(index, new RelativePath.RelativeDirectory(""), util.ClassPath.DefaultJavaContext)
//  }

  def findList(classpath: ClassPath[AbstractFile]): Boolean = {
    classpath.findClass("scala.collection.immutable.List").isDefined &&
    classpath.findClass("scala.Option").isDefined &&
    classpath.findClass("scala.collection.immutable.Vector").isDefined &&
    classpath.findClass("scala.util.hashing.MurmurHash3").isDefined
    //println(countClasses(classPath))
  }

  def getTopLevelPackages(classpath: ClassPath[AbstractFile]): Seq[String] = {
    classpath.packages.flatMap(_.classes.map(_.name))
  }

  def examineTraditionalClasspath: Boolean = {
    val classpath = createClasspath
    findList(classpath)
  }

//  def examineNewClasspath: Boolean = {
//    val classpath = createClasspath2
//    findList(classpath)
//  }

  def main(args: Array[String]): Unit = {
    def timed[T](f: => T): T = {
      val startTime = System.currentTimeMillis()
      val res = f
      val elapsed = System.currentTimeMillis() - startTime
      println(s"Elapsed $elapsed ms")
      res
    }
//    println(timed(examineTraditionalClasspath))
//    println(timed(examineNewClasspath))

//    val th = ichi.bench.Thyme.warmed(verbose = print)

//    val w = th.Warm(examineTraditionalClasspath)
//    th.pbenchWarm(w)
//    th.pbenchOffWarm()(th.Warm(examineNewClasspath))(w)
  }
}