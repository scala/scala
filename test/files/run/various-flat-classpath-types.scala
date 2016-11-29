/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */

import java.io.{File => JFile, FileInputStream, FileOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.reflect.io.{Directory, File}
import scala.tools.nsc.util.ClassPath.RootPackage
import scala.tools.nsc.classpath.PackageNameUtils
import scala.tools.nsc.io.Jar

/**
 * Generates directories, jars and zip files containing sources and classes
 * (the result of a compilation which is executed here)
 * and use them as a class- and sourcepath during compilation and running
 * created application. At the end everything is cleaned up.
 *
 * It can test also current, recursive classpath. Just right now we force
 * flat classpath to test it also when the recursive one would be set as a default.
 */
object Test {

  private implicit class JFileOps(file: JFile) {

    def createDir(newDirName: String) = {
      val newDir = new JFile(file, newDirName)
      newDir.mkdir()
      newDir
    }

    def createSrcFile(newFileName: String) = createFile(newFileName + ".scala")

    def createFile(fullFileName: String) = {
      val newFile = new JFile(file, fullFileName)
      newFile.createNewFile()
      newFile
    }

    def writeAll(text: String): Unit = File(file) writeAll text

    def moveContentToZip(zipName: String): Unit = {
      val newZip = zipsDir createFile s"$zipName.zip"
      val outputStream = new ZipOutputStream(new FileOutputStream(newZip))

      def addFileToZip(dirPrefix: String = "")(fileToAdd: JFile): Unit =
        if (fileToAdd.isDirectory) {
          val dirEntryName = fileToAdd.getName + "/"
          outputStream.putNextEntry(new ZipEntry(dirEntryName))
          fileToAdd.listFiles() foreach addFileToZip(dirEntryName)
        } else {
          val inputStream = new FileInputStream(fileToAdd)
          outputStream.putNextEntry(new ZipEntry(dirPrefix + fileToAdd.getName))

          val buffer = new Array[Byte](1024)
          var count = inputStream.read(buffer)
          while (count > 0) {
            outputStream.write(buffer, 0, count)
            count = inputStream.read(buffer)
          }

          inputStream.close()
        }

      file.listFiles() foreach addFileToZip()
      outputStream.close()

      cleanDir(file)
    }

    def moveContentToJar(jarName: String): Unit = {
      val newJar = jarsDir createFile s"$jarName.jar"
      Jar.create(file = File(newJar), sourceDir = Directory(file), mainClass = "won't be used")
      cleanDir(file)
    }

    def path: String = file.getAbsolutePath
  }

  private case class DirRep(name: String, nestedDirs: Seq[DirRep] = Nil, sourceFiles: Seq[String] = Nil)

  private val compiler = new scala.tools.nsc.MainClass
  private val appRunner = new scala.tools.nsc.MainGenericRunner
  private val javaClassPath = sys.props("java.class.path")

  // creates a test dir in a temporary dir containing compiled files of this test
  // root dir will be automatically deleted after the end of test
  private val rootDir = new JFile(sys.props("partest.output"))
  private val testDir = rootDir createDir s"cp-tests-${System.currentTimeMillis()}"

  private val jarsDir = testDir createDir "jars"
  private val zipsDir = testDir createDir "zips"
  private val srcDir = testDir createDir "src"
  private val binDir = testDir createDir "bin"
  private val outDir = testDir createDir "out"

  def main(args: Array[String]): Unit = {
    createClassesZipInZipsDir()
    createClassesJarInJarsDir()
    createClassesInBinDir()
    createSourcesZipInZipsDir()
    createSourcesJarInJarsDir()
    createSourcesInSrcDir()
    compileFinalApp()
    runApp()
    // at the end all created files will be deleted automatically
  }

  private def createClassesZipInZipsDir(): Unit = {
    val baseFileName = "ZipBin"
    createStandardSrcHierarchy(baseFileName)
    compileSrc(baseFileName)
    outDir moveContentToZip "Bin"
    cleanDir(srcDir)
  }

  private def createClassesJarInJarsDir(): Unit = {
    val baseFileName = "JarBin"
    createStandardSrcHierarchy(baseFileName)
    compileSrc(baseFileName)
    outDir moveContentToJar "Bin"
    cleanDir(srcDir)
  }

  private def createClassesInBinDir(): Unit = {
    val baseFileName = "DirBin"
    createStandardSrcHierarchy(baseFileName)
    compileSrc(baseFileName, destination = binDir)
    cleanDir(srcDir)
  }

  private def createSourcesZipInZipsDir(): Unit = {
    createStandardSrcHierarchy(baseFileName = "ZipSrc")
    srcDir moveContentToZip "Src"
  }

  private def createSourcesJarInJarsDir(): Unit = {
    createStandardSrcHierarchy(baseFileName = "JarSrc")
    srcDir moveContentToJar "Src"
  }

  private def createSourcesInSrcDir(): Unit = {
    createStandardSrcHierarchy(baseFileName = "DirSrc")

    val appFile = srcDir createSrcFile "Main"
    appFile writeAll s"""import nested._
         | object Main extends App {
         |   println(new ZipBin)
         |   println(new JarBin)
         |   println(new DirBin)
         |   println(new ZipSrc)
         |   println(new JarSrc)
         |   println(new DirSrc)
         |
         |   println(new NestedZipBin)
         |   println(new NestedJarBin)
         |   println(new NestedDirBin)
         |   println(new NestedZipSrc)
         |   println(new NestedJarSrc)
         |   println(new NestedDirSrc)
         | }
       """.stripMargin
  }

  private def compileFinalApp(): Unit = {
    val classPath = mkPath(javaClassPath, binDir.path, zipsDir.path + "/Bin.zip", jarsDir.path + "/Bin.jar")
    val sourcePath = mkPath(srcDir.path, zipsDir.path + "/Src.zip", jarsDir.path + "/Src.jar")

    compiler.process(Array("-cp", classPath, "-sourcepath", sourcePath,
      "-d", outDir.path, s"${srcDir.path}/Main.scala"))
  }

  private def runApp(): Unit = {
    val classPath = mkPath(javaClassPath, outDir.path, binDir.path, zipsDir.path + "/Bin.zip", jarsDir.path + "/Bin.jar")
    appRunner.process(Array("-cp", classPath, "Main"))
  }

  private def createStandardSrcHierarchy(baseFileName: String): Unit =
    createSources(RootPackage, srcDir,
      DirRep("",
        nestedDirs = Seq(DirRep("nested", sourceFiles = Seq("Nested" + baseFileName))),
        sourceFiles = Seq(baseFileName)
      )
    )

  private def createSources(pkg: String, dirFile: JFile, dirRep: DirRep): Unit = {
    dirRep.nestedDirs foreach { rep =>
      val nestedDir = dirFile createDir rep.name
      val nestedPkg = PackageNameUtils.packagePrefix(pkg) + rep.name
      createSources(nestedPkg, nestedDir, rep)
    }

    val pkgHeader = if (pkg == RootPackage) "" else s"package $pkg\n\n"
    dirRep.sourceFiles foreach { srcName =>
      val text = s"""${pkgHeader}case class $srcName(x: String = "")"""
      val srcFile = dirFile createSrcFile srcName
      srcFile writeAll text
    }
  }

  private def compileSrc(baseFileName: String, destination: JFile = outDir): Unit = {
    val srcDirPath = srcDir.path
    compiler.process(Array("-cp", javaClassPath, "-d", destination.path,
      s"$srcDirPath/$baseFileName.scala", s"$srcDirPath/nested/Nested$baseFileName.scala"))
  }

  private def cleanDir(dir: JFile): Unit =
    dir.listFiles().foreach { file =>
      if (file.isDirectory) cleanDir(file)
      file.delete()
    }

  private def mkPath(pathEntries: String*) = pathEntries.mkString(File.pathSeparator)
}
