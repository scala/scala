/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import org.junit.Assert._
import org.junit._
import org.junit.rules.TemporaryFolder
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.annotation.tailrec
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.Settings
import scala.tools.util.FlatClassPathResolver
import scala.tools.util.PathResolver

@RunWith(classOf[JUnit4])
class FlatClassPathResolverTest {

  val tempDir = new TemporaryFolder()

  private val packagesToTest = List(FlatClassPath.RootPackage, "scala", "scala.reflect", "scala.reflect.io")
  private val classFilesToFind = List("scala.tools.util.FlatClassPathResolver",
    "scala.reflect.io.AbstractFile",
    "scala.collection.immutable.List",
    "scala.Option",
    "scala.collection.immutable.Vector",
    "scala.util.hashing.MurmurHash3",
    "java.lang.Object",
    "java.util.Date")

  private val classesToFind = classFilesToFind ++ List("TestSourceInRootPackage",
    "scala.reflect.io.TestScalaSource",
    "scala.reflect.io.TestJavaSource")

  private val settings = new Settings

  @Before
  def initTempDirAndSourcePath: Unit = {
    // In Java TemporaryFolder in JUnit is managed automatically using @Rule.
    // It would work also in Scala after adding and extending a class like
    // TestWithTempFolder.java containing it. But in this case it doesn't work when running tests
    // from the command line - java class is not compiled due to some, mysterious reasons.
    // That's why such dirs are here created and deleted manually.
    tempDir.create()
    tempDir.newFile("TestSourceInRootPackage.scala")
    val ioDir = tempDir.newFolder("scala", "reflect", "io")
    new File(ioDir, "AbstractFile.scala").createNewFile()
    new File(ioDir, "ZipArchive.java").createNewFile()
    new File(ioDir, "TestScalaSource.scala").createNewFile()
    new File(ioDir, "TestJavaSource.java").createNewFile()

    settings.usejavacp.value = true
    settings.sourcepath.value = tempDir.getRoot.getAbsolutePath
  }

  @After
  def deleteTempDir: Unit = tempDir.delete()

  private def createFlatClassPath(settings: Settings) =
    new FlatClassPathResolver(settings).result

  @Test
  def testEntriesFromListOperationAgainstSeparateMethods: Unit = {
    val classPath = createFlatClassPath(settings)

    def compareEntriesInPackage(inPackage: String): Unit = {
      val packages = classPath.packages(inPackage)
      val classes = classPath.classes(inPackage)
      val sources = classPath.sources(inPackage)
      val FlatClassPathEntries(packagesFromList, classesAndSourcesFromList) = classPath.list(inPackage)

      val packageNames = packages.map(_.name).sorted
      val packageNamesFromList = packagesFromList.map(_.name).sorted
      assertEquals(s"Methods list and packages for package '$inPackage' should return the same packages",
        packageNames, packageNamesFromList)

      val classFileNames = classes.map(_.name).sorted
      val classFileNamesFromList = classesAndSourcesFromList.filter(_.binary.isDefined).map(_.name).sorted
      assertEquals(s"Methods list and classes for package '$inPackage' should return entries for the same class files",
        classFileNames, classFileNamesFromList)

      val sourceFileNames = sources.map(_.name).sorted
      val sourceFileNamesFromList = classesAndSourcesFromList.filter(_.source.isDefined).map(_.name).sorted
      assertEquals(s"Methods list and sources for package '$inPackage' should return entries for the same source files",
        sourceFileNames, sourceFileNamesFromList)

      val uniqueNamesOfClassAndSourceFiles = (classFileNames ++ sourceFileNames).toSet
      assertEquals(s"Class and source entries with the same name obtained via list for package '$inPackage' should be merged into one containing both files",
        uniqueNamesOfClassAndSourceFiles.size, classesAndSourcesFromList.length)
    }

    packagesToTest foreach compareEntriesInPackage
  }

  @Test
  def testCreatedEntriesAgainstRecursiveClassPath: Unit = {
    val flatClassPath = createFlatClassPath(settings)
    val recursiveClassPath = new PathResolver(settings).result

    def compareEntriesInPackage(inPackage: String): Unit = {

      @tailrec
      def traverseToPackage(packageNameParts: Seq[String], cp: ClassPath[AbstractFile]): ClassPath[AbstractFile] = {
        packageNameParts match {
          case Nil => cp
          case h :: t =>
            cp.packages.find(_.name == h) match {
              case Some(nestedCp) => traverseToPackage(t, nestedCp)
              case _ => throw new Exception(s"There's no package $inPackage in recursive classpath - error when searching for '$h'")
            }
        }
      }

      val packageNameParts = if (inPackage == FlatClassPath.RootPackage) Nil else inPackage.split('.').toList
      val recursiveClassPathInPackage = traverseToPackage(packageNameParts, recursiveClassPath)

      val flatCpPackages = flatClassPath.packages(inPackage).map(_.name)
      val pkgPrefix = PackageNameUtils.packagePrefix(inPackage)
      val recursiveCpPackages = recursiveClassPathInPackage.packages.map(pkgPrefix + _.name)
      assertEquals(s"Packages in package '$inPackage' on flat cp should be the same as on the recursive cp",
        recursiveCpPackages, flatCpPackages)

      val flatCpSources = flatClassPath.sources(inPackage).map(_.name).sorted
      val recursiveCpSources = recursiveClassPathInPackage.classes
        .filter(_.source.nonEmpty)
        .map(_.name).sorted
      assertEquals(s"Source entries in package '$inPackage' on flat cp should be the same as on the recursive cp",
        recursiveCpSources, flatCpSources)

      val flatCpClasses = flatClassPath.classes(inPackage).map(_.name).sorted
      val recursiveCpClasses = recursiveClassPathInPackage.classes
        .filter(_.binary.nonEmpty)
        .map(_.name).sorted
      assertEquals(s"Class entries in package '$inPackage' on flat cp should be the same as on the recursive cp",
        recursiveCpClasses, flatCpClasses)
    }

    packagesToTest foreach compareEntriesInPackage
  }

  @Test
  def testFindClassFile: Unit = {
    val classPath = createFlatClassPath(settings)
    classFilesToFind foreach { className =>
      assertTrue(s"File for $className should be found", classPath.findClassFile(className).isDefined)
    }
  }

  @Test
  def testFindClass: Unit = {
    val classPath = createFlatClassPath(settings)
    classesToFind foreach { className =>
      assertTrue(s"File for $className should be found", classPath.findClass(className).isDefined)
    }
  }
}
