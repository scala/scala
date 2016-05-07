/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.net.URL
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.reflect.io.VirtualFile
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.ClassPath

/**
 * Tests whether AggregateFlatClassPath returns correct entries taken from
 * cp instances used during creating it and whether it preserves the ordering
 * (in the case of the repeated entry for a class or a source it returns the first one).
 */
@RunWith(classOf[JUnit4])
class AggregateClassPathTest {

  private abstract class TestClassPathBase extends ClassPath {
    override def packages(inPackage: String): Seq[PackageEntry] = unsupported
    override def sources(inPackage: String): Seq[SourceFileEntry] = unsupported
    override def classes(inPackage: String): Seq[ClassFileEntry] = unsupported

    override def list(inPackage: String): ClassPathEntries = unsupported
    override def findClassFile(name: String): Option[AbstractFile] = unsupported

    override def asClassPathStrings: Seq[String] = unsupported
    override def asSourcePathString: String = unsupported
    override def asURLs: Seq[URL] = unsupported
  }

  private case class TestClassPath(virtualPath: String, classesInPackage: EntryNamesInPackage*) extends TestClassPathBase {

    override def classes(inPackage: String): Seq[ClassFileEntry] =
      for {
        entriesWrapper <- classesInPackage if entriesWrapper.inPackage == inPackage
        name <- entriesWrapper.names
      } yield classFileEntry(virtualPath, inPackage, name)

    override def sources(inPackage: String): Seq[SourceFileEntry] = Nil

    // we'll ignore packages
    override def list(inPackage: String): ClassPathEntries = ClassPathEntries(Nil, classes(inPackage))
  }

  private case class TestSourcePath(virtualPath: String, sourcesInPackage: EntryNamesInPackage*) extends TestClassPathBase {

    override def sources(inPackage: String): Seq[SourceFileEntry] =
      for {
        entriesWrapper <- sourcesInPackage if entriesWrapper.inPackage == inPackage
        name <- entriesWrapper.names
      } yield sourceFileEntry(virtualPath, inPackage, name)

    override def classes(inPackage: String): Seq[ClassFileEntry] = Nil

    // we'll ignore packages
    override def list(inPackage: String): ClassPathEntries = ClassPathEntries(Nil, sources(inPackage))
  }

  private case class EntryNamesInPackage(inPackage: String)(val names: String*)

  private val dir1 = "./dir1"
  private val dir2 = "./dir2"
  private val dir3 = "./dir3"
  private val dir4 = ""

  private val pkg1 = "pkg1"
  private val pkg2 = "pkg2"
  private val pkg3 = "pkg1.nested"
  private val nonexistingPkg = "nonexisting"

  private def unsupported = throw new UnsupportedOperationException

  private def classFileEntry(pathPrefix: String, inPackage: String, fileName: String) =
    ClassFileEntryImpl(classFile(pathPrefix, inPackage, fileName))

  private def sourceFileEntry(pathPrefix: String, inPackage: String, fileName: String) =
    SourceFileEntryImpl(sourceFile(pathPrefix, inPackage, fileName))

  private def classFile(pathPrefix: String, inPackage: String, fileName: String) =
    virtualFile(pathPrefix, inPackage, fileName, ".class")

  private def sourceFile(pathPrefix: String, inPackage: String, fileName: String) =
    virtualFile(pathPrefix, inPackage, fileName, ".scala")

  private def virtualFile(pathPrefix: String, inPackage: String, fileName: String, extension: String) = {
    val packageDirs =
      if (inPackage == ClassPath.RootPackage) ""
      else inPackage.split('.').mkString("/", "/", "")
    new VirtualFile(fileName + extension, s"$pathPrefix$packageDirs/$fileName$extension")
  }

  private def createDefaultTestClasspath() = {
    val partialClassPaths = Seq(TestSourcePath(dir1, EntryNamesInPackage(pkg1)("F", "A", "G")),
      TestClassPath(dir2, EntryNamesInPackage(pkg1)("C", "B", "A"), EntryNamesInPackage(pkg2)("D", "A", "E")),
      TestClassPath(dir3, EntryNamesInPackage(pkg1)("A", "D", "F")),
      TestSourcePath(dir4, EntryNamesInPackage(pkg2)("A", "H", "I"), EntryNamesInPackage(pkg1)("A")),
      TestSourcePath(dir2, EntryNamesInPackage(pkg3)("J", "K", "L"))
    )

    AggregateClassPath(partialClassPaths)
  }

  @Test
  def testGettingPackages: Unit = {
    case class ClassPathWithPackages(packagesInPackage: EntryNamesInPackage*) extends TestClassPathBase {
      override def packages(inPackage: String): Seq[PackageEntry] =
        packagesInPackage.find(_.inPackage == inPackage).map(_.names).getOrElse(Nil) map PackageEntryImpl
    }

    val partialClassPaths = Seq(ClassPathWithPackages(EntryNamesInPackage(pkg1)("pkg1.a", "pkg1.d", "pkg1.f")),
      ClassPathWithPackages(EntryNamesInPackage(pkg1)("pkg1.c", "pkg1.b", "pkg1.a"),
        EntryNamesInPackage(pkg2)("pkg2.d", "pkg2.a", "pkg2.e"))
    )
    val cp = AggregateClassPath(partialClassPaths)

    val packagesInPkg1 = Seq("pkg1.a", "pkg1.d", "pkg1.f", "pkg1.c", "pkg1.b")
    assertEquals(packagesInPkg1, cp.packages(pkg1).map(_.name))

    val packagesInPkg2 = Seq("pkg2.d", "pkg2.a", "pkg2.e")
    assertEquals(packagesInPkg2, cp.packages(pkg2).map(_.name))

    assertEquals(Seq.empty, cp.packages(nonexistingPkg))
  }

  @Test
  def testGettingClasses: Unit = {
    val cp = createDefaultTestClasspath()

    val classesInPkg1 = Seq(classFileEntry(dir2, pkg1, "C"),
      classFileEntry(dir2, pkg1, "B"),
      classFileEntry(dir2, pkg1, "A"),
      classFileEntry(dir3, pkg1, "D"),
      classFileEntry(dir3, pkg1, "F")
    )
    assertEquals(classesInPkg1, cp.classes(pkg1))

    val classesInPkg2 = Seq(classFileEntry(dir2, pkg2, "D"),
      classFileEntry(dir2, pkg2, "A"),
      classFileEntry(dir2, pkg2, "E")
    )
    assertEquals(classesInPkg2, cp.classes(pkg2))

    assertEquals(Seq.empty, cp.classes(pkg3))
    assertEquals(Seq.empty, cp.classes(nonexistingPkg))
  }

  @Test
  def testGettingSources: Unit = {
    val partialClassPaths = Seq(TestClassPath(dir1, EntryNamesInPackage(pkg1)("F", "A", "G")),
      TestSourcePath(dir2, EntryNamesInPackage(pkg1)("C", "B", "A"), EntryNamesInPackage(pkg2)("D", "A", "E")),
      TestSourcePath(dir3, EntryNamesInPackage(pkg1)("A", "D", "F")),
      TestClassPath(dir4, EntryNamesInPackage(pkg2)("A", "H", "I")),
      TestClassPath(dir2, EntryNamesInPackage(pkg3)("J", "K", "L"))
    )
    val cp = AggregateClassPath(partialClassPaths)

    val sourcesInPkg1 = Seq(sourceFileEntry(dir2, pkg1, "C"),
      sourceFileEntry(dir2, pkg1, "B"),
      sourceFileEntry(dir2, pkg1, "A"),
      sourceFileEntry(dir3, pkg1, "D"),
      sourceFileEntry(dir3, pkg1, "F")
    )
    assertEquals(sourcesInPkg1, cp.sources(pkg1))

    val sourcesInPkg2 = Seq(sourceFileEntry(dir2, pkg2, "D"),
      sourceFileEntry(dir2, pkg2, "A"),
      sourceFileEntry(dir2, pkg2, "E")
    )
    assertEquals(sourcesInPkg2, cp.sources(pkg2))

    assertEquals(Seq.empty, cp.sources(pkg3))
    assertEquals(Seq.empty, cp.sources(nonexistingPkg))
  }

  @Test
  def testList: Unit = {
    val cp = createDefaultTestClasspath()

    val classesAndSourcesInPkg1 = Seq(
      ClassAndSourceFilesEntry(classFile(dir3, pkg1, "F"), sourceFile(dir1, pkg1, "F")),
      ClassAndSourceFilesEntry(classFile(dir2, pkg1, "A"), sourceFile(dir1, pkg1, "A")),
      sourceFileEntry(dir1, pkg1, "G"),
      classFileEntry(dir2, pkg1, "C"),
      classFileEntry(dir2, pkg1, "B"),
      classFileEntry(dir3, pkg1, "D")
    )
    assertEquals(classesAndSourcesInPkg1, cp.list(pkg1).classesAndSources)

    assertEquals(ClassPathEntries(Nil, Nil), cp.list(nonexistingPkg))
  }

  @Test
  def testFindClass: Unit = {
    val cp = createDefaultTestClasspath()

    assertEquals(
      Some(ClassAndSourceFilesEntry(classFile(dir2, pkg1, "A"), sourceFile(dir1, pkg1, "A"))),
      cp.findClass(s"$pkg1.A")
    )
    assertEquals(Some(classFileEntry(dir3, pkg1, "D")), cp.findClass(s"$pkg1.D"))
    assertEquals(Some(sourceFileEntry(dir2, pkg3, "L")), cp.findClass(s"$pkg3.L"))
    assertEquals(None, cp.findClass("Nonexisting"))
  }
}
