package scala.tools.nsc

import java.nio.file.{Files, Path}
import java.util.regex.Pattern

import org.junit.{After, Before, Test}

import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.FileUtils._
import scala.tools.nsc.settings.{DefaultPathFactory, PathFactory}

class PickleWriteTest {
  private var base: Path = _

  // Enables verbose output to console to help understand what the test is doing.
  private val debug = false
  private var deleteBaseAfterTest = true

  @Before def before(): Unit = {
    base = Files.createTempDirectory("pickleWriteTest")
  }

  @After def after(): Unit = {
    if (base != null && !debug && deleteBaseAfterTest) {
      deleteRecursive(base)
    }
  }

  private def projectsBase = createDir(base, "projects")

  object VirtualFilePathFactory {
    private val prefix = "_MEMORY_"
    def path(name: String) = prefix + "/" + name
    val MemPattern = s"""${Pattern.quote(prefix)}/(.*)""".r
  }
  private class VirtualFilePathFactory extends PathFactory {
    import VirtualFilePathFactory._
    val virtualDirs = new java.util.concurrent.ConcurrentHashMap[String, VirtualDirectory]()
    override def getDirectory(path: String): AbstractFile = path match {
      case MemPattern(ident) => virtualDirs.computeIfAbsent(ident, dirName => new VirtualDirectory(dirName, None))
      case _ => DefaultPathFactory.getDirectory(path)
    }
    override def getFile(path: String): AbstractFile = path match {
      case MemPattern(ident) => virtualDirs.computeIfAbsent(ident, dirName => new VirtualDirectory(dirName, None))
      case _ => DefaultPathFactory.getFile(path)
    }

  }

  @Test
  def testPickleWriteVirtual(): Unit = {
    // Demonstrating how to use a custom PathFactory to keep the .sig files in memory. A real build tool
    // would need to manage merging/deletion of these with the results of prior builds to support incremental compilation.
    val pathFactory = new VirtualFilePathFactory

    val build = new Build(projectsBase, "b1")
    val p1 = build.project("p1")
    val p1ApiVirtual = VirtualFilePathFactory.path("p1")
    p1.scalacOptions ++= List(
      "-Ypickle-write", p1ApiVirtual, // write .sig files to the virtual directory
      "-Ypickle-write-api-only",      // Only export the public API in the .sig files
      "-Ystop-after:pickler"          // Don't bother creating .class files.
    )
    p1.withSource("b1/p1/C.scala")(
      """
        |package b1.p1
        |class C {
        |  private def foo = 42
        |  def bar = ""
        |}
    """.stripMargin)

    val p2 = build.project("p2")
    p2.classpath += p1ApiVirtual
    p2.withSource("b1/p2/Client.scala")(
      """
        |package b1.p2
        |class Client {
        |  new b1.p1.C().bar.charAt(0)
        |}
    """.stripMargin)

    val settings1 = new Settings(Console.println, pathFactory)
    settings1.usejavacp.value = true
    val argsFile1 = p1.argsFile()
    val command1 = new CompilerCommand("@" + argsFile1.toAbsolutePath.toString :: Nil, settings1)
    val global1 = new Global(command1.settings)
    new global1.Run().compile(command1.files)
    assert(!global1.reporter.hasErrors)

    val argsFile2 = p2.argsFile()
    val settings2 = new Settings(Console.println, pathFactory)
    settings2.usejavacp.value = true
    val command2 = new CompilerCommand("@" + argsFile2.toAbsolutePath.toString :: Nil, settings2)
    val global2 = new Global(command2.settings)
    new global2.Run().compile(command2.files)
    assert(!global2.reporter.hasErrors)
  }
}
