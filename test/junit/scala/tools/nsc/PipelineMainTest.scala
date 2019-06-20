package scala.tools.nsc

import java.io.{File, IOException}
import java.nio.charset.Charset
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import org.junit.{After, Before, Test}

import scala.collection.JavaConverters._
import scala.collection.mutable
import FileUtils._
import scala.tools.nsc.PipelineMain._
import scala.tools.nsc.reporters.{ConsoleReporter, StoreReporter}

class PipelineMainTest {
  private var base: Path = _

  // Enables verbose output to console to help understand what the test is doing.
  private val debug = false
  private var deleteBaseAfterTest = true

  @Before def before(): Unit = {
    base = Files.createTempDirectory("pipelineBase")
  }

  @After def after(): Unit = {
    if (base != null && !debug && deleteBaseAfterTest) {
      deleteRecursive(base)
    }
  }

  private def projectsBase = createDir(base, "projects")

  @Test def pipelineMainBuildsSeparate(): Unit = {
    check(allBuilds.map(_.projects))
  }

  @Test def pipelineMainBuildsCombined(): Unit = {
    check(List(allBuilds.flatMap(_.projects)))
  }

  private val pipelineSettings = PipelineMain.defaultSettings.copy(
    useJars = true,
    parallelism = java.lang.Runtime.getRuntime.availableProcessors,
    cacheMacro = true,
    cachePlugin = true,
    stripExternalClassPath = true,
    useTraditionalForLeaf = true,
    createReporter = ((s: Settings) => if (debug) new ConsoleReporter(s) else new StoreReporter())
  )

  private def check(projectss: List[List[Build#Project]], altStrategies: List[BuildStrategy] = List(Pipeline)): Unit = {
    def build(strategy: BuildStrategy): Unit = {
      for (projects <- projectss) {
        val argsFiles = projects.map(_.argsFile(Nil))
        val main = new PipelineMainClass(argsFiles, pipelineSettings.copy(strategy = strategy, logDir = Some(base.resolve(strategy.toString))))
        assert(main.process())
      }
    }
    build(Traditional)

    val reference = snapshotClasses(Traditional)
    clean()
    for (strategy <- altStrategies) {
      build(strategy)
      val recompiled = snapshotClasses(strategy)
      // Bytecode should be identical regardless of compilation strategy.
      deleteBaseAfterTest = false
      assertDirectorySame(reference, recompiled, strategy.toString)
      deleteBaseAfterTest = true
    }
  }

  private lazy val allBuilds = List(m1, b2, b3, b4)

  private lazy val m1: Build = {
    val build = new Build(projectsBase, "m1")
    val macroProject = build.project("p1")
    macroProject.withSource("m1/p1/Macro.scala")(
      """
        |package m1.p1
        |import reflect.macros.blackbox.Context, language.experimental._
        |object Macro {
        |  def m: Unit = macro impl
        |  def impl(c: Context): c.Tree = {
        |    import c.universe._
        |    q"()"
        |  }
        |}
      """.stripMargin)
    val internalMacroClient = build.project("internalMacroClient")
    internalMacroClient.scalacOptions ++= List("-Ymacro-classpath", macroProject.out.toString)
    internalMacroClient.classpath += macroProject.out
    internalMacroClient.withSource("m2/p2/InternalClient.scala")(
      """
        |package m1.p2
        |class InternalClient { m1.p1.Macro.m }
      """.stripMargin)
    build
  }

  private lazy val b2: Build = {
    val build = new Build(projectsBase, "b1")
    val p1 = build.project("p1")
    val m1P1 = m1.project("p1")
    p1.classpath += m1P1.out
    p1.scalacOptions ++= List("-Ymacro-classpath", m1P1.out.toString)
    p1.withSource("b1/p1/ExternalClient.scala")(
      """
        |package b2.p2
        |class ExternalClient { m1.p1.Macro.m }
      """.stripMargin)
    build
  }

  private lazy val b3: Build = {
    val build = new Build(projectsBase, "b3")
    val p1 = build.project("p1")
    p1.withSource("b3/p1/JavaDefined.java")(
      """
        |package b3.p1;
        |public class JavaDefined<T> {
        |  ScalaJoint<T> id(T t) { return new ScalaJoint<T>(); }
        |}
      """.stripMargin)
    p1.withSource("b3/p1/ScalaJoint.scala")(
      """
        |package b3.p1
        |class ScalaJoint[T] {
        |  def foo: Unit = new JavaDefined[String]
        |}
      """.stripMargin)
    val p2 = build.project("p2")
    p2.classpath += p1.out
    p2.withSource("b3/p2/JavaClient.java")(
      """
        |package b3.p2;
        |public class JavaClient {
        |  b3.p1.JavaDefined<String> test() { return null; }
        |}
      """.stripMargin)
    p2.withSource("b3/p2/ScalaClient.scala")(
      """
        |package b3.p2
        |class ScalaClient {
        |  def test(): b3.p1.JavaDefined[String] = null;
        |}
      """.stripMargin)
    build
  }

  private lazy val b4: Build = {
    val build = new Build(projectsBase, "b4")
    val b3P1 = b3.project("p1")
    val p2 = build.project("p2")
    p2.classpath += b3P1.out
    p2.withSource("b4/p2/JavaClient.java")(
      """
        |package b4.p2;
        |public class JavaClient {
        |  b3.p1.JavaDefined<String> test() { return null; }
        |}
      """.stripMargin)
    p2.withSource("b4/p2/ScalaClient.scala")(
      """
        |package b4.p2
        |class ScalaClient {
        |  def test(): b3.p1.JavaDefined[String] = null;
        |}
      """.stripMargin)
    build
  }

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
      val classpath = mutable.ListBuffer[Path]()
      val sources = mutable.ListBuffer[Path]()
      def withSource(relativePath: String)(code: String): this.type = {
        val srcFile = src.resolve(relativePath)
        Files.createDirectories(srcFile.getParent)
        Files.write(srcFile, code.getBytes(Charset.defaultCharset()))
        sources += srcFile
        this
      }
      def argsFile(extraOpts: List[String]): Path = {
        val cp = if (classpath.isEmpty) Nil else List("-cp", classpath.mkString(File.pathSeparator))
        val printArgs = if (debug) List("-Xprint-args", "-") else Nil
        val entries = List(
          Build.this.scalacOptions.toList,
          scalacOptions.toList,
          extraOpts,
          printArgs,
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

  private def clean(): Unit = {
    class CleanVisitor() extends SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (dir.getFileName.toString == "target") {
          deleteRecursive(dir)
          Files.createDirectories(dir)
          FileVisitResult.SKIP_SUBTREE
        } else super.preVisitDirectory(dir, attrs)
      }
    }
    Files.walkFileTree(projectsBase, new CleanVisitor())
  }
  private def snapshotClasses(strategy: BuildStrategy): Path = {
    val src = projectsBase
    val dest = createDir(base, strategy.toString + "/classes")
    class CopyVisitor(src: Path, dest: Path) extends SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.createDirectories(dest.resolve(src.relativize(dir)))
        super.preVisitDirectory(dir, attrs)
      }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        val destDir = dest.resolve(src.relativize(dir))
        val listing = Files.list(destDir)
        try {
          if (!listing.iterator().hasNext)
            Files.delete(destDir)
        } finally {
          listing.close()
        }
        super.postVisitDirectory(dir, exc)
      }
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.copy(file, dest.resolve(src.relativize(file)))
        super.visitFile(file, attrs)
      }
    }
    Files.walkFileTree(src, new CopyVisitor(src, dest))
    dest
  }

  private def createDir(dir: Path, s: String): Path = {
    val subDir = dir.resolve(s)
    Files.createDirectories(subDir)
  }
}
