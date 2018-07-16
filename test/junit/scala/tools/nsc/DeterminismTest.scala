package scala.tools.nsc

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import org.junit.Test

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.language.implicitConversions
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.reporters.StoreReporter

class DeterminismTest {
  @Test def testLambdaLift(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |package demo
          |
          |class a {
          |  def x = {
          |    def local = "x"
          |  }
          |  def y = {
          |    def local = "y"
          |  }
          |}
          |
      """.stripMargin),
      source("b.scala",
      """
        |package demo
        |
        |class b {
        |  def test: Unit = {
        |    new a().y
        |  }
        |}
      """.stripMargin)

    )
    test(List(code))
  }
  @Test def testTyperFreshName(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |package demo
          |
          |class a {
          |  def x = {
          |    { case x if "".isEmpty => "" }: PartialFunction[Any, Any]
          |  }
          |  def y = {
          |    { case x if "".isEmpty => "" }: PartialFunction[Any, Any]
          |  }
          |}
          |
      """.stripMargin),
      source("b.scala",
      """
        |package demo
        |
        |class b {
        |  def test: Unit = {
        |    new a().y
        |  }
        |}
      """.stripMargin)

    )
    test(List(code))
  }

  @Test def testReify(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |package demo
          |
          |import language.experimental.macros
          |import scala.reflect.macros.blackbox.Context
          |
          |class a {
          |  def x(c: Context) = {
          |    import c.universe._
          |    reify { type T = Option[_]; () }.tree
          |  }
          |  def y(c: Context) = {
          |    import c.universe._
          |    reify { type T = Option[_]; () }.tree
          |  }
          |}
          |
      """.stripMargin),
      source("b.scala",
      """
        |package demo
        |
        |class b {
        |  def test: Unit = {
        |    new a().y(null)
        |  }
        |}
      """.stripMargin)

    )
    test(List(code))
  }

  @Test def testMacroFreshName(): Unit = {
    val macroCode = source("macro.scala",
        """
          |package demo
          |
          |import language.experimental.macros
          |import scala.reflect.macros.blackbox.Context
          |
          |object Macro {
          |  def impl(c: Context): c.Tree = {
          |    import c.universe._
          |    val name = c.freshName("foo")
          |    Block(ValDef(NoMods, TermName(name), tq"_root_.scala.Int", Literal(Constant(0))) :: Nil, Ident(name))
          |  }
          |  def m: Unit = macro impl
          |}
          |
      """.stripMargin)
    def code = List(
      source("a.scala",
      """
        |package demo
        |
        |class a {
        |  def test: Unit = {
        |    Macro.m
        |  }
        |}
      """.stripMargin),
        source("b.scala",
      """
        |package demo
        |
        |class b {
        |  def test: Unit = {
        |    Macro.m
        |  }
        |}
      """.stripMargin)

    )
    test(List(List(macroCode), code))
  }


  @Test def testRefinementTypeOverride(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |class Global
          |trait Analyzer extends StdAttachments {
          |  val global: Global
          |}
          |trait Context {
          |  val universe: Global
          |}
          |
          |trait StdAttachments {
          |  self: Analyzer =>
          |
          |  type UnaffiliatedMacroContext = Context
          |  type MacroContext = UnaffiliatedMacroContext { val universe: self.global.type }
          |}
          |
      """.stripMargin),
      source("b.scala",
        """
          |class Macros {
          |    self: Analyzer =>
          |  def foo = List.apply[MacroContext]()
          |}
          |
        """.stripMargin)
    )
    test(List(code))
  }

  def source(name: String, code: String): SourceFile = new BatchSourceFile(name, code)
  private def test(groups: List[List[SourceFile]]): Unit = {
    val referenceOutput = Files.createTempDirectory("reference")

    def compile(output: Path, files: List[SourceFile]): Unit = {
      val g = new Global(new Settings)
      g.settings.usejavacp.value = true
      g.settings.classpath.value = output.toAbsolutePath.toString
      g.settings.outputDirs.setSingleOutput(output.toString)
      val storeReporter = new StoreReporter
      g.reporter = storeReporter
      import g._
      val r = new Run
      // println("scalac " + files.mkString(" "))
      r.compileSources(files)
      assert(!storeReporter.hasErrors, storeReporter.infos.mkString("\n"))
    }

    for (group <- groups.init) {
      compile(referenceOutput, group)
    }
    compile(referenceOutput, groups.last)

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
    for (permutation <- permutationsWithSubsets(groups.last)) {
      val recompileOutput = Files.createTempDirectory("recompileOutput")
      copyRecursive(referenceOutput, recompileOutput)
      compile(recompileOutput, permutation)
      assert(diff(referenceOutput, recompileOutput), s"Difference detected between recompiling $permutation Run:\njardiff -r $referenceOutput $recompileOutput\n")
      deleteRecursive(recompileOutput)
    }
    deleteRecursive(referenceOutput)

  }
  def permutationsWithSubsets[A](as: List[A]): List[List[A]] =
    as.permutations.toList.flatMap(_.inits.filter(_.nonEmpty)).distinct

  private def diff(dir1: Path, dir2: Path): Boolean = {
    def allFiles(dir: Path) = Files.walk(dir).iterator().asScala.map(x => (dir.relativize(x), x)).toList.filter(_._2.getFileName.toString.endsWith(".class")).sortBy(_._1.toString)

    val dir1Files = allFiles(dir1)
    val dir2Files = allFiles(dir2)
    val identical = dir1Files.corresponds(dir2Files) {
      case ((rel1, file1), (rel2, file2)) =>
        rel1 == rel2 && java.util.Arrays.equals(Files.readAllBytes(file1), Files.readAllBytes(file2))
    }
    identical
  }
  private def deleteRecursive(f: Path) = new PlainNioFile(f).delete()
  private def copyRecursive(src: Path, dest: Path): Unit = {
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
