package scala.tools.nsc

import java.io.OutputStreamWriter
import java.nio.charset.Charset
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import javax.tools.ToolProvider
import org.junit.Test

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.tools.nsc.reporters.StoreReporter
import FileUtils._

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

  @Test def testAnnotations1(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |class Annot1(s: String) extends scala.annotation.StaticAnnotation
          |class Annot2(s: Class[_]) extends scala.annotation.StaticAnnotation
          |
      """.stripMargin),
      source("b.scala",
        """
          |@Annot1("foo")
          |@Annot2(classOf[AnyRef])
          |class Test
        """.stripMargin)
    )
    test(List(code))
  }

  @Test def testAnnotationsJava(): Unit = {
    def code = List[SourceFile](
      source("Annot1.java",
        """
          |import java.lang.annotation.*;
          |@Retention(RetentionPolicy.RUNTIME)
          |@Target(ElementType.TYPE)
          |@Inherited
          |@interface Annot1 { String value() default ""; }
          |
          |@Retention(RetentionPolicy.RUNTIME)
          |@Target(ElementType.TYPE)
          |@Inherited
          |@interface Annot2 { Class value(); }
          |
      """.stripMargin),
      source("b.scala",
        """
          |@Annot1("foo") @Annot2(classOf[AnyRef]) class Test
        """.stripMargin)
    )
    test(List(code))
  }

  @Test def testAnnotationsJavaRepeatable(): Unit = {
    val javaAnnots = source("Annot1.java",
      """
        |import java.lang.annotation.*;
        |@Repeatable(Annot1.Container.class)
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.TYPE)
        |@interface Annot1 { String value() default "";
        |
        |    @Retention(RetentionPolicy.RUNTIME)
        |    @Target(ElementType.TYPE)
        |    public static @interface Container {
        |        Annot1[] value();
        |    }
        |}
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.TYPE)
        |@Inherited
        |@interface Annot2 { Class value(); }
      """.stripMargin)
    def code =
      List(source("dummy.scala", ""), source("b.scala",
        """
          |@Annot1("foo") @Annot2(classOf[String]) @Annot1("bar") class Test
        """.stripMargin)
    )
    test(List(javaAnnots) :: code :: Nil)
  }

  @Test def testPackedType(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          | class C {
          |   def foo = { object A; object B; object C; object D; object E; object F; def foo[A](a: A) = (a, a); foo((A, B, C, D, E))}
          | }
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
      Predef.assert(!storeReporter.hasErrors, storeReporter.infos.mkString("\n"))
      files.filter(_.file.name.endsWith(".java")) match {
        case Nil =>
        case javaSources =>
          def tempFileFor(s: SourceFile): Path = {
            val f = output.resolve(s.file.name)
            Files.write(f, new String(s.content).getBytes(Charset.defaultCharset()))
          }
          val options = List("-d", output.toString)
          val javac = ToolProvider.getSystemJavaCompiler
          val fileMan = javac.getStandardFileManager(null, null, null)
          val javaFileObjects = fileMan.getJavaFileObjects(javaSources.map(s => tempFileFor(s).toAbsolutePath.toString): _*)
          val task = javac.getTask(new OutputStreamWriter(System.out), fileMan, null, options.asJava, Nil.asJava, javaFileObjects)
          val result = task.call()
          Predef.assert(result)
      }
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
      assertDirectorySame(referenceOutput, recompileOutput, permutation.toString)
      deleteRecursive(recompileOutput)
    }
    deleteRecursive(referenceOutput)

  }
  def permutationsWithSubsets[A](as: List[A]): List[List[A]] =
    as.permutations.toList.flatMap(_.inits.filter(_.nonEmpty)).distinct
}
