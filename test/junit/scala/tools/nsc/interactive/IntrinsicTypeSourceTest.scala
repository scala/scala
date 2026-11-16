package scala.tools.nsc.interactive

import org.junit.Assert._
import org.junit.Test

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.tests.InteractiveTest
import scala.tools.nsc.io.File

class IntrinsicTypeSourceTest {
  private val intrinsicNames = List("Any", "AnyRef", "Nothing", "Null", "Singleton")
  private val valid = """object Healthy {
    |  def cast(value: _root_.scala.AnyRef): String = value.asInstanceOf[String]
    |  def pick(value: String): String = value
    |  def pick(value: _root_.scala.Any): _root_.scala.Any = value
    |  val result: String = pick("x")
    |  val ref: _root_.scala.AnyRef = "ok"
    |  val obj: java.lang.Object = ref
    |  def bottom(value: _root_.scala.Nothing): String = value
    |  def throwing: _root_.scala.Nothing = throw new Exception
    |  def nullable(value: _root_.scala.Null): String = value
    |  val nul: String = null
    |  def literal[T <: _root_.scala.Singleton](value: T): T = value
    |  val exact: 42 = literal(42)
    |  val number: _root_.scala.Int = 1 + 2
    |  val bool: _root_.scala.Boolean = number == 3
    |  class Value(val number: Int) extends AnyVal
    |}""".stripMargin
  private val invalid = """object Invalid {
    |  val badNull: _root_.scala.Int = null
    |  val badSingleton: _root_.scala.Singleton = new Object
    |  val badBottom: _root_.scala.Nothing = 1
    |}""".stripMargin

  private def exercise(code: String, macros: Boolean, expectedError: String, deleteInput: Boolean): Unit = {
    new InteractiveTest {
      override protected def prepareSettings(settings: Settings): Unit = {
        super.prepareSettings(settings)
        settings.YmacroAnnotations.value = macros
      }

      private def completed[T](response: Response[T]): T = response.get(TIMEOUT) match {
        case Some(Left(result)) => result
        case Some(Right(error)) => throw error
        case None => throw new AssertionError("presentation compiler request timed out")
      }

      private def load(name: String, text: String): List[(Int, String)] = {
        val source = new BatchSourceFile(name, text)
        completed(askReload(List(source)))
        completed(askLoadedTyped(source, keepLoaded = true))
        compiler.ask(() => compiler.getUnit(source).get.problems.toList
          .filter(_.severityLevel == 2).map(p => (p.pos.line, p.msg)))
      }

      private def checkTyping(): Unit = {
        assertEquals("subsequent valid request", Nil, load("Healthy.scala", valid))
        assertEquals("invalid assignments must still be rejected", Set(2, 3, 4),
          load("Invalid.scala", invalid).map(_._1).toSet)
      }

      override def execute(): Unit = {
        checkTyping()
        val symbols = compiler.ask(() => intrinsicNames.map { name =>
          val symbol = compiler.definitions.ScalaPackageClass.info.decls.lookup(compiler.TypeName(name))
          assertTrue(name, symbol != compiler.NoSymbol)
          (name, symbol)
        })
        def checkOwnership(): Unit = compiler.ask(() => symbols.foreach { case (name, symbol) =>
          assertSame(name, symbol, compiler.definitions.ScalaPackageClass.info.decls.lookup(compiler.TypeName(name)))
          assertNull(name, symbol.sourceFile)
          assertFalse(name, compiler.currentRun.symSource.contains(symbol))
        })
        checkOwnership()
        val inputErrors = load("Input.scala", code)
        checkTyping()
        checkOwnership()
        if (expectedError.isEmpty) assertEquals(Nil, inputErrors)
        else assertTrue(inputErrors.mkString("\n"), inputErrors.exists(_._2.contains(expectedError)))

        load("Input.scala", code)
        checkTyping()
        checkOwnership()
        if (deleteInput) {
          // Rejected auxiliary source must never acquire ownership of a permanent symbol.
          val deleted = new Response[Unit]
          compiler.askFilesDeleted(List(new BatchSourceFile("Input.scala", code)), deleted)
          completed(deleted)
          checkTyping()
          checkOwnership()
        }

        assertEquals(Nil, load("Editable.scala", "class Editable { def value: Int = 1 }"))
        assertEquals(Nil, load("Editable.scala", "class Editable { def value: String = \"x\" }"))
        assertEquals(Nil, load("Use.scala", "object Use { val value: String = new Editable().value }"))
      }
    }.main(null)
  }

  private def auxiliary(name: String, macros: Boolean): Unit =
    exercise(File(s"src/library-aux/scala/$name.scala").slurp(), macros, "cannot be redefined", deleteInput = true)

  @Test def any(): Unit = auxiliary("Any", macros = false)
  @Test def anyWithMacroAnnotations(): Unit = auxiliary("Any", macros = true)
  @Test def anyRef(): Unit = auxiliary("AnyRef", macros = false)
  @Test def anyRefWithMacroAnnotations(): Unit = auxiliary("AnyRef", macros = true)
  @Test def nothing(): Unit = auxiliary("Nothing", macros = false)
  @Test def nothingWithMacroAnnotations(): Unit = auxiliary("Nothing", macros = true)
  @Test def nullType(): Unit = auxiliary("Null", macros = false)
  @Test def nullTypeWithMacroAnnotations(): Unit = auxiliary("Null", macros = true)
  @Test def singleton(): Unit = auxiliary("Singleton", macros = false)
  @Test def singletonWithMacroAnnotations(): Unit = auxiliary("Singleton", macros = true)

  @Test def caseClassWithDefaults(): Unit =
    exercise("package scala; case class Any(x: Int = 1)", macros = true, expectedError = "cannot be redefined", deleteInput = true)
  @Test def ordinaryUserNames(): Unit =
    exercise("package example; " + intrinsicNames.map("class " + _).mkString("; "), macros = true, expectedError = "", deleteInput = true)
  @Test def companionObjects(): Unit =
    exercise("package scala; " + intrinsicNames.map("object " + _).mkString("; "), macros = true, expectedError = "cannot have a companion object", deleteInput = true)
  @Test def ordinaryUserObjects(): Unit =
    exercise("package example; " + intrinsicNames.map("object " + _).mkString("; "), macros = true, expectedError = "", deleteInput = true)
  @Test def ordinarySourceError(): Unit =
    exercise("class Broken extends Broken", macros = true, expectedError = "illegal cyclic reference", deleteInput = true)

  private def realSource(name: String): Unit =
    exercise(File(s"src/library/scala/$name.scala").slurp(), macros = true, expectedError = "", deleteInput = false)
  @Test def realAnyValSource(): Unit = realSource("AnyVal")
  @Test def realIntSource(): Unit = realSource("Int")
  @Test def realBooleanSource(): Unit = realSource("Boolean")
}
