package scala.tools.nsc.interpreter

import java.io.{StringWriter, PrintWriter}

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.nsc.Settings

class CompletionTest {
  val EmptyString = "" // def string results include the empty string so that JLine won't insert "def ..." at the cursor

  def newIMain(): IMain = {
    val settings = new Settings()
    settings.Xnojline.value = true
    settings.usejavacp.value = true

    val writer = new StringWriter
    val out = new PrintWriter(writer)
    new IMain(settings, out)
  }
  @Test
  def t4438_arrayCompletion(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    assert(completer.complete("Array(1, 2, 3) rev").candidates.contains("reverseMap"))
  }

  @Test
  def completions(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    checkExact(completer, "object O { def x_y_z = 1 }; import O._; x_y")("x_y_z")
    checkExact(completer, "object O { private def x_y_z = 1 }; import O._; x_y")()
    checkExact(completer, "object O { private def x_y_z = 1; x_y", "}")("x_y_z")
    checkExact(completer, "object x_y_z; import x_y")("x_y_z")

    checkExact(completer, "object x_y_z { def a_b_c }; import x_y_z.a_b")("a_b_c")

    checkExact(completer, "object X { private[this] def definition = 0; def")("definition")

    // stable terms are offered in type completion as they might be used as a prefix
    checkExact(completer, """object O { def x_y_z = 0; val x_z_y = ""; type T = x_""")("x_z_y")
    checkExact(completer, """def method { def x_y_z = 0; val x_z_y = ""; type T = x_""")("x_z_y")

    // We exclude inherited members of the synthetic interpreter wrapper classes
    checkExact(completer, """asInstanceO""")()
    checkExact(completer, """class C { asInstanceO""")("asInstanceOf")

    // Output is sorted
    assertEquals(List("prefix_aaa", "prefix_nnn", "prefix_zzz"), completer.complete( """class C { def prefix_nnn = 0; def prefix_zzz = 0; def prefix_aaa = 0; prefix_""").candidates)
  }

  @Test
  def annotations(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    checkExact(completer, "def foo[@specialize", " A]")("specialized")
    checkExact(completer, "def foo[@specialize")("specialized")
    checkExact(completer, """@deprecatedN""", """ class Foo""")("deprecatedName")
    checkExact(completer, """@deprecateN""")("deprecatedName")
    checkExact(completer, """{@deprecateN""")("deprecatedName")
  }

  @Test
  def incompleteStringInterpolation(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    checkExact(completer, """val x_y_z = 1; s"${x_""", "}\"")("x_y_z")
    checkExact(completer, """val x_y_z = 1; s"${x_""", "\"")("x_y_z")
  }

  @Test
  def symbolically(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    checkExact(completer, """class C { def +++(a: Any) = 0; def ---(a: Any) = 0; this.++""")("+++")
  }

  @Test
  def camelCompletions(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    checkExact(completer, "object O { def theCatSatOnTheMat = 1 }; import O._; tCSO")("theCatSatOnTheMat")
    checkExact(completer, "object O { def getBlerganator = 1 }; import O._; blerga")("getBlerganator")
    checkExact(completer, "object O { def xxxxYyyyyZzzz = 1; def xxxxYyZeee = 1 }; import O._; xYZ")("", "xxxxYyyyyZzzz", "xxxxYyZeee")
    checkExact(completer, "object O { def xxxxYyyyyZzzz = 1; def xxxxYyyyyZeee = 1 }; import O._; xYZ")("xxxxYyyyyZzzz", "xxxxYyyyyZeee")
    checkExact(completer, "object O { class AbstractMetaFactoryFactory }; new O.AMFF")("AbstractMetaFactoryFactory")
  }

  @Test
  def lenientCamelCompletions(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    checkExact(completer, "object O { def theCatSatOnTheMat = 1 }; import O._; tcso")("theCatSatOnTheMat")
    checkExact(completer, "object O { def theCatSatOnTheMat = 1 }; import O._; sotm")("theCatSatOnTheMat")
    checkExact(completer, "object O { def theCatSatOnTheMat = 1 }; import O._; TCSOTM")()
  }

  @Test
  def previousLineCompletions(): Unit = {
    val intp = newIMain()
    intp.interpret("class C { val x_y_z = 42 }")
    intp.interpret("object O { type T = Int }")

    val completer = new PresentationCompilerCompleter(intp)

    checkExact(completer, "new C().x_y")("x_y_z")
    checkExact(completer, "(1 : O.T).toCha")("toChar")

    intp.interpret("case class X_y_z()")
    val completer1 = new PresentationCompilerCompleter(intp)
    checkExact(completer1, "new X_y_")("X_y_z")
    checkExact(completer1, "X_y_")("X_y_z")
    checkExact(completer1, "X_y_z.app")("apply")
  }

  @Test
  def previousResultInvocation(): Unit = {
    val intp = newIMain()
    intp.interpret("1 + 1")

    val completer = new PresentationCompilerCompleter(intp)

    checkExact(completer, ".toCha")("toChar")
  }

  @Test
  def defString(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)

    // Double Tab on a fully typed selection shows the def string
    checkExact(completer, "(p: {def a_b_c: Int}) => p.a_b_c")()
    checkExact(completer, "(p: {def a_b_c: Int}) => p.a_b_c")(EmptyString, "def a_b_c: Int")

    // likewise for an ident
    checkExact(completer, "(p: {def x_y_z: Int}) => {import p._; x_y_z")()
    checkExact(completer, "(p: {def x_y_z: Int}) => {import p._; x_y_z")(EmptyString, "def x_y_z: Int")

    // If the first completion only gives one alternative
    checkExact(completer, "(p: {def x_y_z: Int; def x_y_z(a: String): Int }) => p.x_y")("x_y_z")
    // ... it is automatically inserted into the buffer. Hitting <TAB> again is triggers the help
    checkExact(completer, "(p: {def x_y_z: Int; def x_y_z(a: String): Int }) => p.x_y_z")(EmptyString, "def x_y_z(a: String): Int", "def x_y_z: Int")

    checkExact(completer, "(p: {def x_y_z: Int; def x_z_y(a: String): Int }) => p.x_")("x_y_z", "x_z_y")
    // By contrast, in this case the user had to type "y_z" manually, so no def string printing just yet
    checkExact(completer, "(p: {def x_y_z: Int; def x_z_y(a: String): Int }) => p.x_y_z")()
    // Another <TAB>, Okay, time to print.
    checkExact(completer, "(p: {def x_y_z: Int; def x_z_y(a: String): Int }) => p.x_y_z")(EmptyString, "def x_y_z: Int")

    // The def string reconstructs the source-level modifiers (rather than showing the desugarings of vals),
    // and performs as-seen-from with respect to the prefix
    checkExact(completer, "trait T[A]{ lazy val x_y_z: A }; class C extends T[Int] { x_y_z")()
    checkExact(completer, "trait T[A]{ lazy val x_y_z: A }; class C extends T[Int] { x_y_z")(EmptyString, "lazy val x_y_z: Int")

    checkExact(completer, "trait T[A] { def foo: A }; (t: T[Int]) => t.foo")()
    checkExact(completer, "trait T[A] { def foo: A }; (t: T[Int]) => t.foo")(EmptyString, "def foo: Int")
  }

  @Test
  def treePrint(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    checkExact(completer, " 1.toHexString //print")(EmptyString, "scala.Predef.intWrapper(1).toHexString // : String")
  }

  @Test
  def firstCompletionWithNoPrefixHidesUniversalMethodsAndExtensionMethods(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    checkExact(completer, "class C(val a: Int, val b: Int) { this.")("a", "b")
    assert(Set("asInstanceOf", "==").diff(completer.complete("class C(val a: Int, val b: Int) { this.").candidates.toSet).isEmpty)
    checkExact(completer, "case class D(a: Int, b: Int) { this.a")("a", "asInstanceOf")
  }

  @Test
  def performanceOfLenientMatch(): Unit = {
    val intp = newIMain()
    val completer = new PresentationCompilerCompleter(intp)
    val ident: String = "thisIsAReallyLongMethodNameWithManyManyManyManyChunks"
    checkExact(completer, s"($ident: Int) => tia")(ident)
  }

  def checkExact(completer: PresentationCompilerCompleter, before: String, after: String = "")(expected: String*): Unit = {
    assertEquals(expected.toSet, completer.complete(before, after).candidates.toSet)
  }
}
