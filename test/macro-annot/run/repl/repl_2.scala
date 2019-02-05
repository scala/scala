import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.shell._

object Test extends App {
  private def repl(code: String): String = {
    val s = new Settings
    s.Xnojline.value = true
    s.usejavacp.value = false
    s.classpath.value = sys.props("sbt.paths.tests.classpath")
    s.plugin.value = List(sys.props("sbt.paths.plugin.jar"))
    val lines = ILoop.runForTranscript(code, s).linesIterator.toList
    lines.drop(3).dropRight(2).mkString("\n").trim.stripSuffix("scala>").trim
  }

  // @Ignore
  // @Test
  // def precompiledMacrosExpand: Unit = {
    // TODO: The workaround employed in https://github.com/scalamacros/paradise/issues/19
    // no longer works because of the REPL refactoring in 2.13.0-M2.
    // See https://github.com/scalamacros/paradise/issues/102 for discussion.
    // assertEquals(repl("""
    //   |@thingy class Thingy
    //   |@thingy class NonThingy
    // """.stripMargin.trim), """
    //   |scala> @thingy class Thingy
    //   |defined class Thingy
    //   |defined object Thingy
    //   |
    //   |scala> @thingy class NonThingy
    //   |defined class Thingy
    //   |defined object Thingy
    // """.stripMargin.trim)
  // }
  //
  // @Ignore
  // @Test
  // def adhocMacrosExpand = {
    // TODO: The workaround employed in https://github.com/scalamacros/paradise/issues/19
    // no longer works because of the REPL refactoring in 2.13.0-M2.
    // See https://github.com/scalamacros/paradise/issues/102 for discussion.
    // val printout = repl("""
    //   |import scala.language.experimental.macros
    //   |import scala.reflect.macros.whitebox.Context
    //   |import scala.annotation.StaticAnnotation
    //   |
    //   |object thingyAdhocMacro {
    //   |  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    //   |    import c.universe._
    //   |    val toEmit = c.Expr(q"class Thingy(i: Int) { def stuff = println(i) }; object Thingy { def apply(x: Int) = new Thingy(x) }")
    //   |    annottees.map(_.tree) match {
    //   |      case Nil => {
    //   |        c.abort(c.enclosingPosition, "No test target")
    //   |      }
    //   |      case (classDeclaration: ClassDef) :: Nil => {
    //   |        // println("No companion provided")
    //   |        toEmit
    //   |      }
    //   |      case (classDeclaration: ClassDef) :: (companionDeclaration: ModuleDef) :: Nil => {
    //   |        // println("Companion provided")
    //   |        toEmit
    //   |      }
    //   |      case _ => c.abort(c.enclosingPosition, "Invalid test target")
    //   |    }
    //   |  }
    //   |}
    //   |
    //   |class thingyAdhoc extends StaticAnnotation {
    //   |  def macroTransform(annottees: Any*): Any = macro thingyAdhocMacro.impl
    //   |}
    //   |
    //   |@thingyAdhoc class Thingy
    //   |@thingyAdhoc class NonThingy
    // """.stripMargin.trim)
    // assert(printout.contains("defined class Thingy"))
    // assert(printout.contains("defined object Thingy"))
    // assert(!printout.contains("defined class NonThingy"))
    // assert(!printout.contains("defined object NonThingy"))
  // }
}
