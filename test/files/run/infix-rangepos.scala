import scala.tools.partest._

object Test extends CompilerTest {
  import global._
  override def extraSettings = super.extraSettings + " -Yrangepos"
  override def sources = List(
    "class C1 { def t = List(1).map ( x => x ) }",
    "class C2 { def t = List(1).map { x => x } }",
    "class C3 { def t = List(1).map ({x => x}) }",
    "class C4 { def t = List(1) map ( x => x ) }",
    "class C5 { def t = List(1) map { x => x } }",
    "class C6 { def t = List(1) map ({x => x}) }")

  def check(source: String, unit: CompilationUnit): Unit = unit.body foreach {
    case dd: DefDef if dd.name.startsWith("t") =>
      val pos = dd.rhs.pos
      assert(pos.start == 19, pos.start)
      assert(pos.end == 41, pos.end)
    case _ =>
  }
}
