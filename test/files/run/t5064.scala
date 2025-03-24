import scala.tools.partest._

object Test extends CompilerTest {
  import global._

  override def sources = List(
    """|class T5064 {
       |  List(1)
       |  Seq(1)
       |  List
       |  Seq
       |  Nil
       |}""".stripMargin
  )
  def check(source: String, unit: CompilationUnit): Unit = {
    for (ClassDef(_, _, _, Template(_, _, stats)) <- unit.body ; stat <- stats ; t <- stat) {
      t match {
        case _: Select | _: Apply | _: This => println(f"${t.pos.show}%-15s $t")
        case _                              =>
      }
    }
  }
}
