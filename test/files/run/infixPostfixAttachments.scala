import scala.tools.partest._

object Test extends CompilerTest {
  import global._
  override def extraSettings = super.extraSettings + " -Yrangepos -Ystop-after:typer -deprecation"

  override def code =
    """class C {
      |  import scala.language.postfixOps
      |
      |  def a(x: Any) = this
      |  def b(x: (Any, Any)) = this
      |  def c(x: Any, y: Any) = this
      |  def d() = this
      |  def e = this
      |
      |  def t1 = this a 0
      |  def t2 = this b (1, 2) b ((1, 2)) c (1, 2)
      |  def t3 = this.b(1, 2).b((1, 2)).c(1, 2)
      |  // def t4 = this d () // not allowed in 2.13
      |  def t5 = this.d()
      |  def t6 = this d
      |  def t7 = this.d
      |  def t8 = this e
      |  def t9 = this.e
      |}
      |""".stripMargin

  def check(source: String, unit: CompilationUnit): Unit = unit.body foreach {
    case dd: DefDef if dd.name.startsWith("t") =>
      dd.rhs.foreach(t => {
        if (!t.attachments.isEmpty)
          println(s"${dd.symbol.name} $t ${t.attachments.all.toList.sortBy(_.toString)}")
      })
    case _ =>
  }
}
