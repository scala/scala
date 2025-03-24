import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code =
    """:power
      |class C {
      |  def f = new {
      |    def g = new {
      |      def h = 1
      |    }
      |  }
      |}
      |val f = typeOf[C].decl(TermName("f"))
      |val g = f.tpe.resultType.decls.head
      |g.ownerChain.take(4)
      |g.tpe.resultType.typeSymbol
      |g.tpe.resultType.typeSymbol.ownerChain.take(4)
      |""".stripMargin
}
