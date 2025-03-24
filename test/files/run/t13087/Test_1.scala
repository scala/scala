import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code =
    """:power
      |@ann class K
      |typeOf[K]
      |val arg = typeOf[K].typeSymbol.annotations.head.args.head
      |val plusSel = arg.asInstanceOf[Apply].fun
      |plusSel.tpe
      |plusSel.symbol.tpe
      |val fSel = plusSel.asInstanceOf[Select].qualifier
      |fSel.tpe
      |fSel.symbol.tpe
      |val aSel = arg.asInstanceOf[Apply].args.head.asInstanceOf[Apply].fun
      |aSel.tpe
      |aSel.symbol.tpe
      |""".stripMargin
}
