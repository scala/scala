import scala.tools.partest.ReplTest

object Test extends ReplTest {
  // before java 21, nested classes have a `this$0` accessor
  override def eval(): Iterator[String] = super.eval().filterNot(_.contains("val this$0"))

  def code = """
    |:power
    |import rootMirror._
    |getClassIfDefined("p.C1$I").info
    |getClassIfDefined("p.C2$I").info
    |getClassIfDefined("p.C1$I") // symbol unlinked after reading C1
    |getClassIfDefined("p.C2$I") // symbol unlinked after reading C2
    |getClassIfDefined("p.C1").info.member(TypeName("I")).info
    |getClassIfDefined("p.C2").info.member(TypeName("I")).info
  """.stripMargin
}
