import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |class X {
    |   def methodIntIntInt(x: Int, y: Int) = x+y
    |}
    |
    |import scala.reflect.runtime.universe._
    |import scala.reflect.runtime.{ currentMirror => cm }
    |def im: InstanceMirror = cm.reflect(new X)
    |val cs: ClassSymbol = im.symbol
    |val ts: Type = cs.info
    |val ms: MethodSymbol = ts.decl(TermName("methodIntIntInt")).asMethod
    |val MethodType( _, t1 ) = ms.info
    |val t2 = typeOf[scala.Int]
    |t1 == t2
    |t1 =:= t2
    |t1 <:< t2
    |t2 <:< t1
    |""".stripMargin
}
