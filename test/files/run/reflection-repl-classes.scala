import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |class A
    |
    |class B {
    |  def foo(x: A) = 1
    |}
    |
    |object defs {
    |  val cm = reflect.runtime.currentMirror
    |  val u = cm.universe
    |  val im = cm.reflect(new B)
    |  val method = im.symbol.info.member(u.TermName("foo")).asMethod
    |  val mm = im.reflectMethod(method)
    |}
    |import defs._
    |
    |mm(new A)
    |""".stripMargin
}
