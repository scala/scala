import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import scala.reflect.runtime.universe._
    |class A { def foo(x: Int*) = 1 }
    |val sig = typeOf[A] member newTermName("foo") typeSignature
    |val x = sig.asInstanceOf[MethodType].params.head
    |println(x.typeSignature)
    |""".stripMargin
}
