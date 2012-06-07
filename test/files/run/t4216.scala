import scala.tools.partest.ReplTest

// t4216
object Test extends ReplTest {
  def code =
    """
      |import scala.reflect.ClassTag
      |def f[A: ClassTag](a: A) = java.util.Arrays.asList(Array(a): _*)
      |f(".")
      |f(0)
      |def i(a: Int) = java.util.Arrays.asList(Array(a): _*)
      |i(0)
      |def o(a: Any) = java.util.Arrays.asList(Array(a): _*)
      |o(".")
      |class V(val a: Int) extends AnyVal
      |f(new V(0))
      |o(new V(0))
      |""".stripMargin.trim
}
