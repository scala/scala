package scala.issues

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{AfterClass, BeforeClass, Test}
import org.junit.Assert._

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox
import scala.tools.testing.ClearAfterClass

object RunTest {
  class VC(val x: Any) extends AnyVal
  class VCI(val x: Int) extends AnyVal { override def toString = "" + x }
}

@RunWith(classOf[JUnit4])
class RunTest extends ClearAfterClass {
  val toolBox = cached("toolbox", () => universe.runtimeMirror(getClass.getClassLoader).mkToolBox())

  def run[T](code: String): T = {
    toolBox.eval(toolBox.parse(code)).asInstanceOf[T]
  }

  @Test
  def classOfValueClassAlias(): Unit = {
    val code =
      """import scala.issues.RunTest.VC
        |type aVC = VC
        |type aInt = Int
        |type aInteger = Integer
        |classOf[VC] == classOf[aVC] &&
        |  classOf[aInt] == classOf[Int] &&
        |  classOf[aInteger] == classOf[Integer] &&
        |  classOf[aInt] != classOf[aInteger]
      """.stripMargin
    assertTrue(run[Boolean](code))
  }

  @Test
  def classOfFinalVal(): Unit = {
    val code =
      """class C {
        |  final val a1 = classOf[Int]
        |  final val b1 = classOf[List[_]]
        |  final val c1 = classOf[List[String]]
        |  final val d1 = classOf[Array[Int]]
        |  final val e1 = classOf[Array[List[_]]]
        |  final val f1 = classOf[Array[_]]
        |
        |  val a2 = classOf[Int]
        |  val b2 = classOf[List[_]]
        |  val c2 = classOf[List[String]]
        |  val d2 = classOf[Array[Int]]
        |  val e2 = classOf[Array[List[_]]]
        |  val f2 = classOf[Array[_]]
        |
        |  val listC = Class.forName("scala.collection.immutable.List")
        |
        |  val compare = List(
        |    (a1, a2, Integer.TYPE),
        |    (b1, b2, listC),
        |    (c1, c2, listC),
        |    (d1, d2, Array(1).getClass),
        |    (e1, e2, Array(List()).getClass),
        |    (f1, f2, new Object().getClass))
        |}
        |(new C).compare
      """.stripMargin
    type K = Class[_]
    val cs = run[List[(K, K, K)]](code)
    for ((x, y, z) <- cs) {
      assertEquals(x, y)
      assertEquals(x, z)
    }
  }

  @Test
  def t9702(): Unit = {
    val code =
      """import javax.annotation.Resource
        |import scala.issues.RunTest.VC
        |class C {
        |  type aList[K] = List[K]
        |  type aVC = VC
        |  type aInt = Int
        |  type aInteger = Integer
        |  @Resource(`type` = classOf[List[Int]])      def a = 0
        |  @Resource(`type` = classOf[List[_]])        def b = 0
        |  @Resource(`type` = classOf[aList[_]])       def c = 0
        |  @Resource(`type` = classOf[Int])            def d = 0
        |  @Resource(`type` = classOf[aInt])           def e = 0
        |  @Resource(`type` = classOf[Integer])        def f = 0
        |  @Resource(`type` = classOf[aInteger])       def g = 0
        |  @Resource(`type` = classOf[VC])             def h = 0
        |  @Resource(`type` = classOf[aVC])            def i = 0
        |  @Resource(`type` = classOf[Array[Int]])     def j = 0
        |  @Resource(`type` = classOf[Array[List[_]]]) def k = 0
        |}
        |val c = classOf[C]
        |def typeArg(meth: String) = c.getDeclaredMethod(meth).getDeclaredAnnotation(classOf[Resource]).`type`
        |('a' to 'k').toList.map(_.toString).map(typeArg)
      """.stripMargin

    val l = Class.forName("scala.collection.immutable.List")
    val i = Integer.TYPE
    val ig = new Integer(1).getClass
    val v = new RunTest.VC(1).getClass
    val ai = Array(1).getClass
    val al = Array(List()).getClass

    // sanity checks
    assertEquals(i, classOf[Int])
    assertNotEquals(i, ig)

    assertEquals(run[List[Class[_]]](code),
      List(l, l, l, i, i, ig, ig, v, v, ai, al))
  }

  @Test
  def annotationInfoNotErased(): Unit = {
    val code =
      """import javax.annotation.Resource
        |import scala.annotation.meta.getter
        |class C {
        |  type Rg = Resource @getter
        |  @(Resource @getter)(`type` = classOf[Int]) def a = 0
        |  @Rg(`type` = classOf[Int])                 def b = 0
        |}
        |val c = classOf[C]
        |def typeArg(meth: String) = c.getDeclaredMethod(meth).getDeclaredAnnotation(classOf[Resource]).`type`
        |List("a", "b") map typeArg
        |""".stripMargin

    val i = Integer.TYPE
    assertEquals(run[List[Class[_]]](code), List(i, i))
  }

  @Test
  def invocationReceivers(): Unit = {
    import invocationReceiversTestCode._
    assertEquals(run[String](definitions("Object") + runCode), "hi" * 9)
    assertEquals(run[String](definitions("String") + runCode), "hi" * 9) // bridge method for clone generated
  }

  @Test
  def classOfUnitConstant(): Unit = {
    val code =
      """abstract class A { def f: Class[_] }
        |class C extends A { final val f = classOf[Unit] }
        |val c = new C
        |(c.f, (c: A).f)
      """.stripMargin
    val u = Void.TYPE
    assertEquals(run[(Class[_], Class[_])](code), (u, u))
  }

  @Test
  def t9671(): Unit = {
    val code =
      """import scala.issues.RunTest.VCI
        |
        |def f1(a: Any) = "" + a
        |def f2(a: AnyVal) = "" + a
        |def f3[T](a: T) = "" + a
        |def f4(a: Int) = "" + a
        |def f5(a: VCI) = "" + a
        |def f6(u: Unit) = "" + u
        |
        |def n1: AnyRef = null
        |def n2: Null = null
        |def n3: Any = null
        |def n4[T]: T = null.asInstanceOf[T]
        |
        |def npe(s: => String) = try { s; throw new Error() } catch { case _: NullPointerException => "npe" }
        |
        |    f1(null.asInstanceOf[Int])  +
        |    f1(  n1.asInstanceOf[Int])  +
        |    f1(  n2.asInstanceOf[Int])  +
        |    f1(  n3.asInstanceOf[Int])  +
        |    f1(               n4[Int])  + // "null"
        |"-"                             +
        |    f1(null.asInstanceOf[VCI])  +
        |npe(f1(  n1.asInstanceOf[VCI])) + // SI-8097
        |    f1(  n2.asInstanceOf[VCI])  +
        |npe(f1(  n3.asInstanceOf[VCI])) + // SI-8097
        |    f1(               n4[VCI])  + // "null"
        |"-"                             +
        |    f1(null.asInstanceOf[Unit]) +
        |    f1(  n1.asInstanceOf[Unit]) +
        |    f1(  n2.asInstanceOf[Unit]) +
        |    f1(  n3.asInstanceOf[Unit]) +
        |    f1(               n4[Unit]) + // "null"
        |"-"                             +
        |    f2(null.asInstanceOf[Int])  +
        |    f2(  n1.asInstanceOf[Int])  +
        |    f2(  n2.asInstanceOf[Int])  +
        |    f2(  n3.asInstanceOf[Int])  +
        |    f2(               n4[Int])  + // "null"
        |"-"                             +
        |    f2(null.asInstanceOf[VCI])  +
        |npe(f2(  n1.asInstanceOf[VCI])) + // SI-8097
        |    f2(  n2.asInstanceOf[VCI])  +
        |npe(f2(  n3.asInstanceOf[VCI])) + // SI-8097
        |    f2(               n4[VCI])  + // "null"
        |"-"                             +
        |    f2(null.asInstanceOf[Unit]) +
        |    f2(  n1.asInstanceOf[Unit]) +
        |    f2(  n2.asInstanceOf[Unit]) +
        |    f2(  n3.asInstanceOf[Unit]) +
        |    f2(               n4[Unit]) + // "null"
        |"-"                             +
        |    f3(null.asInstanceOf[Int])  +
        |    f3(  n1.asInstanceOf[Int])  +
        |    f3(  n2.asInstanceOf[Int])  +
        |    f3(  n3.asInstanceOf[Int])  +
        |    f3(               n4[Int])  + // "null"
        |"-"                             +
        |    f3(null.asInstanceOf[VCI])  +
        |npe(f3(  n1.asInstanceOf[VCI])) + // SI-8097
        |    f3(  n2.asInstanceOf[VCI])  +
        |npe(f3(  n3.asInstanceOf[VCI])) + // SI-8097
        |    f3(               n4[VCI])  + // "null"
        |"-"                             +
        |    f3(null.asInstanceOf[Unit]) +
        |    f3(  n1.asInstanceOf[Unit]) +
        |    f3(  n2.asInstanceOf[Unit]) +
        |    f3(  n3.asInstanceOf[Unit]) +
        |    f3(               n4[Unit]) + // "null"
        |"-"                             +
        |    f4(null.asInstanceOf[Int])  +
        |    f4(  n1.asInstanceOf[Int])  +
        |    f4(  n2.asInstanceOf[Int])  +
        |    f4(  n3.asInstanceOf[Int])  +
        |    f4(               n4[Int])  +
        |"-"                             +
        |    f5(null.asInstanceOf[VCI])  +
        |npe(f5(  n1.asInstanceOf[VCI])) + // SI-8097
        |    f5(  n2.asInstanceOf[VCI])  +
        |npe(f5(  n3.asInstanceOf[VCI])) + // SI-8097
        |npe(f5(               n4[VCI])) + // SI-8097
        |"-"                             +
        |    f6(null.asInstanceOf[Unit]) +
        |    f6(  n1.asInstanceOf[Unit]) +
        |    f6(  n2.asInstanceOf[Unit]) +
        |    f6(  n3.asInstanceOf[Unit]) +
        |    f6(               n4[Unit])   // "null"
      """.stripMargin

    assertEquals(run[String](code),
      "0000null-0npe0npenull-()()()()null-0000null-0npe0npenull-()()()()null-0000null-0npe0npenull-()()()()null-00000-0npe0npenpe-()()()()null")
  }
}
