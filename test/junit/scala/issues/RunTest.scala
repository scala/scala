package scala.issues

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{AfterClass, BeforeClass, Test}
import org.junit.Assert._

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox
import scala.tools.testing.ClearAfterClass

object RunTest extends ClearAfterClass.Clearable {
  var toolBox = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
  override def clear(): Unit = { toolBox = null }

  // definitions for individual tests

  class VC(val x: Any) extends AnyVal
}

@RunWith(classOf[JUnit4])
class RunTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = RunTest

  def run[T](code: String): T = {
    val tb = RunTest.toolBox
    tb.eval(tb.parse(code)).asInstanceOf[T]
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
}
