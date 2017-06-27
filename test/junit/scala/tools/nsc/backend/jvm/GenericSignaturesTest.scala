package scala.tools.nsc.backend.jvm

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class GenericSignaturesTest extends BytecodeTesting {
  import compiler._

  @Test
  def nestedModules(): Unit = {
    val code =
      """class C[T] {
        |  object O {
        |    object I
        |    class J[U]
        |    class K[V] extends J[V]
        |  }
        |}
      """.stripMargin
    val List(c, o, i, j, k) = compileClasses(code)
    assertEquals(o.name, "C$O$")
    assertEquals(o.methods.asScala.find(_.name == "I").get.signature, "()LC<TT;>.O$I$;")
    assertEquals(k.signature, "<V:Ljava/lang/Object;>LC<TT;>.O$J<TV;>;")
  }

  @Test
  def t10351(): Unit = {
    val code =
      """trait A[U] {
        |  type B <: U
        |}
        |class C {
        |  val a: A[Int] = ???
        |  val b: a.B = ???
        |}
      """.stripMargin

    val List(_, c) = compileClasses(code)
    assertEquals(
      List(("a", "LA<Ljava/lang/Object;>;"), ("b", null)),
      c.fields.asScala.toList.map(f => (f.name, f.signature)).sorted)
  }

  @Test
  def t9810(): Unit = {
    val code =
      """class A[+P] (final val id: Int) extends AnyVal
        |class C extends AnyRef
        |object C {
        |  final val key: A[C] = new A(1)
        |}
      """.stripMargin
    val List(a, aM, c, cM) = compileClasses(code)
    assertEquals(List(("MODULE$", null), ("key", null)),
      cM.fields.asScala.toList.map(f => (f.name, f.signature)).sorted)
  }
}
