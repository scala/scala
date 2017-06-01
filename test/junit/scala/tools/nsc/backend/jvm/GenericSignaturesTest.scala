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
}
