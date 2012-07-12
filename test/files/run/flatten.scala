import scala.tools.partest._

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:flatten -d " + testOutput.path

  override def code =
    """
      |object O {
      |  new {
      |    () => 0
      |    new {
      |    }
      |  }
      |  class C {
      |    new T1 with T2 {}
      |    class D {
      |      def method {
      |        () => "anon fun in method".isEmpty
      |        new { "anon class in method".isEmpty }
      |      }
      |    }
      |  }
      |}
      |
      |trait T1; trait T2
      |
      |trait T3 {
      |  () => { new { } }
      |}
    """.stripMargin

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
