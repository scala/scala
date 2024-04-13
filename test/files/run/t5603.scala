import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Vprint:parser -Vprint-pos -Yrangepos -Ystop-after:parser"

  override def code = """
    trait Greeting {
      val name: String
      val msg = "How are you, "+name
    }
    class C(i: Int) extends {
      val nameElse = "Bob"
    } with Greeting {
      val name = "avc"
      println(msg)
    }

    object Test extends App {}
  """.trim

  override def show(): Unit = assert(compile())
}
