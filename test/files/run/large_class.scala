import scala.tools.partest._
import java.io.{Console => _, _}

// a cold run of partest takes about 15s for this test on my laptop
object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -d " + testOutput.path

  def s(n: Int) = "\""+n+"\""

  override def code
    = s"""
      |class BigEnoughToFail {
      |  def m(a: String, b: String, c: String, d: String, e: String, f: String) = null
      |  ${(1 to 5500) map (n => "def f"+n+" = m("+ s(n+10000)+","+
                                                    s(n+20000)+","+
                                                    s(n+30000)+","+
                                                    s(n+40000)+","+
                                                    s(n+50000)+","+
                                                    s(n+60000)+")") mkString ";"}
      |}""".stripMargin.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
