import scala.tools.nsc.interpreter._
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  class M[@specialized T] { }

  def code = """
    |class M[@specialized T] { override def toString = "mmm" }
    |val m1 = new M[Int]()
    |val m2 = new M[Float]()
    |val m3 = new M[String]()
    |""".stripMargin
}
