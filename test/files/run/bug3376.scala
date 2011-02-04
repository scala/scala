import scala.tools.nsc.interpreter._

object Test {
  class M[@specialized T] { }

  val code = """
    |class M[@specialized T] { override def toString = "mmm" }
    |val m1 = new M[Int]()
    |val m2 = new M[Float]()
    |val m3 = new M[String]()
    |""".stripMargin

  def main(args: Array[String]): Unit = {
    (ILoop run code).lines drop 1 foreach println
  }
}
