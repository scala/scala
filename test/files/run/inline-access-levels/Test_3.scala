import test._

object Test {

  def main(args: Array[String]) {

    B.actOnX(_ + 1)

    val a = new B

    a.actOnX(_ + 1)

  }
}
