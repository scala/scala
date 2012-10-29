package test

object Test {

  def main(args: Array[String]) {

    A.actOnX(_ + 1)

    val a = new A

    a.actOnX(_ + 1)

  }
}
