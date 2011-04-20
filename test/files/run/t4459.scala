import collection._

object Test {
  def main(args: Array[String]) {
    for (i <- 0 until 2000) {
      foo((0 until 10000).toSeq.par)
    }
  }

  def foo(arg: GenSeq[_]): String = arg.map(x => x).mkString(",")
}

