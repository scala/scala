package p {
  object T {
    def run(): Unit = Macros.tree[A].foreach(println)
  }
}

object Test {
  def main(args: Array[String]): Unit = p.T.run()
}
