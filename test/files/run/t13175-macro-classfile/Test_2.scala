//> using filter jdk\.internal\.
// Java classes compiled by javac in the first round, here they are read from classfiles
package p {
  object T {
    def run(): Unit = {
      Macros.tree[Shape].foreach(println)
      Macros.tree[java.lang.constant.ConstantDesc].foreach(println)
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = p.T.run()
}
