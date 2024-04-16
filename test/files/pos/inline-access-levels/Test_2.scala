//> using options -opt:inline:** -Wopt -Werror
package test

object Test {

  def main(args: Array[String]): Unit = {
    A.actOnX(_ + 1)
    B.actOnX(_ + 1)
  }

}
