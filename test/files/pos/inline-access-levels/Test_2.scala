// scalac: -opt:l:inline -opt-inline-from:** -Xfatal-warnings -opt-warnings
package test

object Test {

  def main(args: Array[String]) {

    A.actOnX(_ + 1)

  }

}
