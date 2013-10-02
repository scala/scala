


import collection._



object Test {

  def main(args: Array[String]) {
    val gen: GenTraversable[Int] = List(1, 2, 3)
    gen.head
    gen.headOption
    gen.tail
    gen.last
    gen.lastOption
    gen.init
  }

}
