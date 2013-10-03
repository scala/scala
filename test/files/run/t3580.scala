




object Test {

  class Empty extends Traversable[Nothing] {
    def foreach[U](f: Nothing => U) {}
  }

  def main(args: Array[String]) {
    val t = new Empty
    t.toStream
  }

}
