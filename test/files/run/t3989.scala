




class Foo{ override def equals(o: Any) = false; override def hashCode = 1}

// should not result in a stack overflow
object Test {
  def main(args: Array[String]) {
    import collection.immutable.HashMap
    var m = Map[Foo, Int]()
    for (i <- 1 to 30000) m += (new Foo) -> i
    assert(m.size == 30000)
    m.toString
  }
}
