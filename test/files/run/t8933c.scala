object Test {
  def main(args: Array[String]): Unit = {
    try {    
      {throw T; Symbol}.apply("a")
      assert(false, "exception not thrown")
    } catch {
      case T => // ok
      case t: Throwable =>
        assert(false, "wrong not thrown: " + t)
    }
  }
}

object T extends Throwable
