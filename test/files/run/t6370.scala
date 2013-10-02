object Test {

  def main(args: Array[String]): Unit = {
      val m = collection.immutable.ListMap( "x" -> 1 )
      try {
         m("y")
      } catch {
         case e : NoSuchElementException => assert(e.getMessage() == "key not found: y")
      }

  }
}
