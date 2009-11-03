object Test {
  def main(args: Array[String]) {
    val m = new collection.mutable.HashMap[String, String] {
      override def initialSize = 0
    }
    m.toString
    m("key") = "value"
    assert(m("key") == "value")
  }
}
