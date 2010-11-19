



object Test {
  def main(args: Array[String]) {
    val q = scala.collection.mutable.Queue[String]()
    assert(q.length == 0)
    try {
      assert(q.front != null)
    } catch {
      case _ =>
    }
  }
}
