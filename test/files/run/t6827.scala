object Test extends App {
  val ns = (0 until 20)
  val arr = new Array[Int](10)

  def tryit(label: String, start: Int, len: Int): Unit = {
    val status = try {
      val it = ns.toIterator
      it.copyToArray(arr, start, len)
      "ok"
    } catch {
      case e: Exception => e.toString
    }
    println("%s: %s" format (label, status))
  }

  tryit("start at -5", -5, 10)
  tryit("start at -1", -1, 10)
  tryit("start at limit", 10, 10)
  tryit("start at limit-1", 9, 10)
  tryit("first 10", 0, 10)
  tryit("read all", 0, 20)
  tryit("test huge len", 0, Int.MaxValue)
  tryit("5 from 5", 5, 10)
  tryit("20 from 5", 5, 20)
  tryit("test len overflow", 5, Int.MaxValue)
  tryit("start beyond limit", 30, 10)
  tryit("read 0", 0, 0)
  tryit("read -1", 0, -1)
  tryit("invalid read 0", 30, 0)
  tryit("invalid read -1", 30, -1)
}
