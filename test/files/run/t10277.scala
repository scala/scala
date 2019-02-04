trait Column {}

trait TypedColumn[@specialized(Long, Double) T] extends Column {
  def put(idx: Int, value: T): Unit

  def fillRange(start: Int, len: Int, value: T): Unit = {
    var idx = start
    val end = start + len
    while (idx < end) {
      put(idx, value)
      idx += 1
    }
  }
}

final class LongColumn extends TypedColumn[Long] {
  override def put(idx: Int, value: Long): Unit = {
    val frames = Thread.currentThread().getStackTrace.toList.drop(1).takeWhile(_.getMethodName != "main")
    println(frames.map(_.getMethodName).mkString("\n"))
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new LongColumn
    c.fillRange(0, 1, 10L)
  }
}
