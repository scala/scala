class Interval[@specialized T](val high: T)
class Node[@specialized T](val interval: Interval[T]) {
  val x1 = Some(interval.high)
}

object Test {
  def main(args: Array[String]): Unit = {
    new Node(new Interval(5)).x1
  }
}