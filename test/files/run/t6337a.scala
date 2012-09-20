object Test {
  def main(args: Array[String]) {
    val x = X(XX(3))
    assert(x.q.x.x + 9 == 13)
  }
}
trait Q extends Any {
   def x: Int
   def inc: XX
}
case class X(val x: Q) extends AnyVal {
   def q = X(x.inc)
}
case class XX(val x: Int) extends AnyVal with Q {
   def inc = XX(x + 1)
}
