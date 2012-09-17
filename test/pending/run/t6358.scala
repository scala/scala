object Test {
   def main(args: Array[String]) {
      val y0: AA[Int] = new C(2)
      val y1 = new C(2)
      val c0: Int = y0.x.y
      val c1: Int = y1.x.y
      println(c0)
      println(c1)
   }
}
trait AA[T] extends Any {
   def x: C[T]
}
class C[T](val y: T) extends AnyVal with AA[T] {
   def x = this
}
