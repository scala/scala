object Test {
   def main(args: Array[String]) {
      val y: AA[Int] = C(2)
      val c: Int = y.x.y
      assert(c == 2)
   }
}
trait AA[T] extends Any {
   def x: C[T]
}
case class C[T](val y: T) extends AnyVal with AA[T] {
   def x = this
}
