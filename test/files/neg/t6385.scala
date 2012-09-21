object N {
   def main(args: Array[String]) {
      val y: AA[Int] = C(2)
      val c: Int = y.x.y
      println(c)
   }
}
trait AA[T] extends Any {
   def x: C[T]
}
case class C[T](val y: T) extends AnyVal with AA[T] {
   def x = this
}
