case class C[T](x: T) 

case class CS(xs: C[_]*)

object Test {
  val x = CS(C(5), C("abc")) match { case CS(C(5), xs @ _*) => xs }
  println(x)
}
