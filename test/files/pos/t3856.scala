case class C[T](x: T)

case class CS(xs: C[_]*)

// t3856
object Test {
  val x = CS(C(5), C("abc")) match { case CS(C(5), xs @ _*) => xs }
  println(x)
}
