class ann(a: Array[Int]) extends annotation.StaticAnnotation

object Test1 {
  // bug #1029
  @ann(Array(10, 2)) def u = ()
  val v: String @ann(Array(13, 2)) = "-1"
}
