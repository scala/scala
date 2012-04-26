object Test extends App {
  def foo1[T] = erasureTag[T]
  println(foo1[Int].erasure)
  println(foo1[String].erasure)
  println(foo1[Array[Int]].erasure)

  def foo2[T <: Int] = erasureTag[T]
  println(foo2[Int].erasure)
}