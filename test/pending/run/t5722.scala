object Test extends App {
  def foo[T: ClassTag] = println(classOf[T])
  foo[Int]
  foo[Array[Int]]
  foo[List[Int]]
}