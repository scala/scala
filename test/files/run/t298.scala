object Test extends App {
  implicit def anyList[T]: List[T] = Nil

  implicit def intList: List[Int] = 42::24::Nil

  def foo[T](implicit x: T) = x

  val s = foo[List[Int]]

  println(s)  // correct - prints "List(42, 24)"

  implicit def tupleList[T](implicit t: List[T]): List[(T,T)] = t.map(x => (x,x))

  val t = foo[List[Tuple2[Int,Int]]]

  println(t)  // incorrect - prints "List()"
}
