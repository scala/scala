object Test extends App {
  def test[T: ErasureTag] = {
    println(implicitly[ErasureTag[T]].erasure)
    println(implicitly[ErasureTag[Array[T]]].erasure)
  }

  test[Int]
  test[List[Int]]
  test[List[String]]
  test[Map[Int, String]]

  test[Array[Int]]
  test[Array[List[Int]]]
  test[Array[List[String]]]
  test[Array[Map[Int, String]]]

  test[Array[Array[Int]]]
  test[Array[Array[List[Int]]]]
  test[Array[Array[List[String]]]]
  test[Array[Array[Map[Int, String]]]]
}