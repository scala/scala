object Test extends App {
  def test[T: ArrayTag] = {
    println(implicitly[ArrayTag[T]].newArray(10).getClass)
    println(implicitly[ArrayTag[T]].wrap.newArray(10).getClass)
    println(implicitly[ArrayTag[Array[T]]].wrap.newArray(10).getClass)
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