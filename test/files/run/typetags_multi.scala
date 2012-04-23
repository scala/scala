object Test extends App {
  println(typeTag[Int])
  println(typeTag[Array[Int]])
  println(typeTag[Array[Array[Int]]])
  println(typeTag[Array[Array[Array[Int]]]])
  println(typeTag[Array[Array[Array[Array[Int]]]]])
}