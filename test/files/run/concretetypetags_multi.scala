object Test extends App {
  println(concreteTypeTag[Int])
  println(concreteTypeTag[Array[Int]])
  println(concreteTypeTag[Array[Array[Int]]])
  println(concreteTypeTag[Array[Array[Array[Int]]]])
  println(concreteTypeTag[Array[Array[Array[Array[Int]]]]])
}