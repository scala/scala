object Test extends App {
  def manifestIsConcreteTypeTag[T: Manifest] = {
    println(concreteTypeTag[T].tpe)
    println(concreteTypeTag[T].erasure)
  }

  manifestIsConcreteTypeTag[Int]
  manifestIsConcreteTypeTag[String]
  manifestIsConcreteTypeTag[Array[Int]]
}