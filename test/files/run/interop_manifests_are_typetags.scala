object Test extends App {
  def manifestIsTypeTag[T: Manifest] = {
    println(typeTag[T].tpe)
    println(typeTag[T].erasure)
  }

  manifestIsTypeTag[Int]
  manifestIsTypeTag[String]
  manifestIsTypeTag[Array[Int]]
}